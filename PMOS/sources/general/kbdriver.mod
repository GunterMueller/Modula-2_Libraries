IMPLEMENTATION MODULE KBdriver;

	(****************************************************************)
	(*								*)
	(*		Device driver for the keyboard.			*)
	(*								*)
	(*	Author:		P. Moylan				*)
	(*	Last edited:	17 March 1995				*)
	(*	Status:		OK					*)
	(*								*)
	(*	The "Beep" call has been removed because of a problem:	*)
	(*	semaphore wait inside an interrupt task.  For the	*)
	(*	moment I have a temporary substitute solution, but it's	*)
	(*	not really satisfactory; the issue of lost codes needs	*)
	(*	re-thinking.						*)
	(*								*)
	(****************************************************************)

(************************************************************************)
(*									*)
(* Scan codes produced by the keyboard are filtered and modified by the	*)
(* keyboard controller; the result appears at port 60H and cause a type	*)
(* 9 interrupt.  The keyboard controller is a semi-intelligent device,	*)
(* and the processor can send commands to it by writing to port 60H or	*)
(* port 64H.  (The distinction is that port 64H is for commands to the	*)
(* keyboard controller, and port 60H is for commands to the keyboard	*)
(* itself.)  After a lot of trial and (mostly) error, I have reached	*)
(* the conclusion that this "intelligence" is more of a hindrance than	*)
(* a help, so the only command sent by this module is the ED (hex)	*)
(* command which turns the LED indicators on and off.  The second byte	*)
(* of this command specifies which LEDs to light:			*)
(*		bit 0		scroll lock				*)
(*		bit 1		num lock				*)
(*		bit 2		caps lock				*)
(*		bits 3-7	must be 0				*)
(* On input from port 64H, bit 1 is 0 when the controller is ready to	*)
(* accept a new command byte.  Bit 0 goes to 1 each time a new data	*)
(* byte is available, but we don't need to check that bit explicitly	*)
(* because it causes an interrupt.  The other bits of that port are	*)
(* status flags which I do not bother to use.				*)
(*									*)
(************************************************************************)

FROM MiscPMOS IMPORT
    (* proc *)	EnterCriticalSection, LeaveCriticalSection;

FROM LowLevel IMPORT
    (* proc *)	IANDB, IXORB, INOTB, InByte, OutByte;

FROM TaskControl IMPORT
    (* type *)	Lock,
    (* proc *)	WaitForInterrupt, CreateTask, CreateInterruptTask,
		KillInterruptTask,
		NotUsingFloatingPoint, CreateLock, Obtain, Release;

FROM Semaphores IMPORT
    (* type *)	Semaphore,
    (* proc *)	CreateSemaphore, Wait, Signal;

FROM TerminationControl IMPORT
    (* proc *)	SetTerminationProcedure;

(************************************************************************)

CONST
    InterruptNumber = 9;
    KeyboardDataPort = 60H;
    KeyboardOutputPort = 60H;
    StatusRegister = 64H;
    CommandPort = 64H;
    MaxBufferSubscript = 15;

TYPE
    BufferSubscript = [0..MaxBufferSubscript];

VAR

    (* CodeBuffer is a circular buffer holding scan codes.  The		*)
    (* DataCounter semaphore is a counting semaphore which keeps track	*)
    (* of how many scan codes are at present in the buffer.  GetPlace	*)
    (* points to the next scan code which will be retrieved by the	*)
    (* consumer.  PutPlace is the place where the next keyboard input	*)
    (* will be put.  The condition GetPlace=PutPlace could mean either	*)
    (* that the buffer is completely empty or that it is completely	*)
    (* full.  The flag BufferFull is used to distinguish between these	*)
    (* two cases.							*)

    CodeBuffer:	RECORD
		    BufferFull, OverFlow: BOOLEAN;
		    DataCounter: Semaphore;
		    GetPlace, PutPlace: BufferSubscript;
		    data: ARRAY BufferSubscript OF BYTE
		END;

    (* There are three lights on the keyboard which show the current	*)
    (* state of Caps Lock, Num Lock, and Shift Lock.  The variable	*)
    (* CurrentLEDstatus keeps track of which of these is currently lit.	*)
    (* The encoding is defined by the constants declared in the		*)
    (* the definition module for this module.				*)

    CurrentLEDstatus: BYTE;

    (* We perform a Signal(LEDstatusUpdate) whenever the value of	*)
    (* CurrentLEDstatus changes.					*)

    LEDstatusUpdate: Semaphore;

    (* A Lock to make the sending of a multibyte command to the		*)
    (* keyboard an indivisible operation.				*)

    CommandLock: Lock;

(************************************************************************)
(*			COMMANDS TO THE KEYBOARD			*)
(************************************************************************)

PROCEDURE WaitForKeyboardReady;

    (* Pauses, using a busy wait, until the keyboard status byte says	*)
    (* that the keyboard is ready to receive a command or until we lose	*)
    (* our patience.  This use of a busy wait is admittedly bad		*)
    (* software design, but I couldn't decipher the keyboard controller	*)
    (* documentation well enough to see where there is a way to do this	*)
    (* job using interrupts.  In theory, the keyboard always responds	*)
    (* to a command within 20 milliseconds.  The response to a command	*)
    (* should be an ACK code (0FAH), but it hardly seems worth checking	*)
    (* for that since it comes interleaved with any character data	*)
    (* which might be arriving.  It's simpler just to discard any ACK	*)
    (* codes which arrive with the data.  Of course this means that I	*)
    (* lose the opportunity to check for some transmission errors, but	*)
    (* the keyboard interface is so hard to work with that some		*)
    (* shortcuts seem to be justified.					*)

    CONST patience = 32767;

    VAR count: CARDINAL;

    BEGIN
	count := patience;
	REPEAT
	    DEC (count);
	UNTIL (count = 0) OR (ORD(IANDB(InByte (StatusRegister), 2)) = 0);
    END WaitForKeyboardReady;

(************************************************************************)

PROCEDURE LEDdriver;

    (* Runs as a separate task, which updates the keyboard lock		*)
    (* indicator lights every time it gets a Signal(LEDstatusUpdate).	*)
    (* Using a separate task for this job may seem a little silly, but	*)
    (* I couldn't think of a better way to deal with the fact that the	*)
    (* keyboard doesn't always correctly receive commands sent to it	*)
    (* and is inclined to ask for retransmissions.  Anyway, this	*)
    (* module seems to control the LEDs far more reliably than the BIOS	*)
    (* does, so it can't be all that bad a method.			*)

    BEGIN
	NotUsingFloatingPoint;
	LOOP (*forever*)
	    Wait (LEDstatusUpdate);
	    Obtain (CommandLock);
	    WaitForKeyboardReady;
	    OutByte (KeyboardOutputPort, 0EDH);
	    WaitForKeyboardReady;
	    OutByte (KeyboardOutputPort, CurrentLEDstatus);
	    Release (CommandLock);
	END (*LOOP*);
    END LEDdriver;

(************************************************************************)

PROCEDURE PutLEDs (LEDcode: BYTE);

    (* Sets the keyboard lock indicator lights, as specified by		*)
    (* LEDcode.  Unlike the following two procedures, which can affect	*)
    (* one LED without disturbing the others, this procedure alters all	*)
    (* three LEDs as a group.						*)

    BEGIN
	CurrentLEDstatus := LEDcode;
	Signal (LEDstatusUpdate);
    END PutLEDs;

(************************************************************************)

PROCEDURE ClearLED (LEDcode: BYTE);

    (* Clears one or more of the keyboard lock indicator lights.	*)

    BEGIN
	CurrentLEDstatus := IANDB (CurrentLEDstatus, INOTB(LEDcode));
	Signal (LEDstatusUpdate);
    END ClearLED;

(************************************************************************)

PROCEDURE ToggleLED (LEDcode: BYTE);

    (* Toggles one or more of the keyboard lock indicator lights.	*)

    BEGIN
	CurrentLEDstatus := IXORB (CurrentLEDstatus, LEDcode);
	Signal (LEDstatusUpdate);
    END ToggleLED;

(************************************************************************)
(*			  THE INTERRUPT TASK				*)
(************************************************************************)

PROCEDURE KeyboardInterruptTask;

    (* This is the interrupt task which responds to interrupts from the	*)
    (* keyboard.  Each time an interrupt occurs, we deposit the		*)
    (* keyboard scan code in the circular buffer CodeBuffer, and	*)
    (* perform a semaphore Signal to indicate that a new scan code is	*)
    (* available.  Special case: if there is no room in the buffer, we	*)
    (* insert a special "LostCode" byte at the next available		*)
    (* opportunity.  The LostCode code represents one or more lost	*)
    (* scan codes.  Most device drivers should not be designed to lose	*)
    (* data like this, but the keyboard is a special case: the error	*)
    (* indication to the caller seems to be more useful than simply	*)
    (* locking the keyboard.						*)

    CONST RetransmitCode = BYTE(0FEH);  ACKcode = BYTE(0FAH);

    VAR inputdatum: BYTE;  overflow: BOOLEAN;

    BEGIN
	overflow := FALSE;
	LOOP (*FOREVER*)
	    WaitForInterrupt;
	    inputdatum := InByte (KeyboardDataPort);

	    IF inputdatum = ACKcode THEN
		(* do nothing - ignore ACK codes *)

	    (* Check for retransmission request. *)

	    ELSIF inputdatum = RetransmitCode THEN
		Signal (LEDstatusUpdate);

	    ELSE
		WITH CodeBuffer DO
		    IF BufferFull THEN OverFlow := TRUE;
		    ELSE
			data[PutPlace] := inputdatum;
			IF PutPlace = MaxBufferSubscript THEN PutPlace := 0
			ELSE INC (PutPlace);
			END (*IF*);
			BufferFull := PutPlace = GetPlace;
			Signal (DataCounter);
		    END (*IF*);
		END (*WITH*);
	    END (*IF*);

	END (*LOOP*);

    END KeyboardInterruptTask;

(************************************************************************)
(*			ENTRY POINTS FOR THE USER			*)
(************************************************************************)

PROCEDURE GetScanCode () : BYTE;

    (* Gets one scan code from the keyboard input buffer.	*)

    VAR result: BYTE;  savedPSW: CARDINAL;

    BEGIN
	WITH CodeBuffer DO
	    Wait (DataCounter);
	    IF OverFlow THEN
		result := 0FFH;  OverFlow := FALSE;
		Signal (DataCounter);
	    ELSE
		result := data[GetPlace];
		savedPSW := EnterCriticalSection();
		IF GetPlace = MaxBufferSubscript THEN GetPlace := 0
		ELSE INC (GetPlace);
		END (*IF*);
		BufferFull := FALSE;
		LeaveCriticalSection (savedPSW);
	    END (*IF*);
	END (*WITH*);
	RETURN result;
    END GetScanCode;

(************************************************************************)

PROCEDURE CheckScanCode () : BYTE;

    (* Like GetScanCode, but returns 0 immediately if no scan code is	*)
    (* available - i.e. does not wait for a keyboard press.		*)

    VAR empty: BOOLEAN;  savedPSW: CARDINAL;

    BEGIN
	savedPSW := EnterCriticalSection();
	WITH CodeBuffer DO
	    empty := (GetPlace = PutPlace) AND NOT BufferFull;
	END (*WITH*);
	LeaveCriticalSection (savedPSW);
	IF empty THEN RETURN 0
	ELSE RETURN GetScanCode();
	END (*IF*);
    END CheckScanCode;

(************************************************************************)
(*			   SHUTDOWN PROCESSING				*)
(************************************************************************)

PROCEDURE ShutDownPhase2;

    (* Removes the interrupt handler. *)

    BEGIN
	KillInterruptTask (InterruptNumber);
    END ShutDownPhase2;

(************************************************************************)

PROCEDURE ShutDown;

    (* The main function of this procedure is to ensure that we don't	*)
    (* close down while in the middle of sending a command to the	*)
    (* keyboard.							*)

    BEGIN
	Obtain (CommandLock);
	(* SetTerminationProcedure (ShutDownPhase2); *)
    END ShutDown;

(************************************************************************)

BEGIN

    (* Initialize the scan code buffer.	*)

    WITH CodeBuffer DO
	BufferFull := FALSE;  OverFlow := FALSE;
	CreateSemaphore (DataCounter, 0);
	GetPlace := 0;  PutPlace := 0;
    END (*WITH*);

    (* Install the keyboard interrupt routine.	*)

    CreateInterruptTask (InterruptNumber, KeyboardInterruptTask, "Kbd interrupt");

    (* Install the LED driver. *)

    SetTerminationProcedure (ShutDown);
    CreateLock (CommandLock);
    CreateSemaphore (LEDstatusUpdate, 0);
    CreateTask (LEDdriver, 8, "Kbd LED driver");
    CurrentLEDstatus := 0;  Signal (LEDstatusUpdate);

END KBdriver.
