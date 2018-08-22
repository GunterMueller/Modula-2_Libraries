IMPLEMENTATION MODULE TimeOfDay;

	(****************************************************************)
	(*								*)
	(*			Time-of-day Module			*)
	(*								*)
	(*  This module keeps a time-of-day record, to the nearest 16th	*)
	(*  of a second, by using the CMOS clock hardware.		*)
	(*								*)
	(*  Programmer:		P. Moylan				*)
	(*  Last edited:	22 March 1995				*)
	(*  Status:		Working					*)
	(*								*)
	(****************************************************************)

FROM MiscPMOS IMPORT
    (* proc *)	ReadCMOS, WriteCMOS, EnterCriticalSection, LeaveCriticalSection;

FROM LowLevel IMPORT
    (* proc *)	IORB, IAND, IANDB, LS, RS, OutByte, InByte;

FROM Windows IMPORT
    (* type *)	Window, Colour, FrameType, DividerType,
    (* proc *)	OpenWindow, WriteString, WriteLn;

FROM NumericIO IMPORT
    (* proc *)	WriteHexByte;

FROM TaskControl IMPORT
    (* proc *)	CreateInterruptTask, WaitForInterrupt;

FROM TerminationControl IMPORT
    (* proc *)	SetTerminationProcedure;

(************************************************************************)
(*									*)
(*  The first 14 bytes of CMOS are used for the real-time clock:	*)
(*									*)
(*	00	seconds			01	second alarm		*)
(*	02	minutes			03	minute alarm		*)
(*	04	hours			05	hour alarm		*)
(*	06	day of week		07	date of month		*)
(*	08	month			09	year			*)
(*	0A	status register A	0B	status register B	*)
(*	0C	status register C	0D	status register D	*)
(*									*)
(*  Byte number 50 (decimal) holds the century.  This is maintained	*)
(*  by software, not hardware.						*)
(*									*)
(*  Times and dates are normally stored in BCD format, although there	*)
(*  is an option to use binary.  The four status registers are for	*)
(*  various options and flags; for the details, the Technical Reference	*)
(*  Manual should be consulted.						*)
(*									*)
(************************************************************************)

CONST
    InterruptNo = 70H;		(* hardware interrupt number *)
    StatusA = 10;		(* status register A	*)
    StatusB = 11;		(* status register B	*)
    StatusC = 12;		(* status register C	*)

VAR
    MasterTime: TimeRecord;

    (* BinaryClock keeps track of whether we are currently running in	*)
    (* binary mode.  (If FALSE, we are running in BCD mode.)		*)

    BinaryClock: BOOLEAN;

    (* WrapPending should logically be a local static variable of the	*)
    (* interrupt handler, but Modula-2 has no local static variables.	*)

    WrapPending: BOOLEAN;

(************************************************************************)
(*			    THE INTERRUPT TASK				*)
(************************************************************************)

PROCEDURE InterruptHandler;

    CONST UF = 010H;	(* update-ended flag in Status Register C *)
	testing = FALSE;

    VAR flags: BYTE;
	w: Window;

    BEGIN
	IF testing THEN
	    OpenWindow (w, blue, cyan, 16, 24, 0, 79, simpleframe, nodivider);
	END (*IF*);

	LOOP (*FOREVER*)
	    WaitForInterrupt;
	    flags := ReadCMOS (StatusC);

	    IF testing THEN
		WriteHexByte (w, flags);  WriteString (w, "  ");
	    END (*IF*);

	    IF ORD(IANDB (flags, UF)) <> 0 THEN
		WITH MasterTime DO
		    ticks := 0;
		    seconds := ReadCMOS (0);
		    minutes := ReadCMOS (2);
		    hours := ReadCMOS (4);
		    dayofweek := ReadCMOS (6);
		    dayofmonth := ReadCMOS (7);
		    month := ReadCMOS (8);
		    year := ReadCMOS (9);
		    century := ReadCMOS (50);
		    IF ((year=99) AND BinaryClock)
		    		OR ((year=99H) AND NOT BinaryClock) THEN
			WrapPending := TRUE;
		    ELSIF WrapPending AND (year = 0) THEN
			WrapPending := FALSE;
			INC (century);
			IF NOT BinaryClock
				AND (ORD(IANDB(century,0FH)) > 9) THEN
			    INC (century, 6);
			END (*IF*);
			WriteCMOS (50, century);
		    END (*IF*);
		END (*WITH*);

		IF testing THEN WriteLn(w) END(*IF*);

	    ELSE
		INC(MasterTime.ticks);
	    END (*IF*);

	END (*LOOP*);
    END InterruptHandler;

(************************************************************************)
(*			READING AND SETTING THE CLOCK			*)
(************************************************************************)

PROCEDURE ReadClock (VAR (*OUT*) result: TimeRecord);

    (* Returns the current time and date.	*)

    BEGIN

	(* Take a copy of the master time record, disabling clock	*)
	(* interrupts via the slave interrupt mask register.		*)

	OutByte (0A1H, IORB(InByte(0A1H), 1));
	result := MasterTime;
	OutByte (0A1H, IANDB(InByte(0A1H), 0FEH));
    END ReadClock;

(************************************************************************)

PROCEDURE SetClock (VAR (*IN*) newtime: TimeRecord);

    (* Modifies the current time and date.  Note that newtime.ticks is	*)
    (* in effect ignored.  To take that into account, and thereby	*)
    (* achieve greater accuracy, we would need to use the more complex	*)
    (* approach of delaying for a few ticks before resetting.		*)

    VAR PSW: CARDINAL;

    BEGIN
	(* Disable clock interrupts by setting a bit in the mask	*)
	(* register of the slave interrupt controller.  Also prevent	*)
	(* any clock updates by setting the high-order bit of Status	*)
	(* Register B.  For extra safety (just in case other tasks are	*)
	(* accessing CMOS), disable processor interrupts as well.	*)

	PSW := EnterCriticalSection();
	OutByte (0A1H, IORB(InByte(0A1H), 1));
	WriteCMOS (StatusB, IORB(ReadCMOS(StatusB), 80H));

	MasterTime := newtime;
	WITH newtime DO
	    WriteCMOS (0, seconds);
	    WriteCMOS (2, minutes);
	    WriteCMOS (4, hours);
	    WriteCMOS (6, dayofweek);
	    WriteCMOS (7, dayofmonth);
	    WriteCMOS (8, month);
	    WriteCMOS (9, year);
	    WriteCMOS (50, century);
	END (*WITH*);

	(* Re-enable the clock.	*)

	OutByte (0A1H, IANDB(InByte(0A1H), 0FEH));
	WriteCMOS (StatusB, IANDB(ReadCMOS(StatusB), 7FH));
	LeaveCriticalSection (PSW);

    END SetClock;

(************************************************************************)
(*		     SETTING CLOCK MODE TO BINARY OR BCD		*)
(************************************************************************)

PROCEDURE Convert (j: CARDINAL;  SetToBinary: BOOLEAN);

    (* Converts the value in CMOS register j from BCD to binary if	*)
    (* SetToBinary is TRUE, and from binary to BCD otherwise.		*)

    VAR value: CARDINAL;

    BEGIN
	value := ORD(ReadCMOS(j));
	IF SetToBinary THEN
	    value := 10*ORD(RS(value, 4)) + ORD(IAND(value, 0FH));
	ELSE
	    value := ORD(LS(value DIV 10, 4)) + (value MOD 10);
	END (*IF*);
	WriteCMOS (j, BYTE(value));
    END Convert;

(************************************************************************)

PROCEDURE SetBinaryMode (NewMode: BOOLEAN);

    (* Makes the clock run in BCD if NewMode is FALSE, or in binary if	*)
    (* NewMode is TRUE.  If this requires a mode change, adjusts the	*)
    (* current date/time values held by the hardware.			*)

    VAR j: CARDINAL;  StatusBvalue: BYTE;

    BEGIN
	StatusBvalue := ReadCMOS (StatusB);
	IF (BinaryClock <> NewMode) THEN

	    (* Modify our copy of the date mode bit. *)

	    StatusBvalue := IANDB (StatusBvalue, 07BH);
	    IF NewMode THEN
		INC (StatusBvalue, 4);
	    END (*IF*);

	    (* Disable clock interrupts and clock updating.  See	*)
	    (* procedure SetClock for explanation.			*)

	    OutByte (0A1H, IORB (InByte (0A1H), 1));
	    WriteCMOS (StatusB, IORB (StatusBvalue, 080H));

	    (* Modify all the time and date values. *)

	    FOR j := 0 TO 9 DO
		Convert(j, NewMode);
	    END (*FOR*);
	    Convert (50, NewMode);	(* Don't forget the century *)

	    (* Re-enable the clock in the new mode.	*)

	    BinaryClock := NewMode;
	    WriteCMOS (StatusB, StatusBvalue);
	    OutByte (0A1H, IANDB(InByte(0A1H), 0FEH));

	END (*IF*);
    END SetBinaryMode;

(************************************************************************)
(*			PROGRAM TERMINATION HANDLER			*)
(************************************************************************)

PROCEDURE Cleanup;

    (* Disables CMOS interrupts, and sets the CMOS clock back to its	*)
    (* standard MS-DOS setup condition.					*)

    VAR PSW: CARDINAL;

    BEGIN
	PSW := EnterCriticalSection();
	SetBinaryMode (FALSE);
	WriteCMOS (StatusA, 26H);
	WriteCMOS (StatusB, 2);
	LeaveCriticalSection (PSW);
    END Cleanup;

(************************************************************************)
(*				INITIALISATION				*)
(************************************************************************)

PROCEDURE HardwareSetup;

    CONST
	PIenable = 40H;		(* periodic interrupt enable	*)
	UIenable = 10H;		(* update-ended interrupt enable *)
	mode24 = 2;		(* 24-hour mode *)

    VAR PSW: CARDINAL;

    BEGIN
	PSW := EnterCriticalSection();
	BinaryClock := ORD(IANDB(ReadCMOS (StatusB), 4)) <> 0;
	SetBinaryMode (FALSE);

	(* Select a periodic clock rate of 16 Hz.	*)

	WriteCMOS (StatusA, 2CH);

	(* Set the desired interrupt options in status register B, and	*)
	(* install the interrupt handler.				*)

	WriteCMOS (StatusB, PIenable+mode24);
	CreateInterruptTask (InterruptNo, InterruptHandler, "CMOS interrupt");

	(* Clear a bit in the interrupt mask register.  The mask	*)
	(* register address is 21H for the master interrupt controller,	*)
	(* 0A1H for the slave.						*)

	OutByte (0A1H, IANDB(InByte(0A1H), 0FEH));
	OutByte (021H, IANDB(InByte(021H), 0FBH));
	LeaveCriticalSection (PSW);

    END HardwareSetup;

(************************************************************************)

BEGIN
    WrapPending := FALSE;
    SetTerminationProcedure (Cleanup);
    HardwareSetup;
END TimeOfDay.
