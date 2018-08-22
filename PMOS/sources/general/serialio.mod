IMPLEMENTATION MODULE SerialIO;

	(********************************************************)
	(*							*)
	(*	     Serial I/O through the COM ports		*)
	(*							*)
	(*	This module supports the NS16450 or INS8250	*)
	(*	serial interfaces on COM1 and COM2.  Support	*)
	(*	for COM3 and COM4 has not yet been implemented.	*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	16 March 1995			*)
	(*  Status:		Working, needs more testing.	*)
	(*	Faults:						*)
	(*	 1. Can't get loopback mode to work, although	*)
	(*	    it did work in the non-interrupt version.	*)
	(*	 2. High-speed input seems to crash system.	*)
	(*	 3. Spurious input on channel 1 - this goes	*)
	(*	    away when I unplug my modem, so might not	*)
	(*	    be a problem with this module.		*)
	(*	Other shortcomings:				*)
	(*	 1. Receive overrun simply ignored.		*)
	(*	 2. No ^S/^Q controls.				*)
	(*	 3. No support for advanced features.		*)
	(*							*)
	(********************************************************)

FROM LowLevel IMPORT
    (* proc *)	InByte, OutByte, LowByte, HighByte, IANDB, IORB, Div;

FROM MiscPMOS IMPORT
    (* proc *)	EnterCriticalSection, LeaveCriticalSection;

FROM TerminationControl IMPORT
    (* proc *)	SetTerminationProcedure;

FROM TaskControl IMPORT
    (* proc *)	CreateInterruptTask, WaitForInterrupt, KillInterruptTask;

FROM Semaphores IMPORT
    (* type *)	Semaphore,
    (* proc *)	CreateSemaphore, Wait, Signal;

FROM GlassTTY IMPORT	(* for testing *)
    (* proc *)	WriteChar, WriteHexByte;

(************************************************************************)

TYPE PortNumbers = ARRAY SerialChannelNumber OF CARDINAL;
     PortMasks = ARRAY SerialChannelNumber OF BYTE;
     ParityCode = ARRAY Parity OF SHORTCARD;

CONST
    (* Starting port numbers for the four UARTs.	*)
    (* Still have to look up the details for COM3,COM4.	*)

    BasePort = PortNumbers (3F8H, 2F8H, 0, 0);

    (* Interrupt numbers for the four UARTs.		*)
    (* Still have to look up the details for COM3,COM4.	*)

    IntNum = PortNumbers (12, 11, 0, 0);
    IntNumMask = PortMasks (0EFH, 0F7H, 0FFH, 0FFH);

    (* Port numbers within a UART, expressed as offsets from BasePort.	*)

    TXD = 0;		(* transmitter data register	*)
    RXD = 0;		(* receiver data register	*)
    IER = 1;		(* interrupt enable register	*)
    IIR = 2;		(* interrupt identification reg	*)
    LCR = 3;		(* line control register	*)
    MCR = 4;		(* modem control register	*)
    LSR = 5;		(* line status register		*)
    MSR = 6;		(* modem status register	*)

    (* Mapping of parity codes into LCR bit patterns.	*)

    ParitySpec = ParityCode (00H, 08H, 18H, 28H, 38H);

(************************************************************************)

CONST MaxBufferSubscript = 31;

TYPE
    BufferSubscript = [0..MaxBufferSubscript];

    (* A CircBuffer is a circular buffer.  The DataCounter semaphore is	*)
    (* a counting semaphore which keeps track of how many bytes are at	*)
    (* present in the buffer (for an input buffer) or how many spare	*)
    (* slots are still available (for an output buffer).  GetPlace	*)
    (* points to the next byte which will be retrieved by the consumer.	*)
    (* PutPlace is the place where the next datum will be added.	*)
    (* The condition GetPlace=PutPlace could mean either that the	*)
    (* buffer is completely empty or that it is completely full.  The	*)
    (* flag BufferFlag is used to distinguish between these two cases.	*)
    (* The condition BufferFlag=TRUE means buffer full for an input	*)
    (* buffer, or buffer empty for an output buffer.			*)

    CircBuffer =    RECORD
			BufferFlag: BOOLEAN;
			DataCounter: Semaphore;
			GetPlace, PutPlace: BufferSubscript;
			data: ARRAY BufferSubscript OF BYTE
		    END;

VAR
    (* Receiver and transmitter circular buffers. *)

    RXBuffer, TXBuffer: ARRAY SerialChannelNumber OF CircBuffer;

    (* Saved state of the UARTS at startup, so we can restore them later. *)

    SavedRegisters: ARRAY SerialChannelNumber OF ARRAY[1..8] OF BYTE;

(************************************************************************)
(*	    THE INTERRUPT TASKS AND THEIR SUBSIDIARY PROCEDURES		*)
(************************************************************************)

PROCEDURE PutRXBuffer (chan: SerialChannelNumber;  datum: BYTE);

    (* Puts the new datum into the circular buffer used for input from	*)
    (* the given channel.						*)

    BEGIN
	WITH RXBuffer[chan] DO
	    IF BufferFlag THEN
		(*Beep*);		(* no room in buffer *)
	    ELSE
		data[PutPlace] := datum;
		IF PutPlace = MaxBufferSubscript THEN PutPlace := 0
		ELSE INC (PutPlace);
		END (*IF*);
		BufferFlag := PutPlace = GetPlace;
		Signal (DataCounter);
	    END (*IF*);
	END (*WITH*);
    END PutRXBuffer;

(************************************************************************)

PROCEDURE SendFromTXBuffer (chan: SerialChannelNumber);

    (* Sends a new value from the transmitter circular buffer to the	*)
    (* outside world.  Disables "transmitter ready" interrupts if this	*)
    (* empties the circular buffer.					*)

    VAR UART: CARDINAL;

    BEGIN
	UART := BasePort[chan];
	WITH TXBuffer[chan] DO
	    OutByte (UART+TXD, data[GetPlace]);
	    IF GetPlace = MaxBufferSubscript THEN GetPlace := 0
	    ELSE INC (GetPlace);
	    END (*IF*);
	    BufferFlag := PutPlace = GetPlace;
	    IF BufferFlag THEN
		(* Buffer is empty - disable further output. *)
		OutByte (UART+IER, IANDB (InByte(UART+IER), 0DH));
	    END (*IF*);
	    Signal (DataCounter);
	END (*WITH*);
    END SendFromTXBuffer;

(************************************************************************)

PROCEDURE HandleInterrupt (chan: SerialChannelNumber);

    (* This procedure deals with an interrupt from any serial port.	*)
    (* The input parameter identifies the starting port number of the	*)
    (* hardware interface.						*)
    (* Note: because there are multiple sources of interrupts attached	*)
    (* to the same interrupt request, this procedure keeps looping	*)
    (* until all causes of the interrupt have been dealt with.		*)

    VAR condition, dummy: SHORTCARD;
	UART: CARDINAL;

    BEGIN
	UART := BasePort[chan];
	LOOP
	    (* Read the interrupt identification register.	*)

	    condition := InByte (UART+IIR);

	    (* Bit 0 set means no conditions are pending.	*)

	    IF ODD(condition) THEN
		EXIT(*LOOP*)
	    END(*IF*);

	    IF condition = 6 THEN

		(* Code 6 is receiver line status error.  In this	*)
		(* version we do nothing except clear the condition by	*)
		(* reading the Line Status Register.			*)

		dummy := InByte (UART+LSR);

	    ELSIF condition = 4 THEN

		(* Code 4 means received data available.	*)

		PutRXBuffer (chan, InByte (UART+RXD));

	    ELSIF condition = 2 THEN

		(* Code 2 means transmitter ready.	*)

		SendFromTXBuffer (chan);

	    ELSIF condition = 0 THEN

		(* Code 0 is for modem status interrupt.  In this	*)
		(* version we do nothing except clear the condition	*)
		(* by reading the Modem Status Register.		*)

		dummy := InByte (UART+MSR);

	    END (*IF*);

	END (*LOOP*);

    END HandleInterrupt;

(************************************************************************)

PROCEDURE COM1InterruptTask;

    BEGIN
	LOOP
	    WaitForInterrupt;
	    HandleInterrupt (1);
	END (*LOOP*);
    END COM1InterruptTask;

(************************************************************************)

PROCEDURE COM2InterruptTask;

    BEGIN
	LOOP
	    WaitForInterrupt;
	    HandleInterrupt (2);
	END (*LOOP*);
    END COM2InterruptTask;

(************************************************************************)
(*			CHANNEL INITIALISATION				*)
(************************************************************************)

PROCEDURE ModemControl (chan: SerialChannelNumber;  code: BYTE);

    (* Sends the specified code to the Modem Control Register.  The	*)
    (* code bits are: bit0=DTR, bit1=RTS, bit2=Out1, bit3=Out2,		*)
    (* bit4=Loopback.  (The other 3 bits should be zero.)		*)
    (* You don't normally need to call this procedure.			*)

    BEGIN
	OutByte (BasePort[chan]+MCR, code);
    END ModemControl;

(************************************************************************)

PROCEDURE MouseReset (chan: SerialChannelNumber);

    (* Special break sequence needed for resetting a serial mouse.	*)
    (* (But it doesn't seem to do what I want anyway.  Logitech mouse	*)
    (* still fails to act like a Microsoft mouse.)			*)

    VAR LCRcode: BYTE;  i, j: CARDINAL;

    BEGIN
	LCRcode := InByte(BasePort[chan]+LCR);
	OutByte (BasePort[chan]+LCR, IORB(LCRcode, 40H));
	OutByte (BasePort[chan]+MCR, 8);
	FOR i := 0 TO 1000 DO
	    FOR j := 0 TO 1000 DO
	    END (*FOR*);
	END (*FOR*);
	OutByte (BasePort[chan]+LCR, LCRcode);
	OutByte (BasePort[chan]+MCR, 0BH);
    END MouseReset;

(************************************************************************)

PROCEDURE InitSerialChannel (chan: SerialChannelNumber;  baud: CARDINAL;
				wordlength: WordLength;
				parity: Parity; stopbits: StopBits);

    (* Performs initialisation on channel "chan".			*)

    CONST IntMaskRegister = 21H;

    VAR UART: CARDINAL;
	divisor: CARDINAL;
	dummy: CHAR;

    BEGIN
	UART := BasePort[chan];

	(* Clear the line control register.	*)

	OutByte (UART+LCR, 0);

	(* Set the divisor latch access bit.	*)

	OutByte (UART+LCR, 80H);

	(* Set the desired baud rate.  Assuming a 1.8432 MHz crystal,	*)
	(* and an internal division by 16, we need divisor=115200/baud.	*)

	divisor := Div (115200+LONGCARD(baud DIV 2), baud);
	OutByte (UART, LowByte (divisor));
	OutByte (UART+1, HighByte (divisor));

	(* Set desired code for stop bits, data bits, and parity.	*)

	OutByte (UART+LCR, ParitySpec[parity] + 4*(stopbits-1) + wordlength-5);

	(* Set up modem control bits.  In this version we are setting	*)
	(* DTR (bit 0), RTS (bit 1), Out2 (bit 3) and sometimes		*)
	(* loopback (bit 4).  I don't know why I should set Out2, but	*)
	(* I couldn't get the software to work without it.  I haven't	*)
	(* yet managed to get the loopback option to work.		*)

	OutByte (UART+MCR, 0BH);

	(* Clear out spurious characters and error conditions.  We can	*)
	(* clear line and modem status errors by reading the Line	*)
	(* Status Register and the Modem Status Register, respectively.	*)

	dummy := InByte (UART+RXD);
	dummy := InByte (UART+LSR);
	dummy := InByte (UART+MSR);

	(* Enable interrupts for receiver, receiver line status, and	*)
	(* modem status.  We don't yet enable transmitter inputs, since	*)
	(* we have nothing to transmit so far.				*)

	OutByte (UART+IER, 0DH);
	OutByte (IntMaskRegister,
			IANDB (InByte(IntMaskRegister), IntNumMask[chan]));

    END InitSerialChannel;

(************************************************************************)
(*				INPUT					*)
(************************************************************************)

PROCEDURE ReadSerial (chan: SerialChannelNumber;  VAR (*OUT*) value: BYTE);

    (* Reads one byte from a serial channel.	*)

    VAR savedPSW: CARDINAL;

    BEGIN
	WITH RXBuffer[chan] DO
	    Wait (DataCounter);
	    value := data[GetPlace];
	    savedPSW := EnterCriticalSection();	(* while updating BufferFlag *)
	    IF GetPlace = MaxBufferSubscript THEN GetPlace := 0
	    ELSE INC (GetPlace);
	    END (*IF*);
	    BufferFlag := FALSE;
	    LeaveCriticalSection (savedPSW);
	END (*WITH*);
    END ReadSerial;

(************************************************************************)
(*				OUTPUT					*)
(************************************************************************)

PROCEDURE WriteSerial (chan: SerialChannelNumber;  value: BYTE);

    (* Sends one byte to an output channel.	*)

    VAR savedPSW, port: CARDINAL;

    BEGIN
	WITH TXBuffer[chan] DO
	    Wait (DataCounter);
	    data[PutPlace] := value;
	    savedPSW := EnterCriticalSection();	(* while updating BufferFlag *)
	    IF PutPlace = MaxBufferSubscript THEN PutPlace := 0
	    ELSE INC (PutPlace);
	    END (*IF*);
	    BufferFlag := FALSE;

	    (* Enable transmitter interrupts, in case they were disabled. *)

	    port := BasePort[chan] + IER;
	    OutByte (port, IORB(InByte(port), 2));

	    LeaveCriticalSection (savedPSW);
	END (*WITH*);

    END WriteSerial;

(************************************************************************)
(*			   MODULE INITIALISATION			*)
(************************************************************************)

PROCEDURE InitialiseBuffers;

    (* Sets up our global variables, but does not yet set up the	*)
    (* interrupt tasks or enable interrupts.				*)

    VAR chan: SerialChannelNumber;

    BEGIN
	FOR chan := MIN(SerialChannelNumber) TO MAX(SerialChannelNumber) DO
	    WITH RXBuffer[chan] DO
		BufferFlag := FALSE;
		CreateSemaphore (DataCounter, 0);
		GetPlace := MIN(BufferSubscript);
		PutPlace := MIN(BufferSubscript);
	    END (*WITH*);
	    WITH TXBuffer[chan] DO
		BufferFlag := TRUE;
		CreateSemaphore (DataCounter, MaxBufferSubscript+1);
		GetPlace := MIN(BufferSubscript);
		PutPlace := MIN(BufferSubscript);
	    END (*WITH*);
	END (*FOR*);
    END InitialiseBuffers;

(************************************************************************)

PROCEDURE Test;

    (* Not normally used.  It's here for when I need to dump the	*)
    (* UART registers.							*)

    VAR j: [0..7];  save: BYTE;  UART: CARDINAL;

    BEGIN
	UART := BasePort[1];
	save := InByte (UART+LCR);
	WriteHexByte (save);  WriteChar (" ");
	OutByte (UART+LCR, IORB(80H, save));
	WriteHexByte (InByte (UART));  WriteChar (" ");
	WriteHexByte (InByte (UART+1));  WriteChar (" ");
	OutByte (UART+LCR, IANDB(7FH, save));
	FOR j := 1 TO 7 DO
	    WriteHexByte (InByte (UART+j));  WriteChar (" ");
	END (*FOR*);
    END Test;

(************************************************************************)

PROCEDURE SaveUARTSettings;

    (* Saves the current state of the UARTS in the system.  Used only	*)
    (* during module initialisation.					*)

    VAR channel: SerialChannelNumber;
	j: [0..6];  save: BYTE;  UART: CARDINAL;

    BEGIN
	FOR channel := 1 TO 2 DO
	    UART := BasePort[channel];
	    save := InByte (UART+LCR);

	    (* Save the main control registers. *)

	    OutByte (UART+LCR, IANDB(7FH, save));
	    FOR j := 1 TO 6 DO
		SavedRegisters[channel][j] := InByte (UART+j);
	    END (*FOR*);

	    (* Save the baud rate divisor. *)

	    OutByte (UART+LCR, IORB(80H, save));
	    FOR j := 0 TO 1 DO
		SavedRegisters[channel][j+7] := InByte (UART+j);
	    END (*FOR*);

	    (* Put back the original contents of the LCR. *)

	    OutByte (UART+LCR, save);
	    SavedRegisters[channel][LCR] := save;

	END (*FOR*);

    END SaveUARTSettings;

(************************************************************************)

PROCEDURE RestoreUARTSettings;

    (* Removes the interrupt tasks, and loads the UARTS with the values	*)
    (* saved during module initialisation.				*)

    VAR channel: SerialChannelNumber;
	j: [0..6];  save: BYTE;  UART: CARDINAL;

    BEGIN
	FOR channel := 1 TO 2 DO
	    KillInterruptTask (IntNum[channel]);
	    UART := BasePort[channel];
	    save := SavedRegisters[channel][LCR];

	    (* Restore the baud rate divisor. *)

	    OutByte (UART+LCR, IORB(80H, save));
	    FOR j := 0 TO 1 DO
		OutByte (UART+j, SavedRegisters[channel][j+7]);
	    END (*FOR*);

	    (* Restore the main control registers. *)

	    OutByte (UART+LCR, IANDB(7FH, save));
	    FOR j := 1 TO 6 DO
		OutByte (UART+j, SavedRegisters[channel][j]);
	    END (*FOR*);

	END (*FOR*);

    END RestoreUARTSettings;

(************************************************************************)

VAR dummy: BYTE;

BEGIN
    (*Test;*)
    dummy := InByte (21H);
    SaveUARTSettings;  InitialiseBuffers;
    SetTerminationProcedure (RestoreUARTSettings);
    CreateInterruptTask (IntNum[1], COM1InterruptTask, "COM1 interrupt");
    CreateInterruptTask (IntNum[2], COM2InterruptTask, "COM2 interrupt");
END SerialIO.
