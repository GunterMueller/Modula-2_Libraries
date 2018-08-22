IMPLEMENTATION MODULE Printer;

	(********************************************************)
	(*							*)
	(*	    Device driver for the printer.		*)
	(*							*)
	(*	Author:		P. Moylan			*)
	(*	Last edited:	6 March 1995			*)
	(*	Status:		Partly working			*)
	(*							*)
	(*	More precisely: it was working with my earlier	*)
	(*	Epson printer, but is leaving the DeskJet in	*)
	(*	a "busy" state at the completion of printing.	*)
	(*	I haven't yet tracked down the reason.		*)
	(*							*)
	(********************************************************)

(************************************************************************)
(*									*)
(*  The printer interface is addressed through three consecutive I/O	*)
(*  ports: the data register, the status register, and the control	*)
(*  register, in that order.  An unusual feature of the interface is	*)
(*  that putting a character into the data register does not in itself	*)
(*  cause the character to be printed - it is also necessary to		*)
(*  manipulate a "strobe" bit in the control register.			*)
(*									*)
(*  The status register is almost unused by this module, but for	*)
(*  interest's sake here are the bit assignments:			*)
(*	   bit	7	1=not busy, i.e. ready				*)
(*		6	1=acknowledge (not sure what is acknowledged)	*)
(*		5	1=out of paper					*)
(*		4	1=selected					*)
(*		3	1=I/O error					*)
(*		2-0	unused						*)
(*									*)
(*  The bit assignments for the control register, deduced from tests	*)
(*  and from some very sketchy documentation, appear to	be:		*)
(*	   bit	7-5	unknown (unused?)				*)
(*		4	1 = interrupt enable				*)
(*		3	should be 1, not sure why			*)
(*		2	1=normal operation, 0=init			*)
(*		1	1=automatic line feed after carriage return	*)
(*		0	1=output strobe					*)
(*									*)
(*  For normal operation, we use a value of 1CH, which specifies	*)
(*  interrupt enable, no initialisation, no auto line feed.  When we	*)
(*  actually send a character, we also have to set the strobe bit.	*)
(*									*)
(*  Bit 3 is still a mystery to me.  The tests I have done so far	*)
(*  suggest that this bit has no effect.				*)
(*									*)
(************************************************************************)

FROM LowLevel IMPORT
    (* proc *)	IANDB, IORB, InByte, OutByte;

FROM TaskControl IMPORT
    (* proc *)	CreateInterruptTask, WaitForInterrupt;

FROM TerminationControl IMPORT
    (* proc *)	SetTerminationProcedure;

FROM Semaphores IMPORT
    (* type *)	Semaphore,
    (* proc *)	CreateSemaphore, Wait, Signal;

(************************************************************************)

CONST
    IntMaskRegister = 21H;

    PrinterDataPort = 278H;
    PrinterInterruptNumber = 15;
    InterruptMask = 80H;

    (********************************************************************)
    (*									*)
    (*  Note: the "official" values of these constants are:		*)
    (*					port #1		port #2		*)
    (*		PrinterDataPort          378H		 278H		*)
    (*		PrinterInterruptNumber	  15		  13		*)
    (*		InterruptMask		  80H             20H		*)
    (*									*)
    (*  For some reason, my printer seems to be using the interrupt	*)
    (*  belonging to printer port #1, but the port number of printer	*)
    (*  port #2.							*)
    (*									*)
    (********************************************************************)

    PrinterStatusPort = PrinterDataPort + 1;
    PrinterControlPort = PrinterDataPort + 2;
    ControlByte = 1CH;
    MaxBufferSubscript = 7;

TYPE
    BufferSubscript = [0..MaxBufferSubscript];

VAR

    (* OutputBuffer is a circular buffer holding characters to be sent	*)
    (* to the printer.  The SpaceAvailable semaphore is a counting	*)
    (* semaphore which keeps track of how many unused character slots	*)
    (* remain in the buffer.  OutPlace points to the next character	*)
    (* which will be sent to the printer.  PutPlace is the place where	*)
    (* the next user output character will be put.  The condition	*)
    (* OutPlace=PutPlace could mean either that the buffer is		*)
    (* completely empty or that it is completely full.  We can tell the	*)
    (* difference by knowing whether we have just put something in the	*)
    (* buffer or removed something from it.				*)

    OutputBuffer:   RECORD
			SpaceAvailable: Semaphore;
			OutPlace, PutPlace: BufferSubscript;
			data: ARRAY BufferSubscript OF CHAR
		    END;

(************************************************************************)

PROCEDURE EnablePrinterInterrupts;

    (* Allows printer interrupts to reach the processor, by clearing	*)
    (* the appropriate bit in the interrupt mask register.  This works	*)
    (* provided that there are no other devices sharing the same	*)
    (* interrupt request.  If interrupt lines were shared, then		*)
    (* interrupts would have to be turned on and off at the printer	*)
    (* interface itself.						*)

    BEGIN
	OutByte (IntMaskRegister,
		IANDB (InByte(IntMaskRegister), 0FFH - InterruptMask));
    END EnablePrinterInterrupts;

(************************************************************************)

PROCEDURE DisablePrinterInterrupts;

    BEGIN
	OutByte (IntMaskRegister,
			IORB (InByte(IntMaskRegister), InterruptMask));
    END DisablePrinterInterrupts;

(************************************************************************)

PROCEDURE PrinterInterruptTask;

    (* This task spends most of its time idle in the WaitForInterrupt	*)
    (* call, and is resumed by an interrupt from the printer.  It then	*)
    (* takes the first available character from the circular buffer	*)
    (* OutputBuffer, and performs a semaphore Signal to indicate that	*)
    (* there is extra free space in the buffer.  If the buffer is empty	*)
    (* after this operation, further printer interrupts are disabled.	*)
    (* Procedure PrintChar (see below) re-enables printer interrupts	*)
    (* when it puts a new character in the buffer.			*)

    CONST strobe = 1;

    BEGIN
	LOOP (*FOREVER*)
	    WaitForInterrupt;
	    WITH OutputBuffer DO

		OutByte (PrinterDataPort, data[OutPlace]);

		(* With this particular printer interface, writing to	*)
		(* the printer data port does not strobe the data out	*)
		(* to the printer.  (Why not?  No obvious reason, but	*)
		(* perhaps the hardware designer just had had a bad	*)
		(* day).  Instead, we must explicitly create a strobe	*)
		(* pulse by setting the low order bit of the control	*)
		(* port, and then clearing it.				*)

		OutByte (PrinterControlPort, ControlByte + strobe);
		OutByte (PrinterControlPort, ControlByte);

		IF OutPlace = MaxBufferSubscript THEN OutPlace := 0
		ELSE INC (OutPlace);
		END (*IF*);

		(* If now OutPlace=PutPlace, the buffer is empty, so	*)
		(* the printer must be disabled until the buffer again	*)
		(* contains something to print.				*)

		IF OutPlace = PutPlace THEN
		    DisablePrinterInterrupts;
		END (*IF*);

		Signal (SpaceAvailable);

	    END (*WITH*);
	END (*LOOP*);
    END PrinterInterruptTask;

(************************************************************************)

PROCEDURE PrintChar (ch: CHAR);

    (* Puts one character into the output buffer.  This procedure	*)
    (* contains a somewhat subtle critical section, which can be	*)
    (* explained as follows.  When the buffer is empty, printer		*)
    (* interrupts need to be disabled, since otherwise the interrupt	*)
    (* routine will print rubbish.  Thus, we should re-enable the	*)
    (* printer after putting a new character in the buffer.  However,	*)
    (* there is a possible sequence of events in which we put a new	*)
    (* character in the buffer, and then the interrupt routine prints	*)
    (* that character before we have updated the buffer pointer		*)
    (* PutPlace.  In that case the interrupt routine could make a wrong	*)
    (* decision about whether the buffer is empty.  The detailed	*)
    (* consequences depend on whether we enable interrupts before or	*)
    (* after updating PutPlace, but in either case we could end up with	*)
    (* printer interrupts disabled when they should be enabled, or vice	*)
    (* versa.  The solution is to make the combination of updating	*)
    (* PutPlace and enabling printer interrupts indivisible.  We do	*)
    (* this by disabling printer interrupts (whether or not they were	*)
    (* already disabled) before modifying PutPlace.			*)

    BEGIN
	WITH OutputBuffer DO
	    Wait (SpaceAvailable);
	    data[PutPlace] := ch;

	    (* Since it is possible that the buffer was empty before	*)
	    (* adding this new character, printer interrupts might be	*)
	    (* currently disabled, and we should enable them.  The	*)
	    (* combined operations of updating PutPlace and enabling	*)
	    (* the printer creates a critical section, which we protect	*)
	    (* by disabling the printer in case it was already enabled.	*)

	    DisablePrinterInterrupts;
	    IF PutPlace = MaxBufferSubscript THEN PutPlace := 0
	    ELSE INC (PutPlace);
	    END (*IF*);
	    EnablePrinterInterrupts;

	END (*WITH*);
    END PrintChar;

(************************************************************************)

PROCEDURE WaitForPrinterIdle;

    (* Does not return until the printer buffer is empty.  Of course,	*)
    (* the caller should also ensure that there are no future calls to	*)
    (* PrintChar; otherwise there is little point in calling this	*)
    (* procedure.							*)

    BEGIN
	WITH OutputBuffer DO
	    REPEAT
		Wait (SpaceAvailable);  Signal (SpaceAvailable);
	    UNTIL OutPlace = PutPlace;
	END (*WITH*);
    END WaitForPrinterIdle;

(************************************************************************)
(*			    INITIALISATION				*)
(************************************************************************)

PROCEDURE InitialisePrinter;

    (* Performs the appropriate hardware initialisation to install the	*)
    (* printer interrupt task and make the printer ready for use.	*)

    BEGIN

	CreateInterruptTask (PrinterInterruptNumber, PrinterInterruptTask,
						"Printer int task");

	(* Initialise printer by driving bit 2 low while keeping bit 3	*)
	(* high.							*)

	OutByte (PrinterControlPort, 8);

	(* Disable interrupts on IRQ 7 of 8259.  The time taken by this	*)
	(* operation seems to be about right for the time we need to	*)
	(* kill while keeping the initialisation bit low.		*)

	DisablePrinterInterrupts;

	(* Now send bit 2 high to finish initialisation operation, and	*)
	(* set bit 4 to enable interrupts.  Meanwhile bit 3 remains	*)
	(* high, I don't know why but that's how I got it to work.	*)
	(* From the documentation, I know I am setting non auto LF, not	*)
	(* sure what else.						*)

	OutByte (PrinterControlPort, ControlByte);

	(* Wait for the printer "ready" bit to be set, to ensure that	*)
	(* hardware initialisation is complete.				*)
(*
	REPEAT
	    (* do nothing *)
	UNTIL ORD(IANDB (InByte (PrinterStatusPort), 80H)) <> 0;
*)
	(* At this stage the printer interrupt task exists, the printer	*)
	(* interface has been initialised and its "interrupt enable"	*)
	(* bit has been set, but the interrupt mask register of the	*)
	(* 8259A interrupt controller has been set up so that it will	*)
	(* not yet pass on printer interrupts to the processor.  Thus,	*)
	(* the first interrupt will not occur until there is something	*)
	(* waiting to be printed.					*)

    END InitialisePrinter;

(************************************************************************)

BEGIN

    (* Initialize the character buffer.	*)

    WITH OutputBuffer DO
	CreateSemaphore (SpaceAvailable, MaxBufferSubscript+1);
	OutPlace := 0;  PutPlace := 0;
    END (*WITH*);

    (* Install the printer interrupt routine.	*)

    InitialisePrinter;

    (* Provide for an orderly shutdown.	*)

    SetTerminationProcedure (WaitForPrinterIdle);

END Printer.
