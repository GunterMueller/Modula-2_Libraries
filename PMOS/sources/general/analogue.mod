IMPLEMENTATION MODULE AnalogueIO;

	(********************************************************)
	(*							*)
	(*		Analogue Input and Output.		*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	24 August 1993			*)
	(*  Status:						*)
	(*	Polling method is working.			*)
	(*	DMA method fails, reason not yet known.		*)
	(*							*)
	(********************************************************)

FROM Windows IMPORT	(* for testing *)
    (* type *)	Window, Colour, FrameType, DividerType,
    (* proc *)	OpenWindow, WriteString, WriteLn;

FROM NumericIO IMPORT	(* for testing *)
    (* proc *)	WriteHexByte, WriteCard, WriteLongCard;

FROM SYSTEM IMPORT
    (* type *)	ADDRESS,
    (* proc *)	ADR;

FROM LowLevel IMPORT
    (* proc *)	OutByte, InByte,
		LowByte, HighByte, MakeWord,
		LS, IANDB, AddOffset;

FROM DMA IMPORT
    (* proc *)	LoadDMAparameters;

FROM TaskControl IMPORT
    (* proc *)	WaitForInterrupt, CreateInterruptTask;

FROM Semaphores IMPORT
    (* type *)	Semaphore,
    (* proc *)	CreateSemaphore, DestroySemaphore, Wait, Signal;

(************************************************************************)

CONST testing = FALSE;

VAR debug: Window;	(* for testing *)

CONST

    (* IOBase is the starting port number for the I/O board.  The	*)
    (* value is jumper selectable.  The factory setting is 300H.	*)

    IOBase = 300H;

    (* The interrupt number is jumper selectable, with IRQ2 as the	*)
    (* factory setting.  The interrupt controller normally maps IRQ2 to	*)
    (* processor interrupt number 10.  The PC/AT is different: IRQ2 is	*)
    (* reserved as the slave request from interrupt controller 2, so	*)
    (* the request line which would have been IRQ2 becomes IRQ9, which	*)
    (* is the second request line of interrupt controller 2 and which	*)
    (* the interrupt controller maps to processor interrupt number 113	*)
    (* (=71H).  (Confused yet?  It get worse.)  But then the BIOS maps	*)
    (* interrupt 113 back to processor interrupt number 10.  Where does	*)
    (* this leave us?   If we don't mind the slight overhead caused by	*)
    (* this software mapping, we can just use interrupt number 10 and	*)
    (* keep compatibility across computer models.  That's what this	*)
    (* version of this module does.  If, on the other hand, the		*)
    (* sampling rate is so high that that overhead is intolerable, then	*)
    (* we would have to deal with the slave interrupt controller.	*)

    InterruptNumber = 10;

    (* The I/O board has 14 addressable ports, of which the last two	*)
    (* belong to the on-board counter/timer chip.			*)

    CounterDataPort = IOBase+12;
    CounterControlPort = CounterDataPort+1;

    (* Each of the five counters has a Counter Mode Register.  The bits	*)
    (* in this register govern the following options:			*)
    (*	bits 15-13	gating control (000=no gating)			*)
    (*	bit     12	1 = count on falling clock edge			*)
    (*	bits  11-8	counter source selection			*)
    (*	bits   7-5	counter mode					*)
    (*	bit	 4	0 = binary count, 1 = BCD count			*)
    (*	bit	 3	0 = count down, 1 = count up			*)
    (*	bits   2-0	output control					*)
    (* The following mnemonics define options which we want to use.	*)
    (* For brevity, the options we never use are omitted.		*)

    ActiveHighGate = 8000H;
    F1source = 0B00H;
    ModeQ = 0A0H;
    ModeJ = 60H;
    ModeD = 20H;
    ToggledOutput = 2;
    PulseOutput = 1;

    (* Some useful commands to the counter/timer chip.	*)

    Select4 = 8;
    Select5 = 16;
 
    LoadDataPointerRegister = 0;
    LoadAllCounters = 5FH;
    LoadAndArm = 060H;
    Disarm = 0C0H;
    EnableDataPointerSequencing = 0E0H;
    ClearToggle = 0E0H;
    ResetCounters = 0FFH;

    (* Definitions for the DMA controller. *)

    DMAchannel = 1;
    DMAReadCode = 1;

(************************************************************************)

TYPE
    SamplingMethod = (DMA, Polling);

VAR
    (* The following record keeps track of information needed during	*)
    (* periodic sampling, to save the caller the bother of repeating	*)
    (* the information at every sample time.				*)

    SamplingInfo:   RECORD
			method: SamplingMethod;
			ChannelSelectByte: BYTE;
			BufferAddress: ADDRESS;
			ByteCount: CARDINAL;
		    END (*RECORD*);

    (* The semaphore called tick is used only when analogue input is	*)
    (* being collected in the periodic sampling mode.			*)

    tick: Semaphore;

(************************************************************************)
(*				DIGITAL I/O				*)
(************************************************************************)

PROCEDURE DigitalOut (value: BYTE);

    (* Sends the given value to the digital output port of the board.	*)

    BEGIN
	OutByte (IOBase+11, value);
    END DigitalOut;

(************************************************************************)

PROCEDURE DigitalInput (): BYTE;

    (* Reads the digital input port of the board.	*)

    BEGIN
	RETURN InByte (IOBase+10);
    END DigitalInput;

(************************************************************************)
(*		      ANALOGUE OUTPUT (RTI-815 ONLY)			*)
(************************************************************************)

PROCEDURE AnalogueOut (channel: OutputChannelNumber;  value: WORD);

    (* Analogue output.  The channel number should be 0 or 1.  Only the	*)
    (* least significant 12 bits of the value are used.  The value can	*)
    (* be treated as either a signed or an unsigned 12-bit number,	*)
    (* depending on hardware jumper selections.				*)

    BEGIN
	OutByte (IOBase+5+2*channel, LowByte(value));
	OutByte (IOBase+5+2*channel+1, HighByte(value));
    END AnalogueOut;

(************************************************************************)
(*		      ANALOGUE INPUT - SINGLE SAMPLE			*)
(************************************************************************)

PROCEDURE AnalogueInput (channel: InputChannelNumber; gain: GainCode): WORD;

    (* Analogue input.  The value returned can be a signed or unsigned	*)
    (* number, depending on jumper selections on the board.		*)
    (* This procedure picks up a single sample when called.  It does	*)
    (* not use interrupts or DMA.  It is recommended for use only in	*)
    (* those cases (e.g. isolated sample, or aperiodic sampling) where	*)
    (* the caller takes responsibility for timing.  More commonly, the	*)
    (* periodic sampling procedures, given later, will be more		*)
    (* appropriate.  This procedure should NOT be called when periodic	*)
    (* sampling has been activated; the results would be unpredictable.	*)

    BEGIN
	(* Put the A/D converter into non-interrupt, non-DMA mode, and	*)
	(* clear any flags which might be outstanding.			*)

	OutByte (IOBase, 0);		(* status/control port *)
	OutByte (IOBase+9, 0);		(* "flags clear" port *)

	(* Start the conversion.  (Function LS is a left shift).	*)

	OutByte (IOBase+1, BYTE(channel + ORD(LS(gain, 5))));
	OutByte (IOBase+2, 0);		(* "start conversion" port *)

	(* Wait for conversion complete.  The conversion time is in the	*)
	(* range 8-35 microseconds, depending on things like hardware	*)
	(* options selected, so there's not enough time to make a task	*)
	(* switch worthwhile.  This is why this procedure does not use	*)
	(* an interrupt to sense completion.				*)

	WHILE ORD(IANDB (InByte(IOBase),60H)) = 0 DO
	    (* busy wait, until either the DONE or	*)
	    (* OVERRUN ERROR flag is set.		*)
	END (*WHILE *);

	(* Return the value.  Note that we don't bother telling the	*)
	(* caller about an overrun error, since that is normally caused	*)
	(* by trying to drive the A/D converter too fast, and overheads	*)
	(* like procedure entry time make that impossible with this	*)
	(* procedure.							*)

	RETURN MakeWord (InByte(IOBase+4),InByte(IOBase+3));

    END AnalogueInput;

(************************************************************************)
(*		    ANALOGUE INPUT - PERIODIC SAMPLING			*)
(************************************************************************)

PROCEDURE DumpCounter (w: Window;  counter: SHORTCARD);

    (* For testing: dumps the mode, load, and hold registers of the	*)
    (* given counter.							*)

    BEGIN

	OutByte (CounterControlPort, EnableDataPointerSequencing);
	OutByte (CounterControlPort, LoadDataPointerRegister + counter);

	WriteHexByte (w, InByte (CounterDataPort));
	WriteString (w, "  ");
	WriteHexByte (w, InByte (CounterDataPort));
	WriteString (w, "  ");
	WriteHexByte (w, InByte (CounterDataPort));
	WriteString (w, "  ");
	WriteHexByte (w, InByte (CounterDataPort));
	WriteString (w, "  ");
	WriteHexByte (w, InByte (CounterDataPort));
	WriteString (w, "  ");
	WriteHexByte (w, InByte (CounterDataPort));
	WriteLn (w);

    END DumpCounter;

(************************************************************************)

PROCEDURE LoadCounter (counter: SHORTCARD;
			mode, loadvalue, holdvalue: CARDINAL);

    (* Sets the mode, and the values for the Load and Hold Registers,	*)
    (* for the specified counter.  This does not arm the counter - it	*)
    (* just sets initial register values.  (Note that the Hold Register	*)
    (* contents are irrelevant for many modes; but we specify the value	*)
    (* anyway, in order to have a uniform calling sequence in all cases.*)

    BEGIN

	(* Internal registers of the counter/timer chip are addressed	*)
	(* by sending a command to load the internal data pointer	*)
	(* register, which then sequences through the internal register	*)
	(* file as we send successive bytes to the data port.		*)

	OutByte (CounterControlPort, EnableDataPointerSequencing);
	OutByte (CounterControlPort, LoadDataPointerRegister + counter);

	OutByte (CounterDataPort, LowByte(mode));
	OutByte (CounterDataPort, HighByte(mode));
	OutByte (CounterDataPort, LowByte(loadvalue));
	OutByte (CounterDataPort, HighByte(loadvalue));
	OutByte (CounterDataPort, LowByte(holdvalue));
	OutByte (CounterDataPort, HighByte(holdvalue));

    END LoadCounter;

(************************************************************************)

PROCEDURE PollingMethod (SamplingInterval: LONGCARD);

    (* Sets up periodic sampling by using counter number 4 to trigger	*)
    (* an A/D conversion at the end of each sampling interval, and	*)
    (* cause an interrupt on the "conversion complete" condition.  If	*)
    (* only a single channel is being sampled, then the interrupt task	*)
    (* can finish the job by reading the converted value.  For multi-	*)
    (* channel sampling, the interrupt task can use a polling method to	*)
    (* read the remaining channels.  This approach is suitable for the	*)
    (* case where the sampling interval is long, or where there is only	*)
    (* one channel being sampled.  In the multi-channel case, we have	*)
    (* some processor overhead caused by busy waits in the interrupt	*)
    (* task, but this is not too serious because we only use this	*)
    (* method when we are sampling infrequently.			*)

    VAR clock: CARDINAL;  timer4count: LONGCARD;

    BEGIN

	SamplingInfo.method := Polling;

	(* The count for timer 4 is the sampling interval divided by	*)
	(* the clock frequency.  We have a choice of five clock		*)
	(* frequencies, namely F1=1MHz, F2=F1/16, F3=F2/16, etc.  The	*)
	(* choice of which of these to use is dictated by the fact that	*)
	(* the count must fit into 16 bits.  The clock is specified in	*)
	(* bits 11-8 of the counter mode register as 1011 for F1, 1100	*)
	(* for F2, 1101 for F3, etc.					*)

	clock := 11;  timer4count := SamplingInterval;
	IF testing THEN
	    WriteString (debug, "clock, count = ");
	    WriteCard (debug, clock);  WriteLongCard (debug, timer4count);
	    WriteLn (debug);
	END (*IF*);
	WHILE timer4count > 65535 DO
	    INC (clock);  timer4count := timer4count DIV 16;
	    IF testing THEN
		WriteString (debug, "clock, count = ");
		WriteCard (debug, clock);  WriteLongCard (debug, timer4count);
		WriteLn (debug);
	    END (*IF*);
	END (*WHILE*);

	(* Use mode D for timer 4.  This is a straightforward "rate	*)
	(* generator" mode, with no gating.				*)

	LoadCounter (4, ORD(LS(clock, 8)) + ModeD + PulseOutput,
						CARDINAL(timer4count), 0);

	(* Initialise the A/D converter by clearing any flags which	*)
	(* might be outstanding, specifying the gain and starting	*)
	(* channel number, and enabling interrupts on end of conversion	*)
	(* or overrun.							*)

	OutByte (IOBase+1, SamplingInfo.ChannelSelectByte);
	OutByte (IOBase+9, 0);			(* "flags clear" port *)
	OutByte (IOBase, 3);			(* enable interrupts  *)

	(* Load and arm counter 4.  This will start the periodic	*)
	(* sampling operation.						*)

	OutByte (CounterControlPort, LoadAndArm + Select4);

    END PollingMethod;

(************************************************************************)

PROCEDURE DMAMethod (NumberOfChannels, SamplingInterval: CARDINAL;
					AmplifierGain: GainCode);

    (* Sets up periodic sampling by using counter 4 to trigger the A/D	*)
    (* converter as fast as possible, once for each channel to be	*)
    (* sampled, using DMA to put the results into main memory.  Counter	*)
    (* number 5 is used as a gating source for counter number 4; the	*)
    (* width of the gating pulse controls the number of channels to be	*)
    (* sampled, and the time between gating pulses controls the		*)
    (* sampling interval.  This method can be used only when the	*)
    (* sampling interval is less than 65536 microseconds, because	*)
    (* counter 5 is 16 bits wide and will be driven from a 1 MHz clock.	*)
    (* (We could, with some ingenuity, relax this limit by using a	*)
    (* lower clock frequency, but then we would lose fine control of	*)
    (* the width of the gating pulse, possibly to the point where we	*)
    (* could not correctly control the number of channels to sample.	*)
    (* One way to solve this problem would be to sample more channels	*)
    (* than are actually required, but this would give extra software	*)
    (* complexity, and it is not certain that this is justified).	*)
    (* Remark: there are hardwired connections from the output of	*)
    (* counter 5 to the gate of counter 4, from the output of counter 4	*)
    (* to the A/D converter, and from the output of counter 4 to timer	*)
    (* source number 5.  The first two connections are useful to us,	*)
    (* the last is not; but this is not a limitation since we can	*)
    (* program the counters to ignore the "source" inputs and instead	*)
    (* take their inputs from a 1MHz crystal oscillator called F1.	*)

    VAR timer4count, GateWidth: CARDINAL;

    BEGIN

	SamplingInfo.method := DMA;

	(* First, calculate a count for timer 4 which will produce a	*)
	(* train of pulses at a suitable rate, taking into account the	*)
	(* time needed for an A/D conversion.				*)
	(* The A/D conversion time is 25 microseconds, but for high	*)
	(* gains in the input amplifier this must be increased to allow	*)
	(* for a settling time in the amplifier.			*)
	(* For future thought: the hardware seems to include an option	*)
	(* which would allow us to trim this time down, but it's not	*)
	(* quite clear just how fast we can go.				*)

	IF AmplifierGain = 3 THEN
	    timer4count := 80;
	ELSIF AmplifierGain = 2 THEN
	    timer4count := 40;
	ELSE
	    timer4count := 25;
	END (*IF*);

	GateWidth := NumberOfChannels*timer4count;

	(* Use mode Q for timer 4.  In this mode, the counter does not	*)
	(* count while the gate is low.  The first clock input after	*)
	(* the gate goes high loads the counter from its load register,	*)
	(* and counting starts on the second clock input.  When the	*)
	(* count hits zero, the A/D converter is triggered, and the	*)
	(* counter is reloaded from its load register so that counting	*)
	(* starts again.  This happens repeatedly until the gate goes	*)
	(* low again.							*)

	LoadCounter (4, ActiveHighGate + F1source + ModeQ + PulseOutput,
							timer4count, 0);

	(* Use mode J for timer 5.  This makes the counter a free-	*)
	(* running oscillator where the count is reloaded each time it	*)
	(* gets to zero, alternately reloading from the Load and Hold	*)
	(* registers.  (The Load Register is the one which is used when	*)
	(* we initially load and arm the counter.)			*)

	LoadCounter (5, F1source + ModeJ + ToggledOutput,
			SamplingInterval - GateWidth, GateWidth);

	(* Set the initial output of counter 5 low.  This operation is	*)
	(* necessary because counter 5 has a toggled output (unlike	*)
	(* counter 4, which only puts out a pulse each time its count	*)
	(* goes to zero), so we must explicitly specify its initial	*)
	(* condition.							*)

	OutByte (CounterControlPort, ClearToggle + 5);

	(* Arm the A/D converter and the DMA controller.  The A/D	*)
	(* conversions will not start, however, until triggered by the	*)
	(* first pulse from timer 4.					*)

	WITH SamplingInfo DO
	    LoadDMAparameters (DMAchannel, DMAReadCode,
					BufferAddress, ByteCount);
	    OutByte (IOBase+1, ChannelSelectByte);
	END (*WITH*);

	(* Enable DMA mode, with interrupt on DMA completion.	*)

	OutByte (IOBase, 0CH);

	(* Load and arm both counters.  This will start the cycle.	*)

	OutByte (CounterControlPort, LoadAndArm + Select4 + Select5);

    END DMAMethod;

(************************************************************************)

PROCEDURE StartPeriodicSampling (first, last: InputChannelNumber;
					SamplingInterval: LONGCARD;
					AmplifierGain: GainCode;
					VAR (*OUT*) Buffer: ARRAY OF BYTE);

    (* Initiates a mode of operation in which channels first..last,	*)
    (* inclusive, will be sampled every SamplingInterval microseconds,	*)
    (* with the results stored in array Buffer.  At each sampling time,	*)
    (* the specified channels are read as nearly simultaneously as the	*)
    (* hardware will allow.  Procedure WaitForNextSample, below, should	*)
    (* be called to check when the data are available in array Buffer.	*)
    (* If WaitForNextSample is not called often enough, there can be a	*)
    (* data overrun in which data are overwritten.  We do not signal	*)
    (* this as an error since the only thing which can be done about it	*)
    (* is to use the new data and ignore whatever data have been lost.	*)

    BEGIN
	WITH SamplingInfo DO
	    ChannelSelectByte := BYTE (first + ORD(LS(AmplifierGain, 5)));
	    IF last <> first THEN
		(* Specify that the channel number will auto-increment *)
		INC (ChannelSelectByte, 80H);
	    END (*IF*);
	    BufferAddress := ADR (Buffer);
	    ByteCount := 2*(last-first+1);
	END (*WITH*);
	CreateSemaphore (tick, 0);

	(* Temporary patch: since the DMA method seems to have a bug in	*)
	(* it (reason so far unknown), use the polling method always.	*)

	PollingMethod (SamplingInterval);
(*
	IF (first=last) OR (SamplingInterval>65535) THEN
	    PollingMethod (SamplingInterval)
	ELSE
	    DMAMethod (last-first+1,SHORT(SamplingInterval),AmplifierGain);
	END (*IF*);
*)
	IF testing THEN
	    DumpCounter (debug, 4);
	    DumpCounter (debug, 5);
	END (*IF*);

    END StartPeriodicSampling;

(************************************************************************)

PROCEDURE WaitForNextSample;

    (* Pauses until the buffer specified in the preceding procedure has	*)
    (* been filled with valid data.  Notice that this is essentially a	*)
    (* synchronization procedure; the caller does not have to do any	*)
    (* timing operations since a return from this procedure implies	*)
    (* that the next sample time has arrived.				*)
    (* WARNINGS:							*)
    (*  1. This procedure should not be called unless periodic sampling	*)
    (*     is currently in effect.  Otherwise, it might never return.	*)
    (*  2. Periodic sampling implies that the data buffer is re-filled	*)
    (*	   regularly, regardless of whether the user code has finished	*)
    (*	   with the previous data.  There are no interlocks, and no	*)
    (*	   protection against data being updated just as one is reading	*)
    (*	   it.  (Any such protection could interfere with the precision	*)
    (*	   of the timing of sampling external data).  The caller is	*)
    (*	   advised to move data out of the buffer promptly, especially	*)
    (*	   when the sampling rate is high.				*)

    BEGIN

	(* Wait for the interrupt which indicates operation complete.	*)
	(* This is all we need to do; the interrupt task looks after	*)
	(* all the details of re-arming the hardware, etc.		*)

	Wait (tick);

    END WaitForNextSample;

(************************************************************************)

PROCEDURE StopPeriodicSampling;

    (* Turns off the periodic sampling mode of A/D conversion.	*)

    BEGIN

	(* Disarm the A/D converter.	*)

	OutByte (IOBase, 0);
	OutByte (IOBase+9, 0);

	(* Stop the clocks (counters 4 and 5).	*)

	OutByte (CounterControlPort, Disarm + Select4 + Select5);

	DestroySemaphore (tick);

    END StopPeriodicSampling;

(************************************************************************)
(*			THE INTERRUPT TASK				*)
(************************************************************************)

PROCEDURE InterruptTask;

    (* This task has a job to do only during periodic sampling, since	*)
    (* that is the only time we use interrupts.  Its job depends on	*)
    (* which method of periodic sampling is in use (see above - we use	*)
    (* one of two different methods, depending on the sampling interval	*)
    (* and the number of channels being sampled).  The cases are:	*)
    (*	1. DMA method: the interrupt occurs on DMA completion, so we	*)
    (*	   only have to set up the DMA chip to be ready for the next	*)
    (*	   batch of data, re-arm the A/D converter, and inform the user	*)
    (*	   that the current data have arrived.				*)
    (*	2. Polling method: the interrupt occurs on A/D completion for	*)
    (*	   the first channel to be sampled, and we still have to store	*)
    (*	   the datum in memory.  If multiple channels are to be sampled	*)
    (*	   then the interrupt task must finish the job by sampling the	*)
    (*	   remaining channels.  (This puts a busy wait inside an	*)
    (*	   interrupt task, which is normally not a good idea; it is	*)
    (*	   justified in this case because the amount of time to wait is	*)
    (*	   less than the time it would take to leave and re-enter the	*)
    (*	   interrupt task).  Finally, the A/D converter must be		*)
    (*	   re-armed to be ready for the next batch of samples.		*)

    VAR resultptr: POINTER TO BYTE;
	BytesRemaining: CARDINAL;

    BEGIN
	LOOP (*FOREVER*)
	    WaitForInterrupt;
	    OutByte (IOBase+9, 0);		(* "flags clear" port *)

	    WITH SamplingInfo DO
		IF method = DMA THEN

		    (* Re-arm the DMA controller for the next sample. *)

		    LoadDMAparameters (DMAchannel, DMAReadCode,
						BufferAddress, ByteCount);

		ELSE	(* We are using the polling method *)

		    (* Temporarily disable A/D interrupts. *)

		    OutByte (IOBase, 0);	(* status/control port *)
		    resultptr := BufferAddress;
		    BytesRemaining := ByteCount;

		    LOOP
			(* Read in a result.	*)

			resultptr^ := InByte(IOBase+3);	(* low order byte *)
			resultptr := AddOffset (resultptr, 1);
			resultptr^ := InByte(IOBase+4);	(* high order byte *)
			resultptr := AddOffset (resultptr, 1);

			DEC (BytesRemaining, 2);
			IF BytesRemaining = 0 THEN
			    EXIT (*LOOP*)
			END (*IF*);

			(* Perform the next conversion. *)

			OutByte (IOBase+2, 0);	(* "start conversion" port *)
			WHILE ORD(IANDB (InByte(IOBase),60H)) = 0 DO
			    (* busy wait, until either the DONE or	*)
			    (* OVERRUN ERROR flag is set.		*)
			END (*WHILE *);

		    END (*LOOP*);

		    (* Re-enable interrupts. *)

		    OutByte (IOBase, 3);

		END (*IF*);

		(* Rearm the D/A converter. *)

		OutByte (IOBase+1, ChannelSelectByte);

	    END (*WITH*);

	    Signal (tick);

	END (*LOOP*);

    END InterruptTask;

(************************************************************************)
(*			    INITIALISATION				*)
(************************************************************************)

PROCEDURE InitialSetup;

    (* Resets the A/D board and the counter/timer chip and installs	*)
    (* the interrupt task.						*)

    BEGIN
	OutByte (CounterControlPort, ResetCounters);
	OutByte (CounterControlPort, LoadAllCounters);
	OutByte (CounterControlPort, LoadDataPointerRegister + 4);
	OutByte (IOBase, 0);		(* disable interrupts *)
	OutByte (IOBase+9, 0);		(* clear flags *)
	CreateInterruptTask(InterruptNumber,InterruptTask,"A/D int handler");
    END InitialSetup;

(************************************************************************)

BEGIN
    IF testing THEN
	OpenWindow (debug,yellow,blue,5,11,0,79,doubleframe,nodivider);
    END (*IF*);
    InitialSetup;
END AnalogueIO.
