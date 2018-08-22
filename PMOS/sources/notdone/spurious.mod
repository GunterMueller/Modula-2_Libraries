IMPLEMENTATION MODULE SpuriousInterrupts;

	(****************************************************************)
	(*								*)
	(*	Spurious interrupt handler for operating system.	*)
	(*								*)
	(*	Author:		P. Moylan				*)
	(*	Last edited:	18 March 1991				*)
	(*	Description:						*)
	(*		This module contains interrupt handlers for	*)
	(*		interrupts 0-7 and 11-13, which are not		*)
	(*		supposed to occur in the present version of my	*)
	(*		software.  The aim is to detect unexpected	*)
	(*		interrupts.					*)
	(*								*)
	(*	Status:	Seems OK, but not adequately tested.		*)
	(*								*)
	(****************************************************************)

FROM LowLevelIO IMPORT
    (* proc *)	EnterCriticalSection, LeaveCriticalSection,
		ProcessorStatus;

FROM Interrupts IMPORT
    (* proc *)	InstallInterruptTask, AcknowledgeInterrupt;

FROM TaskControl IMPORT
    (* proc *)	WaitForInterrupt;

FROM GlassTTY IMPORT
    (* proc *)	WriteString, WriteCard, WriteLn, WriteHexWord;

(************************************************************************)
(*									*)
(*  Remark: The interrupts which are NOT spurious, i.e. the ones which	*)
(*  are genuinely used by my software, are:				*)
(*									*)
(*		 8	timer						*)
(*		 9	keyboard					*)
(*		10	analogue input					*)
(*		14	floppy disk					*)
(*		15	printer						*)
(*									*)
(*  The analogue input interrupt is a little strange: the hardware	*)
(*  actually produces an INT 113 (the second request line of interrupt	*)
(*  controller 2), but then the BIOS maps this back to INT 10.		*)
(*									*)
(************************************************************************)

TYPE InterruptNumber = [0..15];

VAR

    (* Array Handler lists the interrupt routines in this module.	*)

    Handler: ARRAY InterruptNumber OF PROC;

(************************************************************************)

PROCEDURE ErrorMessage (number: InterruptNumber);

    (* Puts an error message on the screen.	*)

    BEGIN
	WriteLn; WriteString ("ERROR: Unexpected interrupt ");
	WriteCard (number);
	WriteString ("  Processor status word is ");
	WriteHexWord (ProcessorStatus());
    END ErrorMessage;

(************************************************************************)

PROCEDURE Handler0;

    (* Interrupt routine for interrupt #0.	*)

    BEGIN
	LOOP
	    WaitForInterrupt(0);  ErrorMessage(0);
	END (*LOOP*);
    END Handler0;

(************************************************************************)

PROCEDURE Handler1;

    (* Interrupt routine for interrupt #1.	*)

    BEGIN
	LOOP
	    WaitForInterrupt(1);  ErrorMessage(1);
	END (*LOOP*);
    END Handler1;

(************************************************************************)

PROCEDURE Handler2;

    (* Interrupt routine for interrupt #2.	*)

    BEGIN
	LOOP
	    WaitForInterrupt(2);  ErrorMessage(2);
	END (*LOOP*);
    END Handler2;

(************************************************************************)

PROCEDURE Handler3;

    (* Interrupt routine for interrupt #3.	*)

    BEGIN
	LOOP
	    WaitForInterrupt(3);  ErrorMessage(3);
	END (*LOOP*);
    END Handler3;

(************************************************************************)

PROCEDURE Handler4;

    (* Interrupt routine for interrupt #4.	*)

    BEGIN
	LOOP
	    WaitForInterrupt(4);  ErrorMessage(4);
	END (*LOOP*);
    END Handler4;

(************************************************************************)

PROCEDURE Handler5;

    (* Interrupt routine for interrupt #5.	*)

    BEGIN
	LOOP
	    WaitForInterrupt(5);  ErrorMessage(5);
	END (*LOOP*);
    END Handler5;

(************************************************************************)

PROCEDURE Handler6;

    (* Interrupt routine for interrupt #6.	*)

    BEGIN
	LOOP
	    WaitForInterrupt(6);  ErrorMessage(6);
	END (*LOOP*);
    END Handler6;

(************************************************************************)

PROCEDURE Handler7;

    (* Interrupt routine for interrupt #7.	*)

    BEGIN
	LOOP
	    WaitForInterrupt(7);  ErrorMessage(7);
	END (*LOOP*);
    END Handler7;

(************************************************************************)

PROCEDURE Handler11;

    (* Interrupt routine for interrupt #11.	*)

    BEGIN
	LOOP
	    WaitForInterrupt(11);  ErrorMessage(11);
	    AcknowledgeInterrupt;
	END (*LOOP*);
    END Handler11;

(************************************************************************)

PROCEDURE Handler12;

    (* Interrupt routine for interrupt #12.	*)

    BEGIN
	LOOP
	    WaitForInterrupt(12);  ErrorMessage(12);
	    AcknowledgeInterrupt;
	END (*LOOP*);
    END Handler12;

(************************************************************************)

PROCEDURE Handler13;

    (* Interrupt routine for interrupt #13.	*)

    BEGIN
	LOOP
	    WaitForInterrupt(13);  ErrorMessage(13);
	    AcknowledgeInterrupt;
	END (*LOOP*);
    END Handler13;

(************************************************************************)

PROCEDURE InitialiseInterruptVectors;

    (* Sets up the interrupt vectors for interrupts 0-7 and 11-13, to	*)
    (* point to the appropriate interrupt handlers.			*)

    VAR j: InterruptNumber;

    BEGIN
	EnterCriticalSection;
	FOR j := 0 TO 7 DO
	    InstallInterruptTask (j, Handler[j]);
	END (*FOR*);
	FOR j := 11 TO 13 DO
	    InstallInterruptTask (j, Handler[j]);
	END (*FOR*);
	LeaveCriticalSection;
    END InitialiseInterruptVectors;

(************************************************************************)

BEGIN
    Handler[0] := Handler0;
    Handler[1] := Handler1;
    Handler[2] := Handler2;
    Handler[3] := Handler3;
    Handler[4] := Handler4;
    Handler[5] := Handler5;
    Handler[6] := Handler6;
    Handler[7] := Handler7;

    Handler[11] := Handler11;
    Handler[12] := Handler12;
    Handler[13] := Handler13;

    InitialiseInterruptVectors;
END SpuriousInterrupts.
