MODULE SerialTest;

	(********************************************************)
	(*							*)
	(*		Test of Serial I/O			*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	16 February 1994		*)
	(*  Status:		Seems to be working.		*)
	(*			Not fully tested.		*)
	(*							*)
	(********************************************************)

FROM TaskControl IMPORT
    (* proc *)	CreateTask;

FROM Semaphores IMPORT
    (* type *)	Semaphore,
    (* proc *)	CreateSemaphore, Wait, Signal;

FROM SerialIO IMPORT
    (* type *)	Parity,
    (* proc *)	InitSerialChannel, ReadSerial, WriteSerial;

FROM Windows IMPORT
    (* type *)	Window, Colour, FrameType, DividerType,
    (* proc *)	OpenWindow, ChangeScrollingRegion, CloseWindow,
		WriteString, ReadChar, Write;

(************************************************************************)

VAR TaskDone: Semaphore;

(************************************************************************)

PROCEDURE COM1OutputTask;

    CONST Esc = CHR(27);

    VAR w1: Window;  ch: CHAR;

    BEGIN
	OpenWindow (w1, blue, cyan, 8, 15, 0, 39, simpleframe, doubledivider);
	WriteString (w1, "Output to COM1");
	ChangeScrollingRegion (w1, 3, 6);

	LOOP
	    ReadChar (w1, ch);
	    IF ch = Esc THEN EXIT(*LOOP*) END(*IF*);
	    WriteSerial (1, ch);
	END (*LOOP*);

	CloseWindow (w1);
	Signal (TaskDone);

    END COM1OutputTask;

(************************************************************************)

PROCEDURE COM2OutputTask;

    CONST Esc = CHR(27);

    VAR w1: Window;  ch: CHAR;

    BEGIN
	OpenWindow (w1, white, magenta, 8, 15, 40, 79, simpleframe, doubledivider);
	WriteString (w1, "Output to COM2");
	ChangeScrollingRegion (w1, 3, 6);

	LOOP
	    ReadChar (w1, ch);
	    IF ch = Esc THEN EXIT(*LOOP*) END(*IF*);
	    WriteSerial (2, ch);
	END (*LOOP*);

	CloseWindow (w1);
	Signal (TaskDone);

    END COM2OutputTask;

(************************************************************************)

PROCEDURE COM1InputTask;

    VAR w2: Window;  ch: CHAR;

    BEGIN
	OpenWindow (w2, white, magenta, 0, 7, 0, 39, simpleframe, doubledivider);
	WriteString (w2, "Input from COM1");
	ChangeScrollingRegion (w2, 3, 6);

	LOOP
	    ReadSerial (1, ch);
	    Write (w2, ch);
	END (*LOOP*);

    END COM1InputTask;

(************************************************************************)

PROCEDURE COM2InputTask;

    VAR w2: Window;  ch: CHAR;

    BEGIN
	OpenWindow (w2, blue, cyan, 0, 7, 40, 79, simpleframe, doubledivider);
	WriteString (w2, "Input from COM2");
	ChangeScrollingRegion (w2, 3, 6);

	LOOP
	    ReadSerial (2, ch);
	    Write (w2, ch);
	END (*LOOP*);

    END COM2InputTask;

(************************************************************************)

PROCEDURE RunTheTest;

    BEGIN
	CreateTask (COM1OutputTask, 1, "COM1 output");
	CreateTask (COM1InputTask, 1, "COM1 input");
	CreateTask (COM2OutputTask, 1, "COM2 output");
	CreateTask (COM2InputTask, 1, "COM2 input");
	Wait (TaskDone);  Wait (TaskDone);
    END RunTheTest;

(************************************************************************)
(*				MAIN PROGRAM				*)
(************************************************************************)

BEGIN
    (* The following parameters seem to be appropriate for the mouse.	*)

    InitSerialChannel (1, 9600, 8, NoParity, 1);
    InitSerialChannel (2, 2400, 8, OddParity, 2);
    CreateSemaphore (TaskDone, 0);
    RunTheTest;
END SerialTest.
