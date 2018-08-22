MODULE SoundTst;

	(********************************************************)
	(*							*)
	(*	Test of module SoundEffects.			*)
	(*							*)
	(*	Programmer:	P. Moylan			*)
	(*	Last edited:	16 August 1993			*)
	(*	Status:						*)
	(*	    Seems to be working.			*)
	(*							*)
	(********************************************************)

FROM Trace IMPORT
    (* proc *)	Pause, TraceOn, InTrace, OutTrace;

FROM Windows IMPORT
    (* type *)	Window, Colour, FrameType, DividerType,
    (* proc *)	OpenWindow, CloseWindow, SetCursor;

FROM NumericIO IMPORT
    (* proc *)	WriteCard;

FROM SoundEffects IMPORT
    (* type *)	Note,
    (* proc *)	Beep, Play;

FROM Semaphores IMPORT
    (* type *)	Semaphore,
    (* proc *)	CreateSemaphore, Wait;

(************************************************************************)

PROCEDURE Test1;

    (* Performs a simple beep.	*)

    BEGIN
	InTrace ("Test1");
	Beep;
	OutTrace ("Test1");
    END Test1;

(************************************************************************)

PROCEDURE Test2;

    (* Plays a short sequence of notes.	*)

    CONST max = 10;

    VAR tune: ARRAY [0..max] OF Note;
	j: [0..max];
	done: Semaphore;

    BEGIN
	InTrace ("Test2");
	CreateSemaphore (done, 0);
	FOR j := 0 TO max DO tune[j].duration := 200;  END (*FOR*);
	tune[0].period := 600;
	tune[1].period := 500;
	tune[2].period := 1;
	tune[3].period := 700;
	tune[4].period := 500;
	tune[5].period := 800;

	tune[6].duration := 0;

	Play (tune, done);  Wait (done);
	OutTrace ("Test2");
    END Test2;

(************************************************************************)

PROCEDURE WaitAWhile;

    (* Does nothing, but does it slowly.	*)

    VAR j: CARDINAL;
	w: Window;

    BEGIN
	InTrace ("WaitAWhile");
	OpenWindow (w, yellow, green, 12, 14, 50, 69,
					simpleframe, nodivider);
	FOR j := 0 TO 5000 DO
	    SetCursor (w, 1, 1);
	    WriteCard (w, j);
	END (*FOR*);
	CloseWindow (w);
	OutTrace ("WaitAWhile");
    END WaitAWhile;

(************************************************************************)

BEGIN
    TraceOn (11, 24, 0, 79, 1);
    Test1;
    Test2;
    WaitAWhile;
END SoundTst.
