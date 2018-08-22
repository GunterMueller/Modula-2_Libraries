MODULE WTest2;

	(********************************************************)
	(*							*)
	(*		Test of window operations.		*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	17 February 1993		*)
	(*  Status:		Working				*)
	(*							*)
	(*	This was an attempt to duplicate the display	*)
	(*	corruption observed in the test programs	*)
	(*	DiskPatch and CopyFile.  The problem has now	*)
	(*	been repaired.					*)
	(*							*)
	(********************************************************)

IMPORT Floppy;

IMPORT HardDisk;

FROM Trace IMPORT
    (* proc *)	TraceOff, TraceOn, InTrace, OutTrace, Pause;

FROM Windows IMPORT
    (* type *)	Window, FrameType, DividerType, Colour,
    (* proc *)	OpenWindow, CloseWindow, ChangeScrollingRegion,
		WriteChar, WriteString, WriteLn, ScrollUp;

(****************************************************************)

PROCEDURE increment (VAR (* inout *) ch: CHAR);

    VAR j: CARDINAL;

    BEGIN
	InTrace ("increment");
	IF ch = CHR(255) THEN
	    ch := " ";
	ELSE
	    INC (ch);
	END (* if *);
	FOR j := 0 TO 10000 DO (*nothing*) END(*FOR*);
	OutTrace ("increment");
    END increment;

(****************************************************************)

PROCEDURE RunTheTest;

    VAR w2: Window;
	ch: CHAR;
	i, j, loopcount: CARDINAL;

    BEGIN
	InTrace ("RunTheTest");
	OpenWindow (w2, white, magenta, 0, 20, 0, 79, simpleframe, doubledivider);
	WriteString (w2, "Test Message, window w2");
	ChangeScrollingRegion (w2, 3, 19);
	(*Pause*);
	ch := " ";  loopcount := 0;
	FOR i := 1 TO 60 DO
	    FOR j := 1 TO 40 DO
		WriteChar (w2, ch);  increment (ch);
	    END (* for *);
	    INC (loopcount);
	END (* for *);
	ScrollUp (w2);  ScrollUp (w2);  ScrollUp (w2);
	(*DumpWindow (w2)*);
	Pause;  CloseWindow (w2);
	OutTrace ("RunTheTest");
    END RunTheTest;

(****************************************************************)

BEGIN
    (*TraceOn (0, 4, 10, 40, 1);*)
    RunTheTest;
    TraceOff;
END WTest2.
