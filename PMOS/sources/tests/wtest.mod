MODULE WTest;

	(************************************************)
	(*						*)
	(*	Test of window operations.		*)
	(*						*)
	(*  Programmer:		P. Moylan		*)
	(*  Last edited:	7 November 1993		*)
	(*  Status:		Working			*)
	(*						*)
	(************************************************)

FROM Trace IMPORT
    (* proc *)	TraceOff, TraceOn, InTrace, OutTrace, Pause;

FROM Windows IMPORT
    (* type *)	Window, FrameType, DividerType, Colour,
    (* proc *)	OpenWindow, CloseWindow, ChangeScrollingRegion,
		WriteChar, WriteString, WriteLn;

FROM Bounce IMPORT
    (* proc *)	Bouncing;

(****************************************************************)

PROCEDURE increment (VAR (* inout *) ch: CHAR);

    BEGIN
	InTrace ("increment");
	IF ch = CHR(255) THEN
	    ch := " ";
	ELSE
	    INC (ch);
	END (*IF*);
	OutTrace ("increment");
    END increment;

(****************************************************************)

PROCEDURE RunTheTest;

    VAR w1, w2, w3: Window;
	ch: CHAR;
	i, j, loopcount: CARDINAL;

    BEGIN
	InTrace ("RunTheTest");
	Bouncing (0, 11, 41, 79);
	Pause;
	OpenWindow (w1, blue, cyan, 6, 16, 0, 40, simpleframe, nodivider);
	WriteString (w1, "Test Message, window w1");
	Pause;
	OpenWindow (w2, white, magenta, 12, 22, 20, 63, doubleframe, nodivider);
	WriteString (w2, "Test Message, window w2");
	OpenWindow (w3, brown, black, 13, 24, 45, 75,
			simpleframe, doubledivider);
	WriteString (w3, "This one should have");  WriteLn (w3);
	WriteString (w3, " a non-scrolling header.");
	ChangeScrollingRegion (w3, 4, 10);
	(*Pause*);
	ch := " ";  loopcount := 0;
	FOR i := 1 TO 40 DO
	    FOR j := 1 TO 40 DO
		WriteChar (w1, ch);  increment (ch);
	    END (* for *);
	    FOR j := 1 TO 40 DO
		WriteChar (w2, ch);  increment (ch);
	    END (* for *);
	    INC (loopcount);
	    IF loopcount = 10 THEN
		WriteLn (w3);
		WriteString (w3, " and footer");
		ChangeScrollingRegion (w3, 4, 8);
	    ELSIF loopcount = 20 THEN
		WriteLn (w3);
		ChangeScrollingRegion (w3, 1, 8);
	    ELSIF loopcount = 30 THEN
		WriteLn (w3);
		ChangeScrollingRegion (w3, 4, 10);
		loopcount := 0;
	    END (* if *);
	    FOR j := 1 TO 40 DO
		WriteChar (w3, ch);  increment (ch);
	    END (* for *);
	    (*Pause*);
	END (* for *);
	(*DumpWindow (w2)*);
	Pause;  CloseWindow (w2);
	Pause;  CloseWindow (w3);
	(*Pause*);  CloseWindow (w1);
	OutTrace ("RunTheTest");
    END RunTheTest;

(****************************************************************)

BEGIN
    TraceOn (0, 4, 10, 40, 1);
    RunTheTest;
    TraceOff;
END WTest.
