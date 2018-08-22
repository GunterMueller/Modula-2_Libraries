MODULE RealTest;

	(********************************************************)
	(*							*)
	(*		Test of real I/O using windows.		*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	10 October 1992			*)
	(*  Status:						*)
	(*	Was working with FTL - see below - but still	*)
	(*	bugs in the TopSpeed version, possibly to do	*)
	(*	with LongTrunc but maybe because of an E	*)
	(*	format problem.					*)
	(*							*)
	(*	Working, still running various tests.		*)
	(*	I have noticed the following faults:		*)
	(*	1. Precision error: 123.456E7 is converted	*)
	(*	   to 1234599...				*)
	(*	2. 0.1E-8 is written as 0E-8 when the field	*)
	(*	   size is 4 or 5.  I haven't yet decided	*)
	(*	   whether to call this an error.		*)
	(*							*)
	(********************************************************)

FROM Trace IMPORT
    (* proc *)	NYI, Pause, TraceOn;

FROM Conversions IMPORT
    (* proc *)	StringToLongReal;

FROM RealIO IMPORT
    (* proc *)	WriteLongReal, ReadLongReal;

FROM Windows IMPORT
    (* type *)	Window, Colour, FrameType, DividerType,
    (* proc *)	OpenWindow, CloseWindow, WriteLn, WriteString;

(********************************************************************)

PROCEDURE StringTest (w: Window);

    (* Check that procedure StringToReal works.	*)

    PROCEDURE Test (TestString: ARRAY OF CHAR);

	VAR value: LONGREAL;

	BEGIN
	    value := StringToLongReal (TestString);
	    WriteString (w, TestString);  WriteString (w, " = ");
	    WriteLongReal (w, value, 10);  WriteLn (w);
	END Test;

    BEGIN
	WriteString (w, "Now testing StringToLongReal.");  WriteLn (w);
	Test ("123.456");
	Test ("50000000000");
	Test ("0.00009999");
	Test ("   .00009999");
	Test ("   .000099999");
	Test ("000.001");
	Test ("0");
	Test ("123");
	Test ("123.456E7");
	Test ("123.456E-7");
    END StringTest;

(********************************************************************)

PROCEDURE InputTest (w: Window);

    VAR result: LONGREAL;

    BEGIN
	WriteString (w, "Now testing ReadLongReal.");  WriteLn (w);
	REPEAT
	    result := ReadLongReal (w);  WriteString (w, " = ");
	    WriteLongReal (w, result, 10);  WriteLn (w);
	UNTIL result = 0.0;
	Pause;
    END InputTest;

(********************************************************************)

PROCEDURE OutputTest (w: Window);

    VAR	x: LONGREAL;
	j: CARDINAL;

    BEGIN
	WriteString (w, "   0.0 = ");  WriteLongReal (w, 0.0, 8);  WriteLn(w);
	WriteString (w, "   1.0 = ");  WriteLongReal (w, 1.0, 8);  WriteLn(w);
	WriteString (w, "  10.0 = ");  WriteLongReal (w, 10.0, 8);  WriteLn(w);
	WriteString (w, " 1.005 = ");  WriteLongReal (w, 1.005, 8);  WriteLn(w);
	WriteString (w, "1.0052 = ");  WriteLongReal (w, 1.0052, 8);  WriteLn(w);
	Pause;
	WriteString (w, "1.1 = ");  WriteLongReal (w, 1.1, 8);  WriteLn(w);
	WriteString (w, "1.5 = ");  WriteLongReal (w, 1.5, 8);  WriteLn(w);

	Pause;

	x := 3.14159265;
	FOR j := 12 TO 1 BY -1 DO
	    WriteLongReal (w, x, j);  WriteLn(w);  Pause;
	END (*FOR*);

	FOR j := 14 TO 1 BY -1 DO
	    WriteLongReal (w, 12345678.0, j);
	    WriteLn (w);
	    Pause;
	    WriteLongReal (w, 0.0000000012345, j);  WriteLn (w);
	    Pause;
	END (*FOR*);
    END OutputTest;

(********************************************************************)

PROCEDURE RunTheTest;

    VAR w: Window;

    BEGIN
	OpenWindow (w, black, white, 10, 20, 0, 40, simpleframe, nodivider);
	OutputTest (w);
	StringTest (w);
	InputTest (w);
	Pause;
	CloseWindow (w);

    END RunTheTest;

(********************************************************************)

BEGIN
 (*   TraceOn (0, 10, 5, 40);  *)
    RunTheTest;
END RealTest.
