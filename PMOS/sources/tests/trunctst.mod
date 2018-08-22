MODULE TruncTst;

	(********************************************************)
	(*							*)
	(*		Test of suspected bug in TRUNC		*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	21 March 1993			*)
	(*  Status:		Working				*)
	(*							*)
	(*	The tests confirm that TRUNC rounds rather	*)
	(*	than truncates.					*)
	(*							*)
	(********************************************************)

FROM BugFix IMPORT
    (* proc *)	Trunc, LongTrunc;

FROM Windows IMPORT
    (* type *)	Window, Colour, DividerType, FrameType,
    (* proc *)	OpenWindow, CloseWindow, WriteString, WriteLn,
		EraseLine, PressAnyKey;

FROM NumericIO IMPORT
    (* proc *)	WriteRJCard, WriteRJLongCard, WriteHexByte, WriteHexWord;

FROM RealIO IMPORT
    (* proc *)	ReadReal, ReadLongReal, WriteReal;

IMPORT Str;

(************************************************************************)

TYPE Word2 = ARRAY[0..1] OF WORD;
TYPE Word4 = ARRAY[0..3] OF WORD;

(************************************************************************)

PROCEDURE WriteLongReal (w: Window;  number: LONGREAL;  places: CARDINAL);

    (* To bypass possible bug in module Conversions.	*)

    VAR strbuff: ARRAY [0..20] OF CHAR;  OK: BOOLEAN;

    BEGIN
	Str.RealToStr (number, places, FALSE, strbuff, OK);
	WriteString (w, strbuff);
    END WriteLongReal;

(************************************************************************)

PROCEDURE RealTest;

    CONST half = REAL(Word2 (0, 03F00H));

    VAR w: Window;  x: REAL;

    BEGIN
	OpenWindow (w, cyan, blue, 0, 24, 0, 79, noframe, nodivider);
	WriteString (w, "       x            CARDINAL(x)      TRUNC(x)     Trunc(x)   VAL(CARDINAL,x-.5)");
	LOOP
	    WriteLn (w);  WriteString (w, "x = ? ");
	    x := ReadReal (w);  EraseLine (w, 0);
	    IF x < half THEN EXIT(*LOOP*) END(*IF*);
	    WriteLongReal (w, LONGREAL(x), 15);
	    WriteRJCard (w, CARDINAL(x), 15);
	    WriteRJCard (w, TRUNC(x), 15);
	    WriteRJCard (w, Trunc(x), 15);
	    WriteRJCard (w, VAL(CARDINAL,x-half), 15);
	END (*LOOP*);
	PressAnyKey (w);
	CloseWindow(w);
    END RealTest;

(************************************************************************)

PROCEDURE LongTest;

    CONST half = LONGREAL(Word4(0, 0, 0, 3FE0H));

    VAR w: Window;  x: LONGREAL;

    BEGIN
	OpenWindow (w, cyan, blue, 0, 24, 0, 79, noframe, nodivider);
	WriteString (w, "       x            LONGCARD(x)      LongTrunc(x)   VAL(LONGCARD,x-0.5)");
	LOOP
	    WriteLn (w);  WriteString (w, "x = ? ");
	    x := ReadLongReal (w);  EraseLine (w, 0);
	    IF x < half THEN EXIT(*LOOP*) END(*IF*);
	    WriteLongReal (w, x, 15);
	    WriteRJLongCard (w, LONGCARD(x), 15);
	    WriteRJLongCard (w, LongTrunc(x), 15);
	    WriteRJLongCard (w, VAL(LONGCARD,x-half), 15);
	END (*LOOP*);
	PressAnyKey (w);
	CloseWindow(w);
    END LongTest;

(************************************************************************)

PROCEDURE HalfTest;

    CONST half1 = Word2 (0FFFFH, 03EFFH);
	half2 = Word4 (0FFFFH, 0FFFFH, 0FFFFH, 3FDFH);
	Rhalf = REAL(Word2 (0FFFFH, 03EFFH));
	Lhalf = LONGREAL(Word4(0FFFFH, 0FFFFH, 0FFFFH, 3FDFH));

    VAR w: Window;  j: CARDINAL;

    BEGIN
	OpenWindow (w, cyan, blue, 0, 24, 0, 79, noframe, nodivider);
	FOR j := 1 TO 0 BY -1 DO
	    WriteHexWord (w, half1[j]);
	END (*FOR*);
	WriteString (w, "    ");
	WriteLongReal (w, LONGREAL(Rhalf), 15);

	WriteLn (w);
	FOR j := 3 TO 0 BY -1 DO
	    WriteHexWord (w, half2[j]);
	END (*FOR*);
	WriteString (w, "    ");
	WriteLongReal (w, Lhalf, 15);
	PressAnyKey (w);
	CloseWindow(w);
    END HalfTest;

(************************************************************************)
(*				MAIN PROGRAM				*)
(************************************************************************)

BEGIN
    HalfTest;
    RealTest;
    LongTest;
END TruncTst.
