MODULE ORtest;

FROM Windows IMPORT
	Window, Colour, FrameType, DividerType,
	OpenWindow, CloseWindow, WriteString, WriteLn, PressAnyKey;

FROM NumericIO IMPORT
	WriteHexWord, WriteHexByte, WriteRJCard;

FROM LowLevel IMPORT
	IOR, IORB, IXOR, IXORB, LS;

(*******************************************************************)

VAR w: Window;

(*******************************************************************)

PROCEDURE TestIOR (x, y: CARDINAL);

    BEGIN
	WriteLn (w);
	WriteHexWord (w, x);  WriteString (w, " IOR ");
	WriteHexWord (w, y);  WriteString (w, " = ");
	WriteHexWord (w, IOR(x,y));
    END TestIOR;

(*******************************************************************)

PROCEDURE TestIXOR (x, y: CARDINAL);

    BEGIN
	WriteLn (w);
	WriteHexWord (w, x);  WriteString (w, " IXOR ");
	WriteHexWord (w, y);  WriteString (w, " = ");
	WriteHexWord (w, IXOR(x,y));
    END TestIXOR;

(*******************************************************************)

PROCEDURE TestIORB (x, y: SHORTCARD);

    BEGIN
	WriteLn (w);
	WriteHexByte (w, x);  WriteString (w, " IORB ");
	WriteHexByte (w, y);  WriteString (w, " = ");
	WriteHexByte (w, IORB(x,y));
    END TestIORB;

(*******************************************************************)

PROCEDURE LStest;

    VAR datum, j: CARDINAL;

    BEGIN
	datum := 1234;
	FOR j := 0 TO 16 DO
	    WriteLn (w);
	    WriteRJCard (w, j, 3);
	    WriteString (w, "   ");
	    WriteHexWord (w, LS(datum, j));
	END (*FOR*);
    END LStest;

(*******************************************************************)

BEGIN
    OpenWindow (w, blue, cyan, 0, 24, 0, 79, noframe, nodivider);
    WriteString (w, "Test of IOR");
    TestIOR (5, 19);
    TestIOR (055AAH, 01234H);
    TestIORB (5, 19);
    TestIORB (012H, 089H);
    TestIXOR (5, 19);
    TestIXOR (012H, 089H);
    PressAnyKey (w);
    LStest;
    PressAnyKey (w);
    CloseWindow (w);
END ORtest.
