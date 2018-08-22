MODULE Graftst1;

	(****************************************************************)
	(*								*)
	(*		Test of Graphics module.			*)
	(*								*)
	(*	NOTE: This program is not intended as a "final		*)
	(*	product" whose code is static.  It's a collection	*)
	(*	of small tests which can be commented or uncommented	*)
	(*	as needed - or modified, as needed - depending on	*)
	(*	what aspect of the screen graphics is being tested	*)
	(*	at the time.						*)
	(*								*)
	(*  Programmer:		P. Moylan				*)
	(*  Last edited:	18 March 1995				*)
	(*  Status:		OK					*)
	(*	Can draw points, lines, and characters for the three	*)
	(*	CGA modes, EGA modes 13-16, Hercules monochrome, VGA	*)
	(*	modes 17-19, and all of the SVGA modes which I have	*)
	(*	been able to test (except for the modes with 24-bit	*)
	(*	colour; I haven't yet thought of a fast way to support	*)
	(*	24-bit modes.)						*)
	(*								*)
	(* For mode definitions and support status, see Graphics.DEF.	*)
	(*								*)
	(****************************************************************)

FROM ScreenGeometry IMPORT
    (* type *)	Rectangle;

FROM Graphics IMPORT
    (* proc *)	SetMode, SetDefaultMode, PlotDot, PlotLine, PlotRectangle,
		DrawChar, PlotString, GetScreenShape, Fill, ACopy;

FROM Conversions IMPORT
    (* proc *)	CardinalToString;

FROM IO IMPORT
    (* proc *)	RdKey;

IMPORT GlassTTY;

(************************************************************************)

VAR Xmax, Ymax, MaxColour: CARDINAL;

(************************************************************************)

PROCEDURE Pause;

    (* Waits until a key is struck on the keyboard.	*)

    VAR dummy: CHAR;

    BEGIN
	dummy := RdKey();
    END Pause;

(************************************************************************)

PROCEDURE CentreCross (x, y, colour: CARDINAL);

    (* Plots a cross at position (x,y), using PlotLine.	*)

    BEGIN
	PlotLine (x-5,y,x+5,y,colour);
	PlotLine (x,y-5,x,y+5,colour);
    END CentreCross;

(************************************************************************)

PROCEDURE ShortLines (y, colour: CARDINAL);

    (* Plots some short lines, using point graphics.	*)

    VAR j: CARDINAL;

    BEGIN

	FOR j := 0 TO 7 DO
	    PlotDot (0,y-j,colour);
	END (*FOR*);
	FOR j := 0 TO 15 DO
	    PlotDot (10,y-j,colour);
	END (*FOR*);
	FOR j := 0 TO 31 DO
	    PlotDot (20,y-j,colour);
	END (*FOR*);
	FOR j := 0 TO 63 DO
	    PlotDot (30,y-j,colour);
	END (*FOR*);
	FOR j := 0 TO 127 DO
	    PlotDot (40,y-j,colour);
	END (*FOR*);
(*
	FOR j := 0 TO 255 DO
	    PlotDot (50,y-j,colour);
	END (*FOR*);
	FOR j := 0 TO 479 DO
	    PlotDot (60,y-j,colour);
	END (*FOR*);
*)
	FOR j := 0 TO 319 DO
	    PlotDot (j,y,colour);
	END (*FOR*);
    END ShortLines;

(************************************************************************)

PROCEDURE DiagonalLine (colour: CARDINAL);

    (* Plots a diagonal line, using point graphics.	*)

    VAR j: CARDINAL;

    BEGIN
	FOR j := 0 TO 99 DO
	    PlotDot (2*j,j,colour);
	END (*FOR*);
    END DiagonalLine;

(************************************************************************)

PROCEDURE PointTest (colour: CARDINAL);

    (* Plots a diagonal line, waits for a key to be struck, and then	*)
    (* erases the line.  This test provides a quick verification	*)
    (* that something really does appear on the screen, i.e. that a	*)
    (* suitable mode is set and that PlotDot works.			*)

    VAR j: CARDINAL;

    BEGIN
	DiagonalLine (colour);
	Pause;
	DiagonalLine (0);
    END PointTest;

(************************************************************************)

PROCEDURE Diamond (colour: CARDINAL);

    (* Plots a diamond shape, using PlotLine.	*)

    BEGIN
	PlotLine (15,Ymax DIV 2,Xmax DIV 2,15,colour);
	PlotLine (Xmax DIV 2,15,Xmax-15,Ymax DIV 2,colour);
	PlotLine (Xmax-15,Ymax DIV 2,Xmax DIV 2,Ymax-15,colour);
	PlotLine (Xmax DIV 2,Ymax-15,15,Ymax DIV 2,colour);
    END Diamond;

(************************************************************************)

PROCEDURE LineTest (colour: CARDINAL);

    (* Plots a diamond shape, then erases it.  This is a test that	*)
    (* PlotLine works.							*)

    BEGIN
	Diamond (colour);
	Pause;
	Diamond (0);
    END LineTest;

(************************************************************************)

PROCEDURE TextTest (colour: CARDINAL);

    (* Checks whether DrawChar works.	*)

    BEGIN
	DrawChar ("F", 100, 50, colour);
	Pause;
    END TextTest;

(************************************************************************)

PROCEDURE FillTest (colour: CARDINAL);

    (* Checks whether Fill works.	*)

    BEGIN
	Fill (Xmax DIV 2 - 15, Ymax DIV 2 - 15,
		Xmax DIV 2 + 15, Ymax DIV 2 + 15, colour);
    END FillTest;

(************************************************************************)

PROCEDURE CopyTest;

    (* Fills a region, and then copies most of it. *)

    CONST
	colour1 = 2;  colour2 = 1;
	width = 31;  height = 25;
	dx = 0;  dy = 33;

    VAR R: Rectangle;
	x0, y0, x1, y1: CARDINAL;

    BEGIN
	x0 := 7*Xmax DIV 8;  y0 := Ymax DIV 8;
	x1 := x0+width-1;  y1 := y0+height-1;
	Fill (x0+1, y0+1, x1-1, y1-1, colour1);
	Fill (x0+5, y0+5, x1-5, y1-5, colour2);
	WITH R DO
	    left := x0;  right := x1;
	    bottom := y0;  top := y1;
	END (*WITH*);
	PlotRectangle (R, colour2);
	WITH R DO
	    DEC(left,2);  INC(right,2);
	    DEC(bottom,2);  INC(top,2);
	END (*WITH*);
	PlotRectangle (R, colour2);
	(*Pause;*)
	ACopy (x0, y1, width, height, dx, dy);
	(*Pause;*)
    END CopyTest;

(************************************************************************)

(************************************************************************)

PROCEDURE ColourDisplay;

    (* This is useful mainly for the many-colour modes, to give a feel	*)
    (* for what the colour range is.					*)

    VAR x, y, c: CARDINAL;

    BEGIN
	x := 0;  y := Ymax;
	FOR c := 0 TO MaxColour DO
	    PlotDot (x, y, c);
	    IF x < Xmax THEN INC(x);
	    ELSE
		x := 0;  DEC (y);
	    END (*IF*);
	END (*FOR*);
	Pause;
    END ColourDisplay;

(************************************************************************)

PROCEDURE Title (mode, messx, messy, colour: CARDINAL);

    (* Writes a title message for the given mode.  The last three	*)
    (* parameters say where on the screen to put the message and what	*)
    (* colour to write it in.						*)

    VAR Message: ARRAY [0..25] OF CHAR;
	StringBuffer: ARRAY [0..4] OF CHAR;
	k: [0..4];  count: [0..26];

    BEGIN
	Message := "MODE xxx   XXXXxYYYYxCCCCC";
	count := 5;
	CardinalToString (mode, StringBuffer, 3);
	FOR k := 0 TO 2 DO
	    IF StringBuffer[k] <> " " THEN
		Message[count] := StringBuffer[k];  INC (count);
	    END (*IF*);
	END (*FOR*);
	Message[count] := " ";  INC (count);
	Message[count] := " ";  INC (count);
	CardinalToString (Xmax+1, StringBuffer, 4);
	FOR k := 0 TO 3 DO
	    IF StringBuffer[k] <> " " THEN
		Message[count] := StringBuffer[k];  INC (count);
	    END (*IF*);
	END (*FOR*);
	Message[count] := "x";  INC (count);
	CardinalToString (Ymax+1, StringBuffer, 4);
	FOR k := 0 TO 3 DO
	    IF StringBuffer[k] <> " " THEN
		Message[count] := StringBuffer[k];  INC (count);
	    END (*IF*);
	END (*FOR*);
	Message[count] := "x";  INC (count);
	IF MaxColour = MAX(CARDINAL) THEN
	    StringBuffer := "65536";
	ELSE
	    CardinalToString (MaxColour+1, StringBuffer, 5);
	END (*IF*);
	FOR k := 0 TO 4 DO
	    IF StringBuffer[k] <> " " THEN
		Message[count] := StringBuffer[k];  INC (count);
	    END (*IF*);
	END (*FOR*);
	PlotString (Message, messx, messy, count, colour);
    END Title;

(************************************************************************)

PROCEDURE RunTheTest;

    (* Runs us through a sequence of tests.	*)

    TYPE ModeSet = SET OF [0..511];

    CONST
	NonGraphicsModes = ModeSet {0..3, 7..12, 264..268};
	BasicModes = ModeSet {4..6, 13..19};
	WorkingVesaModes = ModeSet {256, 257, 259, 272, 273, 275, 276};
	WorkingModes = BasicModes + WorkingVesaModes;
	BadSynchOnTrident = ModeSet {106,258,260};
	ModesWhichMyTridentWontSupport = ModeSet {261,269,270,278,279,281,
						282,354};
	ExtraModes = ModeSet {368,369};

	TridentVesaModes = WorkingVesaModes + BadSynchOnTrident + ExtraModes;
	TridentModes = BasicModes + TridentVesaModes;

	ATIVesaModes = ModeSet {106, 256..259, 260};
	ATIExtraModes = ModeSet {83..85, 97..99};
	ATIFailingModes = ModeSet {99, 259};
	ATIModes = BasicModes + ATIExtraModes + ATIVesaModes;

	S3VesaModes = ModeSet {257..261, 272..274};
	S3OK = ModeSet {257};

	(* The list below shows modes which have been dropped from the	*)
	(* tests because they have been found to duplicate the function	*)
	(* of modes already in one of the lists above.  (This is for	*)
	(* my Trident adaptor.)						*)

	Duplicates = ModeSet {348..351, 362, 372..375};

    VAR j, colour, CharHeight: CARDINAL;

    BEGIN
	(*FOR j := 0 TO 511 DO*)
	FOR j := 85 TO 85 DO
	    (* IF j IN ModesToTest + ExtraModes THEN *)
	    (* IF j IN S3VesaModes-S3OK THEN *)
	    IF j IN ATIModes (* - BasicModes*) - ATIFailingModes THEN
(*
		SetMode (3);
		GlassTTY.WriteLn;
		GlassTTY.WriteString ("Now about to test mode ");
		GlassTTY.WriteCard (j);
		Pause;
*)
		SetMode (j, TRUE);
		GetScreenShape (Xmax, Ymax, MaxColour, CharHeight);
		IF MaxColour = 1 THEN colour := 1;
		ELSIF MaxColour > 255 THEN colour := 3;
		ELSE colour := 2;
		END (*IF*);
		Title (j, 30, 5, colour);
		Title (j, Xmax-210, Ymax-15, colour);
		ShortLines (Ymax, colour);
		FillTest (colour);
		CentreCross (Xmax DIV 2, Ymax DIV 2, colour-1);
		PointTest (colour);
		CopyTest;
		LineTest (colour);
		(* TextTest (colour); *)
		(* ColourDisplay; *)
	    END (*IF*);
	END (*FOR*);
    END RunTheTest;

(************************************************************************)
(*				MAIN PROGRAM				*)
(************************************************************************)

BEGIN
    RunTheTest;
END Graftst1.
