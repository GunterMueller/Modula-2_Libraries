MODULE VidChk;

	(****************************************************************)
	(*								*)
	(*		    Test of PMOS graphics.			*)
	(*								*)
	(*	The purpose of this program is to list the graphics	*)
	(*	modes which we believe are supported by PMOS on		*)
	(*	the video adaptor in use when the program is run.	*)
	(*								*)
	(*  Programmer:		P. Moylan				*)
	(*  Last edited:	18 March 1995				*)
	(*  Status:		OK, still some room for improvement	*)
	(*								*)
	(****************************************************************)

FROM Types IMPORT
    (* proc *)	FarPointer, FarBytePointer;

FROM Storage1 IMPORT
    (* proc *)	ALLOCATE, DEALLOCATE;

FROM LowLevel IMPORT
    (* proc *)	OFFSET, SEGMENT, FarAddOffset;

FROM MiscPMOS IMPORT
    (* type *)	RegisterPacket,
    (* proc *)	BIOS;

FROM Screen IMPORT
    (* const*)	HercGraphics,
    (* type *)	VideoAdaptorType, ModeInfoType,
    (* proc *)	VideoKind, Supported, VESAdriverPresent, GetModeInfo;

FROM Graphics IMPORT
    (* type *)	ColourType,
    (* proc *)	SetMode, GraphicsOff, PlotLine,
		ClippedString,	(* for testing it *)
		PlotString, GetScreenShape, Fill;

FROM Conversions IMPORT
    (* proc *)	CardinalToString;

FROM IO IMPORT
    (* proc *)	RdKey;

FROM GlassTTY IMPORT
    (* proc *)	WriteChar, WriteString, WriteLn;

(************************************************************************)

CONST VideoInt = 16;

TYPE
    Mode = [0..511];
    ModeSet = SET OF Mode;

VAR
    (* The kind of display adaptor which is installed. *)

    AdaptorType: VideoAdaptorType;

    (* Maximum X and Y, and maximum colour. *)

    Xmax, Ymax: CARDINAL;
    MaxColour: ColourType;

(************************************************************************)

PROCEDURE Abort(): BOOLEAN;

    (* Waits until a key is struck on the keyboard, returns TRUE if	*)
    (* the key was the Escape key.					*)

    CONST Esc = CHR(01BH);

    BEGIN
	RETURN RdKey() = Esc;
    END Abort;

(************************************************************************)

PROCEDURE YesNo(): BOOLEAN;

    (* Waits for Y or N from keyboard; returns TRUE for Y, FALSE for N.	*)

    VAR ch: CHAR;

    BEGIN
	LOOP
	    ch := RdKey();
	    IF CAP(ch) = 'Y' THEN
		WriteChar ("Y");  RETURN TRUE;
	    ELSIF CAP(ch) = 'N' THEN
		WriteChar ("N");  RETURN FALSE;
	    END (*IF*);
	END (*LOOP*);
    END YesNo;

(************************************************************************)

PROCEDURE ColourStrip (x0, y0, width, height: CARDINAL;  c0, c1: ColourType);

    (* Displays a band, whose lower left is (x0,y0) and whose height	*)
    (* is "height", of the colour range c0..c1.  Each coloured block is	*)
    (* "width" pixels wide.						*)

    VAR c: ColourType;

    BEGIN
	FOR c := c0 TO c1 DO
	    Fill (x0, y0, x0+width-1, y0+height, c);
	    INC (x0, width);
	END (*FOR*);
    END ColourStrip;

(************************************************************************)

PROCEDURE AssembleTitle (mode: Mode;  VAR (*OUT*) Title: ARRAY OF CHAR)
								: CARDINAL;

    (* Creates a title string for the given mode.  The function result	*)
    (* is equal to the number of characters assembled.			*)

    VAR StringBuffer: ARRAY [0..4] OF CHAR;
	k: [0..4];  count: CARDINAL;

    BEGIN
	StringBuffer := "MODE ";
	FOR k := 0 TO 4 DO
	    Title[k] := StringBuffer[k];
	END (*FOR*);
	count := 5;
	CardinalToString (mode, StringBuffer, 3);
	FOR k := 0 TO 2 DO
	    IF StringBuffer[k] <> " " THEN
		Title[count] := StringBuffer[k];  INC (count);
	    END (*IF*);
	END (*FOR*);
	Title[count] := " ";  INC (count);
	Title[count] := " ";  INC (count);
	CardinalToString (Xmax+1, StringBuffer, 4);
	FOR k := 0 TO 3 DO
	    IF StringBuffer[k] <> " " THEN
		Title[count] := StringBuffer[k];  INC (count);
	    END (*IF*);
	END (*FOR*);
	Title[count] := "x";  INC (count);
	CardinalToString (Ymax+1, StringBuffer, 4);
	FOR k := 0 TO 3 DO
	    IF StringBuffer[k] <> " " THEN
		Title[count] := StringBuffer[k];  INC (count);
	    END (*IF*);
	END (*FOR*);
	Title[count] := "x";  INC (count);
	IF MaxColour = MAX(CARDINAL) THEN
	    StringBuffer := "65536";
	ELSE
	    CardinalToString (MaxColour+1, StringBuffer, 5);
	END (*IF*);
	FOR k := 0 TO 4 DO
	    IF StringBuffer[k] <> " " THEN
		Title[count] := StringBuffer[k];  INC (count);
	    END (*IF*);
	END (*FOR*);
	RETURN count;
    END AssembleTitle;

(************************************************************************)

PROCEDURE TestMode (m: Mode): BOOLEAN;

    (* Displays a test pattern for mode m, waiting for the user to	*)
    (* press a key before returning.  Returns TRUE if the key was the	*)
    (* Esc key.								*)

    VAR count: CARDINAL;  textcolour, c: ColourType;
	Title: ARRAY [0..25] OF CHAR;
	x0, x1, y0, y1, xgap, ygap, width, height, CharHeight: CARDINAL;

	left, right, ymin, ymax: CARDINAL;	(* for testing ClippedString *)

    BEGIN
	(* Set the mode, and pick a display colour. *)

	SetMode (m, TRUE);
	GetScreenShape (Xmax, Ymax, MaxColour, CharHeight);
	IF MaxColour >= 255 THEN textcolour := 3;
	ELSE textcolour := MaxColour;
	END (*IF*);

	(* Put together a descriptive caption. *)

	count := AssembleTitle (m, Title);

	(* Put a band of colours across part of the screen. *)

	IF MaxColour < Xmax THEN
	    x0 := (Xmax+1) MOD (MaxColour+1) DIV 2;
	    y0 := 3*Ymax DIV 4;
	    width := (Xmax+1) DIV (MaxColour+1);
	    height := Ymax DIV 8;
	    ColourStrip (x0, y0, width, height, 0, MaxColour);
	    IF width > 5 THEN
		(* Put a frame around the black section. *)
		PlotLine (x0,y0,x0+width-1,y0,1);
		PlotLine (x0,y0+height,x0+width-1,y0+height,1);
		PlotLine (x0,y0,x0,y0+height,1);
	    END (*IF*);

	END (*IF*);

	(* Write the caption in the centre of the screen. *)

	x0 := (Xmax - 8*count) DIV 2;
	y0 := (Ymax - CharHeight) DIV 2;
	PlotString (Title, x0, y0, count, textcolour);

	(* If testing ClippedString: write the caption again, clipped. *)

(*
	INC (y0, 15);
	left := x0;
	right := x0 + 8*count - 3;
	ymin := y0;
	ymax := y0 + CharHeight - 1;
	ClippedString (Title, x0, y0, count, textcolour,
				left, right, ymin, ymax);
*)
	RETURN Abort();

    END TestMode;

(************************************************************************)

PROCEDURE TestPatterns (S: ModeSet);

    (* Displays a test pattern for every mode in S, waiting for the	*)
    (* user to press a key in between tests.				*)

    VAR j: Mode;

    BEGIN
	WriteString ("Press any key to start each test.");
	IF NOT Abort() THEN
	    j := 0;
	    LOOP
		IF j IN S THEN
		    IF TestMode (j) THEN EXIT(*LOOP*) END(*IF*);
		END (*IF*);
		IF j = MAX(Mode) THEN EXIT(*LOOP*) END(*IF*);
		INC (j);
	    END (*LOOP*);
	END (*IF*);
    END TestPatterns;

(************************************************************************)

PROCEDURE ShowExtraModes;

    (* Tells which SVGA modes are supported, and optionally displays a	*)
    (* test pattern for each of them.					*)

    VAR mode: Mode;  count: CARDINAL;  TextMode: BOOLEAN;
	Title: ARRAY [0..26] OF CHAR;
	ExtraModes: ModeSet;
	info: ModeInfoType;

    BEGIN
	WriteString ("Your hardware appears to support the following ");
	WriteString ("SVGA modes.");
	WriteLn;
	WriteString ("Of these, PMOS will support the graphics modes ");
	WriteString ("with up to 65536 colours.");
	WriteLn;  WriteLn;
	ExtraModes := ModeSet {};
	FOR mode := 20 TO MAX(Mode) DO
	    IF Supported (mode) THEN
		GetModeInfo (mode, info);
		Xmax := info.MaxX;
		Ymax := info.MaxY;
		MaxColour := info.MaxColour;
		TextMode := info.TextMode;

		IF NOT TextMode THEN
		    ExtraModes := ExtraModes + ModeSet {mode};
		END (*IF*);
		count := AssembleTitle (mode, Title);
		Title[count] := CHR(0);
		WriteString ("        ");  WriteString (Title);
		IF TextMode THEN
		    WriteString ("  text");
		END (*IF*);
		WriteLn;
	    END (*IF*);
	END (*FOR*);
	IF ExtraModes = ModeSet {} THEN
	    WriteString ("(No extra modes found)");
	    WriteLn;
	ELSE
	    WriteLn;
	    WriteString ("Do you want to display test patterns for these modes?");
	    WriteString (" [Y/N] ");
	    IF YesNo() THEN
		WriteLn;
		TestPatterns (ExtraModes);
	    END (*IF*);
	    GraphicsOff (TRUE);
	END (*IF*);
	WriteLn;
    END ShowExtraModes;

(************************************************************************)

PROCEDURE ShowStandardModes;

    (* Tells which standard (up to VGA) modes are supported, and	*)
    (* optionally displays a test pattern for each of them.		*)

    VAR StandardModes: ModeSet;

    BEGIN
	WriteString ("Under PMOS you should have the following ");
	WriteString ("standard graphics modes available.");
	WriteLn;  WriteLn;
	StandardModes := ModeSet {};
	IF AdaptorType = MDA THEN
	    WriteString ("No graphics modes are supported.  Sorry.");
	    WriteLn;
	ELSIF AdaptorType = Hercules THEN
	    WriteString ("   720 x 348 monochrome");  WriteLn;
	    StandardModes := ModeSet {HercGraphics};
	ELSE
	    IF AdaptorType >= CGA THEN
		WriteString ("      4     320 x 200 x   4");  WriteLn;
		WriteString ("      5     320 x 200 monochrome");  WriteLn;
		WriteString ("      6     640 x 200 monochrome");  WriteLn;
		StandardModes := StandardModes + ModeSet {4..6};
	    END (*IF*);
	    IF AdaptorType >= EGA THEN
		WriteString ("     13     320 x 200 x  16");  WriteLn;
		WriteString ("     14     640 x 200 x  16");  WriteLn;
		WriteString ("     15     640 x 350 monochrome");  WriteLn;
		WriteString ("     16     640 x 350 x  16");  WriteLn;
		StandardModes := StandardModes + ModeSet {13..16};
	    END (*IF*);
	    IF AdaptorType >= VGA THEN
		WriteString ("     17     640 x 480 monochrome");  WriteLn;
		WriteString ("     18     640 x 480 x  16");  WriteLn;
		WriteString ("     19     320 x 200 x 256");  WriteLn;
		StandardModes := StandardModes + ModeSet {17..19};
	    END (*IF*);
	    IF AdaptorType > VGA THEN
		WriteLn;
		WriteString ("(This does not include the SVGA modes, ");
		WriteString ("which will be listed later.)");
		WriteLn;
	    END (*IF*);
	END (*IF*);
	WriteLn;
	WriteString ("Do you want to display test patterns for these modes?");
	WriteString (" [Y/N] ");
	IF YesNo() THEN
	    WriteLn;
	    TestPatterns (StandardModes);
	END (*IF*);
	GraphicsOff (TRUE);
	WriteLn;
    END ShowStandardModes;

(************************************************************************)

PROCEDURE ReportOEMString;

    (* Writes the OEM string from the VESA driver.	*)

    TYPE CharPtr = FarBytePointer;

    VAR Registers: RegisterPacket;
	SysInfoPtr: POINTER TO
			    RECORD
				VesaSignature: ARRAY [0..3] OF CHAR;
				VesaVersion: WORD;
				OEM_OffsetString: CharPtr;
				capabilities: LONGWORD;
				ModeListPointer: FarPointer;
				NumberOf64KBlocks: CARDINAL;
				reserved: ARRAY [20..255] OF BYTE;
			    END (*RECORD*);
	pch: CharPtr;

    (********************************************************************)

    BEGIN
	(* Call the VESA BIOS subfunction 0, which is supposed to	*)
	(* return system information.					*)

	NEW(SysInfoPtr);
	WITH Registers DO
	    AX := 4F00H;
	    DI := OFFSET(SysInfoPtr);
	    ES := SEGMENT(SysInfoPtr);
	    BIOS(VideoInt,Registers);
	END (*WITH*);

	WriteLn;
	WriteString ("The manufacturer identification from the");
	WriteString (" VESA driver is:");
	WriteLn;
	pch := SysInfoPtr^.OEM_OffsetString;
	WHILE pch^ <> BYTE(0) DO
	    WriteChar (pch^);
	    pch := FarAddOffset (pch, 1);
	END (*WHILE*);
	WriteLn;
	DISPOSE (SysInfoPtr);

    END ReportOEMString;

(************************************************************************)

PROCEDURE OpeningMessage;

    (* Explains the purpose of the program, reports display adaptor	*)
    (* type, and writes the OEM string from the VESA driver if present.	*)

    BEGIN
	GraphicsOff (TRUE);
	WriteString ("This program shows which graphics modes ");
	WriteString ("PMOS supports on your display adaptor.");
	WriteLn;
	WriteString ("PMOS classifies your adaptor as being of type: ");
	CASE AdaptorType OF
		MDA:	WriteString ("MDA");
	  |
		Hercules: WriteString ("Hercules");
	  |
		CGA:	WriteString ("CGA");
	  |
		EGA:	WriteString ("EGA");
	  |
		VGA:	WriteString ("VGA");
	  |
		SVGA:	WriteString ("SVGA");
	  |
		ATI:	WriteString ("ATI");
	  |
		S3:	WriteString ("S3");
	  |
		Trident: WriteString ("Trident");
	END (*CASE*);
	WriteLn;
	IF VESAdriverPresent() THEN
	    ReportOEMString;
	ELSE
	    WriteLn;
	    WriteString ("No VESA driver was detected.");
	    WriteLn;
	END (*IF*);
	WriteLn;
    END OpeningMessage;

(************************************************************************)
(*				MAIN PROGRAM				*)
(************************************************************************)

VAR dummy: BOOLEAN;

BEGIN
    AdaptorType := VideoKind ();
    OpeningMessage;
    ShowStandardModes;
    IF AdaptorType > VGA THEN
	ShowExtraModes;
    END (*IF*);
    WriteString ("End of tests.");
    WriteLn;
    WriteString ("Press any key to exit.");
    WriteLn;
    dummy := Abort();
    GraphicsOff (TRUE);
END VidChk.
