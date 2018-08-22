MODULE DiskPatch;

	(********************************************************)
	(*							*)
	(*		Low-level disk utility.			*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	5 March 1995			*)
	(*  Status:		Working				*)
	(*							*)
	(*	Ultimately, this could become a utility for	*)
	(*	checking and patching the contents of floppy	*)
	(*	OR hard disks.  For now, it is just a test	*)
	(*	of the device drivers.				*)
	(*							*)
	(*	Getting inconsistent results towards the end	*)
	(*	of the hard disk: sector sometimes found, and	*)
	(*	sometimes not.  However, this is in an area	*)
	(*	which is officially beyond the disk capacity.	*)
	(*							*)
	(********************************************************)

FROM SYSTEM IMPORT
    (* type *)	BYTE,
    (* proc *)	ADR;

FROM Trace IMPORT
    (* proc *)	TraceOn, NYI, Pause, InTrace, OutTrace;

FROM Keyboard IMPORT
    (* proc *)	InKey, PutBack;

FROM Windows IMPORT
    (* type *)	Window, Colour, FrameType, DividerType,
    (* proc *)	OpenWindow, ChangeScrollingRegion, CloseWindow,
		WriteString, WriteChar, WriteLn, ScrollUp, ScrollDown,
		ColourSwap, SetCursor, EditString;

FROM NumericIO IMPORT
    (* proc *)	ReadCard, WriteCard, WriteHexByte, WriteHexWord,
		WriteHexLongword, EditHexLongword;

FROM Devices IMPORT
    (* type *)	Device, BlockNumberType,
    (* proc *)	IdentifyDevice, ReadPhysicalBlock;

FROM IOErrorCodes IMPORT
    (* type *)	ErrorCode,
    (* proc *)	TranslateErrorCode;

(*IMPORT HardDisk;*)

IMPORT Floppy;

(************************************************************************)

CONST Esc = CHR(27);

CONST
    (* MaxRow is the ending row number of the data area of the screen	*)
    (* window.								*)

    MaxRow = 18;

TYPE BufferSubscript = [0..511];

(************************************************************************)
(*									*)
(*  Essentially the whole of this module consists of procedures to	*)
(*  manipulate the data in a single block buffer, and to display it in	*)
(*  a fixed screen window.  Therefore, we might as well make this	*)
(*  information global to the module.					*)
(*									*)
(************************************************************************)

VAR

    (*  The fields in Display are:					*)
    (*		window:	the screen window.				*)
    (*		row, column: the current screen coordinates.		*)
    (*		ExtraAtTop: the number of rows which have scrolled off	*)
    (*			the top of the screen.				*)
    (*		ExtraAtBottom: the number of rows which failed to fit	*)
    (*			at the bottom of the screen.			*)

    Display: RECORD
		window: Window;
		row: [0..MaxRow];
		column: [0..15];
		ExtraAtTop, ExtraAtBottom: CARDINAL;
	     END (*RECORD*);

    (*  The record called "CurrentBlock" has fields:			*)
    (*		ByteNumber: the number of the byte within the block	*)
    (*		data:	actual data for the disk block on which we are	*)
    (*			currently operating.				*)

    CurrentBlock:   RECORD
			ByteNumber: BufferSubscript;
			data: ARRAY BufferSubscript OF BYTE;
		    END (*RECORD*);

(************************************************************************)
(*			BASIC DISPLAY OPERATIONS			*)
(************************************************************************)

PROCEDURE Highlight;

    (* Highlights the currently selected byte on the display.	*)

    BEGIN
	InTrace ("Highlight");
	WITH Display DO
	    ColourSwap (window, row+3, 3*column+8, 2);
	    ColourSwap (window, row+3, column+60, 1);
	    SetCursor (window, 1, 37);
	    WriteHexWord (window, CurrentBlock.ByteNumber);
	END (*WITH*);
	OutTrace ("Highlight");
    END Highlight;

(************************************************************************)

PROCEDURE Unhighlight;

    (* Removes the highlighting of the currently selected byte.	*)

    BEGIN
	InTrace ("Unhighlight");
	Highlight;
	OutTrace ("Unhighlight");
    END Unhighlight;

(************************************************************************)

PROCEDURE DisplayRow (j: CARDINAL);

    (* Displays row j of the Buffer.  (One row is 16 bytes).	*)

    VAR k: [0..15];
	Base: CARDINAL;

    BEGIN
	SetCursor (Display.window, j+3, 1);
	Base := 16*(j+Display.ExtraAtTop);

	(* Write the address of the first byte in the row.	*)

	WriteHexWord (Display.window, Base);  WriteString (Display.window, ":  ");

	(* Write 16 bytes in hexadecimal.	*)

	FOR k := 0 TO 15 DO
	    WriteHexByte (Display.window, CurrentBlock.data[Base+k]);
	    WriteChar (Display.window, " ");
	END (*FOR*);
	WriteString (Display.window, "    ");

	(* Now the same information in character form.	*)

	FOR k := 0 TO 15 DO
	    WriteChar (Display.window, CHR (CurrentBlock.data[Base+k]));
	END (*FOR*);
    END DisplayRow;

(************************************************************************)

PROCEDURE InitialDisplay;

    (* Displays the beginning of the buffer (a display of the whole	*)
    (* buffer would not fit onto the screen.				*)

    VAR j: [0..MaxRow-2];

    BEGIN
	InTrace ("InitialDisplay");
	WITH Display DO
	    row := 0;  column := 0;  ExtraAtTop := 0;
	    ExtraAtBottom := MAX(BufferSubscript) DIV 16 - MaxRow + 2;
	END (*WITH*);
	CurrentBlock.ByteNumber := 0;
	FOR j := 0 TO MaxRow-2 DO
	    DisplayRow (j);
	END (*FOR*);
	Highlight;
	OutTrace ("InitialDisplay");
    END InitialDisplay;

(************************************************************************)

PROCEDURE ShiftDisplayUp;

    (* Moves the display up one row, adding a new row at the bottom.	*)
    (* Exception: nothing happens if there is no new row to add.	*)

    VAR MaxRowMinusTwo: CARDINAL;

    BEGIN
	InTrace ("ShiftDisplayUp");
	WITH Display DO
	    IF ExtraAtBottom > 0 THEN
		ScrollUp (window);
		INC (ExtraAtTop);  DEC (ExtraAtBottom);
		INC (CurrentBlock.ByteNumber, 16);

		(* The following seemingly indirect way of proceeding	*)
		(* is to compensate for an unexplained intermittent	*)
		(* crash of the compiler.  It is very hard to pin down	*)
		(* the exact circumstances of the crash, because it is	*)
		(* not always repeatable, but it seems to be related to	*)
		(* using a constant as an actual procedure parameter.	*)

		MaxRowMinusTwo := MaxRow - 2;
		DisplayRow (MaxRowMinusTwo);
	    END (*IF*);
	END (*WITH*);
	OutTrace ("ShiftDisplayUp");
    END ShiftDisplayUp;

(************************************************************************)

PROCEDURE ShiftDisplayDown;

    (* Moves the display down one row, adding a new row at the top.	*)
    (* Exception: nothing happens if there is no new row to add.	*)

    BEGIN
	InTrace ("ShiftDisplayDown");
	WITH Display DO
	    IF ExtraAtTop > 0 THEN
		ScrollDown (window);
		DEC (ExtraAtTop);  INC (ExtraAtBottom);
		DEC (CurrentBlock.ByteNumber, 16);
		DisplayRow (0);
	    END (*IF*);
	END (*WITH*);
	OutTrace ("ShiftDisplayDown");
    END ShiftDisplayDown;

(************************************************************************)
(*			      CURSOR MOVEMENTS				*)
(************************************************************************)

PROCEDURE CursorUp (lines: CARDINAL);

    (* Moves the cursor up by the given number of rows.	*)

    VAR j: CARDINAL;

    BEGIN
	WITH Display DO
	    FOR j := 1 TO lines DO
		IF row = 0 THEN ShiftDisplayDown
		ELSE
		    DEC (row);  DEC (CurrentBlock.ByteNumber, 16);
		END (*IF*);
	    END (*FOR*);
	END (*WITH*);
    END CursorUp;

(************************************************************************)

PROCEDURE CursorDown (lines: CARDINAL);

    (* Moves the cursor down by the given number of rows.	*)

    VAR j: CARDINAL;

    BEGIN
	WITH Display DO
	    FOR j := 1 TO lines DO
		IF row = MaxRow-2 THEN ShiftDisplayUp
		ELSE
		    INC (row);  INC (CurrentBlock.ByteNumber, 16);
		END (*IF*);
	    END (*FOR*);
	END (*WITH*);
    END CursorDown;

(************************************************************************)

PROCEDURE CursorLeft;

    (* Moves the cursor one byte left.	*)

    BEGIN
	WITH Display DO
	    IF column = 0 THEN
		IF CurrentBlock.ByteNumber > 0 THEN
		    column := 15;  INC (CurrentBlock.ByteNumber, 15);
		    CursorUp(1);
		END (*IF*);
	    ELSE
		DEC (column);  DEC (CurrentBlock.ByteNumber);
	    END (*IF*);
	END (*WITH*);
    END CursorLeft;

(************************************************************************)

PROCEDURE CursorRight;

    (* Moves the cursor one byte right.	*)

    BEGIN
	WITH Display DO
	    IF column = 15 THEN
		IF CurrentBlock.ByteNumber < MAX(BufferSubscript) THEN
		    column := 0;  DEC (CurrentBlock.ByteNumber, 15);
		    CursorDown(1);
		END (*IF*);
	    ELSE
		INC (column);  INC (CurrentBlock.ByteNumber);
	    END (*IF*);
	END (*WITH*);
    END CursorRight;

(************************************************************************)

PROCEDURE Home;

    (* Moves the cursor to the beginning of the row.	*)

    BEGIN
	WITH Display DO
	    DEC (CurrentBlock.ByteNumber, column);  column := 0;
	END (*WITH*);
    END Home;

(************************************************************************)

PROCEDURE GoToEnd;

    (* Moves the cursor to the end of the row.	*)

    BEGIN
	WITH Display DO
	    INC (CurrentBlock.ByteNumber, 15-column);  column := 15;
	END (*WITH*);
    END GoToEnd;

(************************************************************************)

PROCEDURE CursorMovements (VAR (*INOUT*) nextchar: CHAR);

    (* Moves the cursor around the data buffer as directed by keyboard	*)
    (* input.  On entry, nextchar holds the first keyboard character.	*)
    (* On exit, nextchar holds the first input character which this	*)
    (* procedure does not recognise.					*)

    CONST Nul = CHR(0);  pagejump = 10;

    BEGIN
	InTrace ("CursorMovements");
	LOOP
	    IF nextchar = Nul THEN
		nextchar := InKey();
		Unhighlight;
		IF nextchar = "K" THEN		(* cursor left *)
		    CursorLeft;
		ELSIF nextchar = "M" THEN	(* cursor right *)
		    CursorRight;
		ELSIF nextchar = "H" THEN	(* cursor up *)
		    CursorUp (1);
		ELSIF nextchar = "P" THEN	(* cursor down *)
		    CursorDown (1);
		ELSIF nextchar = "I" THEN	(* page up *)
		    CursorUp (pagejump);
		ELSIF nextchar = "Q" THEN	(* page down *)
		    CursorDown (pagejump);
		ELSIF nextchar = "G" THEN	(* home *)
		    Home;
		ELSIF nextchar = "O" THEN	(* end *)
		    GoToEnd;
		ELSE
		    Highlight;
		    EXIT (*LOOP*);
		END (*IF*);
		Highlight;
		nextchar := InKey();
	    ELSE
		EXIT (*LOOP*);
	    END (*IF*);
	END (*LOOP*);
	OutTrace ("CursorMovements");
    END CursorMovements;

(************************************************************************)
(*			THE MAIN BLOCK OPERATIONS			*)
(************************************************************************)

PROCEDURE OperateOn (drive: CHAR;  BlockNumber: BlockNumberType);

    VAR nextchar: CHAR;  status: ErrorCode;
	device: Device;  unit: CARDINAL;
	string: ARRAY [0..31] OF CHAR;

    BEGIN
	IdentifyDevice (drive, device, unit);
	status := ReadPhysicalBlock (device, unit, BlockNumber,
						ADR(CurrentBlock.data));
	OpenWindow (Display.window,green,black,0,MaxRow+2,0,79,
						simpleframe,doubledivider);
	WriteString (Display.window, "Drive ");
	WriteChar (Display.window, drive);
	WriteString (Display.window, "     Block ");
	WriteHexLongword (Display.window, BlockNumber);
	WriteString (Display.window, "     Byte");
	WriteString (Display.window, "           status = ");
	TranslateErrorCode (status, string);
	WriteString (Display.window, string);
	ChangeScrollingRegion (Display.window, 3, MaxRow+1);
	InitialDisplay;
	nextchar := InKey();
	REPEAT
	    CursorMovements (nextchar);
	    IF nextchar <> Esc THEN
		nextchar := InKey();
	    END (*IF*);
	UNTIL nextchar = Esc;
	(*Pause;*)
	CloseWindow (Display.window);
    END OperateOn;

(************************************************************************)

PROCEDURE MainMenu;

    CONST CR = CHR(13);

    VAR w: Window;  drivename: ARRAY [0..0] OF CHAR;
	drive, ch: CHAR;  BlockNumber: BlockNumberType;

    BEGIN
	OpenWindow (w,white,red,0,4,0,35,simpleframe,nodivider);
	WriteString (w, "Test of hard disk read operations");
	BlockNumber := 0;  drive := "C";
	SetCursor (w, 2, 1);
	WriteString (w, "Drive: C   Block number: 00000000");
	LOOP
	    SetCursor (w, 2, 8);
	    drivename[0] := drive;
	    EditString (w, drivename, 1);

	    ch := InKey();
	    IF ch = Esc THEN EXIT(*LOOP*)
	    ELSIF ch <> CR THEN PutBack(ch);
	    END (*IF*);

	    drive := CAP(drivename[0]);
	    IF (drive < "A") OR (drive > "D") THEN EXIT(*LOOP*) END(*IF*);
	    SetCursor (w, 2, 26);
	    EditHexLongword (w, BlockNumber);
	    IF BlockNumber = 0FFFFFFFFH THEN EXIT(*LOOP*) END(*IF*);

	    ch := InKey();
	    IF ch = Esc THEN EXIT(*LOOP*)
	    ELSIF ch <> CR THEN PutBack(ch);
	    END (*IF*);

	    OperateOn (drive, BlockNumber);
	    SetCursor (w, 3, 1);
	    WriteString (w, "Finished with block ");
	    WriteHexLongword (w, BlockNumber);
	END (*LOOP*);
	CloseWindow (w);
    END MainMenu;

(************************************************************************)
(*				MAIN PROGRAM				*)
(************************************************************************)

BEGIN
(*    TraceOn (15,24,0,79,1); *)
    MainMenu;
END DiskPatch.
