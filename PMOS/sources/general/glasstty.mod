IMPLEMENTATION MODULE GlassTTY;

	(********************************************************)
	(*							*)
	(*	     Simple screen output routines.		*)
	(*							*)
	(*  This module handles screen output at a very low	*)
	(*  level, without supplying the advanced features	*)
	(*  which may be found in, for example, module Windows.	*)
	(*  It is intended for things like error message	*)
	(*  output, and is designed for compactness rather	*)
	(*  than comprehensiveness.				*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	27 February 1995		*)
	(*  Status:		OK				*)
	(*							*)
	(********************************************************)

FROM SYSTEM IMPORT
    (* proc *)	ADR;

FROM Types IMPORT
    (* proc *)	FarPointer, FarCharPointer;

FROM TextVideo IMPORT
    (* proc *)	VideoKind, PositionCursor;

FROM LowLevel IMPORT
    (* proc *)	LowByte, HighByte, RSB, IANDB,
		Far, MakePointer, SEGMENT, OFFSET, FarCopy;

(************************************************************************)

CONST
    BytesPerChar = 2;			(* # bytes/char in video buffer	*)
    NoOfColumns = 80;
    BytesPerRow = NoOfColumns*BytesPerChar;  (* bytes per screen row	*)
    bottomrow = 24;			(* row # of last screen row	*)

TYPE
    RowRange = [0..bottomrow];
    HexDigit = SHORTCARD [0..15];

(*************************************************************************)

VAR
    (* BlankRow is set up by the initialisation code as a row of space	*)
    (* characters.							*)

    BlankRow: ARRAY [0..BytesPerRow-1] OF CHAR;

    (* 16*ScreenSeg is the physical address of the video buffer.  The	*)
    (* value depends on whether we are using black-and-white or colour.	*)

    ScreenSeg: CARDINAL;

    (* ScreenPosition is the current cursor location, relative to the	*)
    (* start of the screen.  SavedScreenPosition is a copy made by	*)
    (* procedure SaveCursor.						*)

    ScreenPosition, SavedScreenPosition: CARDINAL;

    (* CurrentRow is the number of the current screen line.		*)

    CurrentRow: RowRange;

    (* CurrentColumn is the number of the current screen column.  The	*)
    (* special case CurrentColumn = NoOfColumns means that we have run	*)
    (* off the end of the current row, and must do a WriteLn or		*)
    (* SetCursor before writing a new character.			*)

    CurrentColumn: [0..NoOfColumns];

(************************************************************************)
(*			SCROLLING AND CURSOR MOVEMENTS			*)
(************************************************************************)

PROCEDURE SetCursor (row, column: CARDINAL);

    (* Moves the screen cursor to the specified row and column.		*)

    BEGIN
	CurrentRow := row;  CurrentColumn := column;
	ScreenPosition := BytesPerRow*row + BytesPerChar*column;
	PositionCursor (TRUE, ScreenPosition, FALSE);
    END SetCursor;

(************************************************************************)

PROCEDURE SaveCursor;

    (* Remembers the current cursor position, for use by a subsequent	*)
    (* call to RestoreCursor.  Note that nesting is not supported, i.e.	*)
    (* a call to SaveCursor destroys the information saved by any	*)
    (* earlier call to SaveCursor.					*)

    BEGIN
	SavedScreenPosition := ScreenPosition;
    END SaveCursor;

(************************************************************************)

PROCEDURE RestoreCursor;

    (* Sets the cursor back to where it was at the time of the last	*)
    (* call to SaveCursor.						*)

    BEGIN
	ScreenPosition := SavedScreenPosition;
	CurrentRow := ScreenPosition DIV BytesPerRow;
	CurrentColumn := (ScreenPosition MOD BytesPerRow) DIV BytesPerChar;
	PositionCursor (TRUE, ScreenPosition, FALSE);
    END RestoreCursor;

(************************************************************************)

PROCEDURE ScrollUp;

    (* Scrolls the screen contents up by one line.  The last row is	*)
    (* filled with spaces.						*)

    VAR screenloc: FarPointer;

    BEGIN
	FarCopy (MakePointer (ScreenSeg, BytesPerRow),
			MakePointer (ScreenSeg, 0),
				BytesPerRow*bottomrow);
	ScreenPosition := BytesPerRow*bottomrow;
	screenloc := MakePointer (ScreenSeg, ScreenPosition);
	FarCopy (Far(ADR(BlankRow)), screenloc, BytesPerRow);
    END ScrollUp;

(************************************************************************)

PROCEDURE WriteLn;

    (* Moves the screen cursor to the beginning of the next line,	*)
    (* scrolling if necessary.						*)

    BEGIN
	IF CurrentRow = bottomrow THEN ScrollUp
	ELSE INC (CurrentRow);
	END (*IF*);
	CurrentColumn := 0;
	ScreenPosition := BytesPerRow*CurrentRow;
	PositionCursor (TRUE, ScreenPosition, FALSE);
    END WriteLn;

(************************************************************************)
(*			CHARACTER AND STRING OUTPUT			*)
(************************************************************************)

PROCEDURE WriteChar (ch: CHAR);

    (* Writes one character, and updates the cursor.  This procedure	*)
    (* does not recognise the concept of a control character.  Every	*)
    (* possible value of ch produces something readable on the screen.	*)
    (* If we have run off the end of the current row, wraps to a	*)
    (* new line.							*)

    VAR screenloc: FarCharPointer;

    BEGIN
	IF CurrentColumn = NoOfColumns THEN
	    WriteLn;
	END (*IF*);
	screenloc := MakePointer (ScreenSeg, ScreenPosition);
	screenloc^ := ch;
	INC (ScreenPosition, BytesPerChar);
	INC (CurrentColumn);
	PositionCursor (TRUE, ScreenPosition, FALSE);
    END WriteChar;

(************************************************************************)

PROCEDURE WriteString (text: ARRAY OF CHAR);

    (* Writes a sequence of characters, terminated either by NUL or by	*)
    (* the end of the array.						*)

    VAR j: CARDINAL;

    BEGIN
	j := 0;
	LOOP
	    IF ORD (text[j]) = 0 THEN EXIT(*LOOP*)  END (*IF*);
	    WriteChar (text[j]);  INC (j);
	    IF j > HIGH (text) THEN EXIT(*LOOP*)  END (*IF*);
	END (*LOOP*);
    END WriteString;

(************************************************************************)
(*			NUMERIC OUTPUT (HEXADECIMAL)			*)
(************************************************************************)

PROCEDURE WriteHexDigit (number: HexDigit);

    (* Writes a one-digit hexadecimal number.	*)

    BEGIN
	IF number < 10 THEN
	    WriteChar (CHR(ORD("0")+ORD(number)))
	ELSE
	    WriteChar (CHR(ORD("A")+ORD(number)-10))
	END (*IF*);
    END WriteHexDigit;

(*************************************************************************)

PROCEDURE WriteHexByte (number: BYTE);

    (* Writes its argument as a two-digit hexadecimal number.	*)

    BEGIN

	(* The obscure function names from LowLevel are:	*)
	(*	RSB = right shift	IANDB = logical AND	*)

	WriteHexDigit (RSB(number,4));
	WriteHexDigit (IANDB(number,15));
    END WriteHexByte;

(*************************************************************************)

PROCEDURE WriteHexWord (number: CARDINAL);

    (* Writes its argument as a four-digit hexadecimal number.	*)

    BEGIN
	WriteHexByte (HighByte(number));
	WriteHexByte (LowByte(number));
    END WriteHexWord;

(************************************************************************)

PROCEDURE WriteAddress (addr: ADDRESS);

    (* Writes a segmented address to the screen.	*)

    BEGIN
	WriteHexWord (SEGMENT(addr));  WriteChar (":");
	WriteHexWord (OFFSET(addr));
    END WriteAddress;

(************************************************************************)
(*			NUMERIC OUTPUT (DECIMAL)			*)
(************************************************************************)

PROCEDURE WriteLongCard (number: LONGCARD);

    (* Writes a number to the screen.	*)

    VAR remainder: CARDINAL;

    BEGIN
	IF number > 9 THEN
	    WriteLongCard (number DIV 10);
	END (*IF*);
	remainder := CARDINAL (number MOD 10);
	WriteChar (CHR(remainder + ORD("0")));
    END WriteLongCard;

(************************************************************************)

PROCEDURE WriteCard (number: CARDINAL);

    (* Writes a number to the screen.	*)

    BEGIN
	WriteLongCard (VAL(LONGCARD,number));
    END WriteCard;

(************************************************************************)

PROCEDURE WriteInt (number: INTEGER);

    (* Writes a number to the screen.	*)

    BEGIN
	IF number < 0 THEN
	    WriteChar ('-');  number := -number;
	END (*IF*);
	WriteCard (VAL(CARDINAL,number));
    END WriteInt;

(************************************************************************)
(*			    INITIALISATION				*)
(************************************************************************)

PROCEDURE Initialise;

    VAR j: CARDINAL;  dummy: BOOLEAN;

    BEGIN
	VideoKind (ScreenSeg, dummy);

	FOR j := 0 TO HIGH(BlankRow)-1 BY 2 DO
	    BlankRow[j] := " ";  BlankRow[j+1] := CHR(07H);
	END (*FOR*);

	ScreenPosition := 0;  SavedScreenPosition := 0;
	CurrentRow := 0;  CurrentColumn := 0;

    END Initialise;

(************************************************************************)

BEGIN
    Initialise;
END GlassTTY.
