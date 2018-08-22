IMPLEMENTATION MODULE FastWindows;

	(********************************************************)
	(*							*)
	(*	A very crude implementation of screen windows.	*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	11 August 1989			*)
	(*  Status:						*)
	(*	Seems to work, but not fully tested.		*)
	(*							*)
	(********************************************************)

FROM SYSTEM IMPORT
    (* proc *)	ADR;

FROM Storage2 IMPORT
    (* proc *)	ALLOCATE, DEALLOCATE;

FROM TerminationControl IMPORT
    (* proc *)	SetTerminationProcedure;

FROM LowLevelIO IMPORT
    (* proc *)	OutByte;

FROM Conversions IMPORT
    (* proc *)	HighByte, LowByte;

FROM BIOS IMPORT
    (* type *)	RegisterPacket,
    (* proc *)	BIOS;

FROM MemoryModel IMPORT
    (* proc *)	MakePointer, AddOffset, SubtractOffset;

FROM FastMove IMPORT
    (* proc *)	Moveup, Movedown;

	(* Remark: We use Movedown in preference to Moveup wherever	*)
	(* possible, because it is marginally more efficient.  These	*)
	(* two procedures are equivalent in their final result, except	*)
	(* for the case (which occurs in scrolling) where the source	*)
	(* and destination strings overlap in memory.			*)

FROM Keyboard IMPORT
    (* proc *)	InKey, PutBack;

(************************************************************************)

CONST
    VideoInt = 16;			(* interrupt number for BIOS	*)
    BytesPerChar = 2;			(* # bytes/char in video buffer	*)
    CharsPerRow = MaxColumnNumber + 1;	(* characters per row		*)
    BytesPerRow = BytesPerChar*CharsPerRow;
    buffersize = BytesPerRow * (MaxRowNumber+1);
					(* size in bytes of video buffer*)
    NormalVideo = 07x;  ReverseVideo = 070x;

(************************************************************************)

TYPE
    buffersubscript = [0..buffersize - 1];

    Window = POINTER TO WindowData;

	(****************************************************************)
	(*								*)
	(* The row and column values stored in this record are actual	*)
	(* screen row and column, i.e. they are not window-relative.	*)
	(*								*)
	(* The ScreenLocation field is a pointer into the hardware	*)
	(* video buffer.  It can be computed easily from the "row" and	*)
	(* "column" fields, but it is more convenient to keep this	*)
	(* technically redundant variable.				*)
	(*								*)
	(****************************************************************)

    WindowData = RECORD
		    frame: FrameType;  divider: DividerType;
		    ScrollRegion:   RECORD
					top, bottom: RowRange;
					left, right: ColumnRange;
				    END;
		    FirstRow, LastRow, row: RowRange; 
		    FirstColumn, LastColumn, column: ColumnRange;
		    ScreenLocation: POINTER TO CHAR;
		    foreground, background: Colour;
		    CurrentAttributes: CHAR;
		 END (*RECORD*);

(************************************************************************)

VAR

    (* BlackAndWhite is true if we have a monochrome display.		*)

    BlackAndWhite: BOOLEAN;

    (* ScreenSeg is a segment selector for the hardware video buffer,	*)
    (* and VideoPage is the page number.  (In fact, I don't use		*)
    (* VideoPage in this version, but it's here to document the way to	*)
    (* find its value, in case I need it in a future version.)		*)
    (* CRTCport is the port number to use when addressing the CRT	*)
    (* controller chip.  It is a variable, because the address depends	*)
    (* on whether a colour or monochrome interface is installed.	*)

    ScreenSeg: CARDINAL;
    VideoPage: CARDINAL;
    CRTCport: CARDINAL;

    (* Registers is a record of register values, to use when		*)
    (* communicating with the BIOS.					*)

    Registers: RegisterPacket;

    (* BlankRow is set up by the initialisation code as a row of space	*)
    (* characters.  Note however that the attribute codes need to be	*)
    (* filled in before each use.					*)

    BlankRow: ARRAY [0..BytesPerRow-1] OF CHAR;

(************************************************************************)
(*		    MISCELLANEOUS SCREEN OPERATIONS			*)
(************************************************************************)

PROCEDURE SetCursorShape (startline, endline: CARDINAL);

    (* Sets the start and end scan lines for the cursor.  This has to	*)
    (* be done with a BIOS call, rather than by programming the CRTC	*)
    (* registers directly, to ensure correct treatment over the variety	*)
    (* of different video interfaces which could be present.		*)

    VAR BIOSframe: RegisterPacket;

    BEGIN
	WITH BIOSframe DO
	    AH := 1;  CH := startline;  CL := endline;
	END (*WITH*);
	BIOS (VideoInt, BIOSframe);
    END SetCursorShape;

(************************************************************************)

PROCEDURE CursorOff;

    (* Turns the cursor on by putting 01 in bits 5 and 4 of the cursor	*)
    (* start register of the 6845 CRT controller.  Remark: according to	*)
    (* the documentation, it should be bits 6 and 5; I do not as yet	*)
    (* have an explanation of this discrepancy.				*)

    BEGIN
	SetCursorShape (16,0);
    END CursorOff;

(************************************************************************)

PROCEDURE CursorOn (row: RowRange;  column: ColumnRange);

    (* Displays a blinking screen cursor at the specified position.	*)

    VAR position: CARDINAL;

    BEGIN
	position := CharsPerRow*row + column;
	OutByte (CRTCport, 14);	(* the "cursor position higher" register *)
	OutByte (CRTCport+1, HighByte(position));
	OutByte (CRTCport, 15);	(* the "cursor position lower" register *)
	OutByte (CRTCport+1, LowByte(position));
	IF BlackAndWhite THEN
	    SetCursorShape (11,12);
	ELSE
	    SetCursorShape (6,7);
	END (*IF*);
    END CursorOn;

(************************************************************************)
(*			    SCREEN REFRESHING				*)
(************************************************************************)

PROCEDURE PutOnTop (w: Window);

    (* This is a dummy procedure, present only for compatibility with	*)
    (* module Windows.  The essential difference between module Windows	*)
    (* and the present module is that we do not maintain our windows in	*)
    (* a stack, and we do not do any screen refreshing (except as	*)
    (* needed for things like scrolling), since we assume that the	*)
    (* windows are in disjoint regions on the screen.			*)

    BEGIN
    END PutOnTop;

(************************************************************************)

PROCEDURE Blank(startrow, endrow: RowRange; startcol, endcol: ColumnRange;
							attributes: CHAR);

    (* Blanks the given (inclusive) rectangular region on the screen.	*)
    (* The attribute bytes are also set as specified; this will have no	*)
    (* immediate effect, but it affects the colour and blinking status	*)
    (* of characters which will subsequently be written into this	*)
    (* part of the screen.						*)

    VAR i: RowRange;
	offset, count: buffersubscript;

    BEGIN
	FOR offset := 1 TO BytesPerRow-1 BY BytesPerChar DO
	    BlankRow[offset] := attributes;
	END (*FOR*);
	offset := BytesPerRow*startrow + BytesPerChar*startcol;
	count := BytesPerChar*(endcol - startcol + 1);
	FOR i := startrow TO endrow DO
	    Movedown (ADR(BlankRow), MakePointer(ScreenSeg, offset), count);
	    INC (offset, BytesPerRow);
	END (*FOR*);
    END Blank;

(************************************************************************)
(*			BASIC SCREEN READ AND WRITE			*)
(************************************************************************)

PROCEDURE PutChar (place: buffersubscript;  ch: CHAR);

    (* Puts character ch at screen offset "place".	*)

    VAR p: POINTER TO CHAR;

    BEGIN
	p := MakePointer (ScreenSeg, place);
	p^ := ch;
    END PutChar;

(************************************************************************)
(*			    OPENING A WINDOW				*)
(************************************************************************)

PROCEDURE ChangeScrollingRegion (w: Window;  firstline, lastline: RowRange);

    (* Changes the scrolling region of window w to the new line		*)
    (* boundaries given, and sets its cursor to the start of the new	*)
    (* scrolling region.  The line numbers are window-relative.		*)

    VAR horizontal, vertical, leftT, rightT: CHAR;
	j: ColumnRange;  place: buffersubscript;

    BEGIN
	(* Although the user specifies window-relative line numbers,	*)
	(* we use screen-relative numbers internally.  Adjust the	*)
	(* parameters to take this into account.			*)

	INC (firstline, w^.FirstRow);
	INC (lastline, w^.FirstRow);

	(* Work out what characters to use for the frame and divider.	*)

	horizontal := 0C4x;  vertical := 0B3x;
	leftT := 0C3x;  rightT := 0B4x;
	WITH w^ DO
	    IF divider = doubledivider THEN
		horizontal := 0CDx;
	    END (*IF*);
	    IF frame = doubleframe THEN
		vertical := 0BAx;
		IF divider = doubledivider THEN
		    leftT := 0CCx;  rightT := 0B9x;
		ELSE
		    leftT := 0C7x;  rightT := 0B6x;
		END (*IF*);
	    ELSIF divider = doubledivider THEN
		leftT := 0C6x;  rightT := 0B5x;
	    END (*IF*);

	    (* Clean up the frame. *)

	    IF frame <> noframe THEN

		(* Remove the left and right T belonging to the	*)
		(* old divider bars, if necessary.		*)

		IF ScrollRegion.top - 1 > FirstRow THEN
		    place := BytesPerRow*(ScrollRegion.top - 1)
					+ BytesPerChar*FirstColumn;
		    PutChar (place, vertical);
		    PutChar (place + BytesPerChar*(LastColumn - FirstColumn),
					vertical);
		END (*IF*);

		IF ScrollRegion.bottom + 1 < LastRow THEN
		    place := BytesPerRow*(ScrollRegion.bottom+1)
					+ BytesPerChar*FirstColumn;
		    PutChar (place, vertical);
		    PutChar (place + BytesPerChar*(LastColumn - FirstColumn),
					vertical);
		END (*IF*);
	    END (*IF*);

	    (* Put in the new divider bars.	*)

	    IF divider <> nodivider THEN

		(* Put in the top horizontal bar.	*)

		IF firstline > FirstRow + 1 THEN
		    place := BytesPerRow*(firstline-1)
					+ BytesPerChar*FirstColumn;
		    IF frame <> noframe THEN
			PutChar (place, leftT);  INC (place, BytesPerChar);
		    END (*IF*);
		    FOR j := ScrollRegion.left TO ScrollRegion.right DO
			PutChar (place, horizontal);  INC(place, BytesPerChar);
		    END (*FOR*);
		    IF frame <> noframe THEN
			PutChar (place, rightT);
		    END (*IF*);
		END (*IF*);

		(* Put in the bottom horizontal bar.	*)

		IF lastline < LastRow - 1 THEN
		    place := BytesPerRow*(lastline+1)
					+ BytesPerChar*FirstColumn;
		    IF frame <> noframe THEN
			PutChar (place, leftT);  INC (place, BytesPerChar);
		    END (*IF*);
		    FOR j := ScrollRegion.left TO ScrollRegion.right DO
			PutChar (place, horizontal); INC(place, BytesPerChar);
		    END (*FOR*);
		    IF frame <> noframe THEN
			PutChar (place, rightT);
		    END (*IF*);
		END (*IF*);

	    END (*IF*);

	    (* Finally, update the scrolling region parameters.	*)

	    WITH ScrollRegion DO
		top := firstline;  bottom := lastline;
	    END (*WITH*);
	    SetCursor (w, firstline - FirstRow,
				ScrollRegion.left - FirstColumn);
	END (*WITH*);
    END ChangeScrollingRegion;

(************************************************************************)

PROCEDURE FillInFrame (w: Window);

    (* Makes the box around the window.	*)

    VAR i: RowRange;  j: ColumnRange;
	corner: ARRAY [1..4] OF CHAR;
	horizontal, vertical: CHAR;
	place, offset: buffersubscript;

    BEGIN
	IF w^.frame = simpleframe THEN
	    corner[1] := 0DAx;  corner[2] := 0BFx;
	    corner[3] := 0C0x;  corner[4] := 0D9x;
	    horizontal := 0C4x;  vertical := 0B3x;
	ELSE
	    corner[1] := 0C9x;  corner[2] := 0BBx;
	    corner[3] := 0C8x;  corner[4] := 0BCx;
	    horizontal := 0CDx;  vertical := 0BAx;
	END (*IF*);

	WITH w^ DO

	    offset := BytesPerChar*(LastColumn - FirstColumn);
	    place := BytesPerRow*FirstRow + BytesPerChar*FirstColumn;
	    PutChar (place, corner[1]);
	    PutChar (place+offset, corner[2]);
	    INC (place, BytesPerRow);

	    FOR i := FirstRow + 1 TO LastRow - 1 DO
		PutChar (place, vertical);
		PutChar (place+offset, vertical);
		INC (place, BytesPerRow);
	    END (*FOR*);

	    PutChar (place, corner[3]);
	    PutChar (place+offset, corner[4]);

	    offset := BytesPerRow*(LastRow - FirstRow);
	    FOR j := FirstColumn + 1 TO LastColumn - 1 DO
		INC (place, BytesPerChar);
		PutChar (place-offset, horizontal);
		PutChar (place, horizontal);
	    END (*FOR*);

	END (*WITH*);
    END FillInFrame;

(************************************************************************)

PROCEDURE MakeMonochrome (VAR (*INOUT*) foreground, background: Colour);

    (* Changes the two given colours to a suitable B/W combination.	*)

    BEGIN
	IF (foreground = black) OR (foreground = darkgrey) THEN
	    background := white
	ELSE
	    IF foreground > white THEN
		foreground := intensewhite
	    ELSE
		foreground := white;
	    END (*IF*);
	    background := black;
	END (*IF*);
    END MakeMonochrome;

(************************************************************************)

PROCEDURE OpenWindow (VAR (*OUT*) w: Window;
			ForegroundColour, BackgroundColour: Colour;
			firstline, lastline: RowRange;
			firstcol, lastcol: ColumnRange;
			FrameDesired: FrameType;
			DividerDesired: DividerType);

    (* Creates a new window, filled initially with space characters.	*)

    VAR i: RowRange;  k: buffersubscript;

    BEGIN

	(* Create the new window, and fill in all its fields.	*)

	NEW (w);
	WITH w^ DO
	    foreground := ForegroundColour;  background := BackgroundColour;
	    IF BlackAndWhite THEN
		MakeMonochrome (foreground, background);
	    END (*IF*);
	    CurrentAttributes := CHR(16*ORD(background) + ORD(foreground));
	    frame := FrameDesired;  divider := DividerDesired;
	    FirstRow := firstline;  LastRow := lastline;
	    FirstColumn := firstcol;  LastColumn := lastcol;

	    (* Set the window contents to all space characters.	*)

	    Blank (firstline, lastline, firstcol, lastcol, CurrentAttributes);

	    (* Set up a default scrolling region.	*)

	    WITH ScrollRegion DO
		top := FirstRow;  bottom := LastRow;
		left := FirstColumn;  right := LastColumn;
	    END (*WITH*);

	    (* Make the frame.	*)

	    IF frame <> noframe THEN
		FillInFrame(w);
		WITH ScrollRegion DO
		    INC (top);  INC (left);  DEC (bottom);  DEC (right);
		END (*WITH*);
	    END (*IF*);

	    row := ScrollRegion.top;  column := ScrollRegion.left;
	    ScreenLocation := MakePointer (ScreenSeg,
				BytesPerRow*row + BytesPerChar*column);
	END (*WITH*);

    END OpenWindow;

(************************************************************************)

PROCEDURE OpenSimpleWindow (VAR (*OUT*) w: Window;
			firstline, lastline: RowRange;
			firstcol, lastcol: ColumnRange);

    (* Identical to OpenWindow, except that you don't get any choice	*)
    (* about the colours or frame.  The window is white-on-black with	*)
    (* a simple frame and no dividers for the scrolling region.  This	*)
    (* version of OpenWindow is useful for those with monochrome	*)
    (* displays who don't want to be bothered with importing the types	*)
    (* Colour, FrameType, and DividerType.				*)

    BEGIN
	OpenWindow (w, white, black, firstline, lastline,
			firstcol, lastcol, simpleframe, nodivider);
    END OpenSimpleWindow;

(************************************************************************)
(*			    CLOSING A WINDOW				*)
(************************************************************************)

PROCEDURE CloseWindow (w: Window);

    (* Reclaims the space used for this window, and blanks the part of	*)
    (* the screen which it occupied.					*)

    VAR p: Window;

    BEGIN
	WITH w^ DO
	    Blank (FirstRow, LastRow, FirstColumn, LastColumn, NormalVideo);
	END (*WITH*);
	DISPOSE (w);
    END CloseWindow;

(************************************************************************)
(*	     	OPERATIONS ON CHARACTER ATTRIBUTES			*)
(************************************************************************)

PROCEDURE ChangeAttributes (w: Window; r: RowRange; c: ColumnRange;
				nchar: CARDINAL; newattribute: CHAR);

    (* POSSIBLY OBSOLETE - CHECK THIS LATER.				*)
    (* Changes nchar characters, starting at location (r,c), to be	*)
    (* displayed with the given new attribute code.  The row and column	*)
    (* numbers are window-relative, not absolute screen coordinates.	*)

    VAR k, start: buffersubscript;

    BEGIN
	WITH w^ DO
	    start := BytesPerRow*(r+FirstRow)
			+ BytesPerChar*(c+FirstColumn) + 1;
	    FOR k := start TO start+BytesPerChar*(nchar-1) BY BytesPerChar DO
		PutChar (k, newattribute);
	    END (*FOR*);
	END (*WITH*);
    END ChangeAttributes;

(************************************************************************)

PROCEDURE ColourSwap (w: Window; r: RowRange; c: ColumnRange;
							nchar: CARDINAL);

    (* Switches the foreground and background colours for nchar		*)
    (* characters, starting at location (r,c).  The row and column	*)
    (* numbers are window-relative, not absolute screen coordinates.	*)
    (* This is our colour equivalent of the "reverse video" operation.	*)
    (* NOTE: This procedure will not wrap around to a new row.		*)

    VAR k, oldattribute: CARDINAL;
	p: POINTER TO CHAR;

    BEGIN
	WITH w^ DO
	    p := MakePointer (ScreenSeg, BytesPerRow*(r+FirstRow)
				+ BytesPerChar*(c+FirstColumn) + 1);
	    FOR k := 0 TO nchar-1 DO
		oldattribute := ORD (p^);
		p^ := CHR (16*(oldattribute MOD 16)
					+ (oldattribute DIV 16));
		p := AddOffset (p, BytesPerChar);
	    END (*FOR*);
	END (*WITH*);
    END ColourSwap;

(************************************************************************)
(*			    CURSOR OPERATIONS				*)
(************************************************************************)

PROCEDURE SetCursor (w: Window; r: RowRange; c: ColumnRange);

    (* Sets the cursor for window w to relative row r, column c.	*)

    BEGIN
	WITH w^ DO
	    row := r + FirstRow;  column := c + FirstColumn;
	    ScreenLocation := MakePointer (ScreenSeg,
				BytesPerRow*row + BytesPerChar*column);
	END (*WITH*);
    END SetCursor;

(************************************************************************)

PROCEDURE SaveCursor (w: Window; VAR (*out*) r: RowRange;
				 VAR (*out*) c: ColumnRange);

    (* Returns the current cursor position of window w.		*)

    BEGIN
	WITH w^ DO
	    r := row - FirstRow;  c := column - FirstColumn;
	END (*WITH*);
    END SaveCursor;

(************************************************************************)

PROCEDURE CursorLeft (w: Window);

    (* Moves the window cursor one position left.  If it falls off the	*)
    (* left edge of the window, move to the right edge in the same row.	*)

    BEGIN
	WITH w^ DO
	    IF column = FirstColumn THEN
		column := LastColumn;
		ScreenLocation := MakePointer (ScreenSeg,
				BytesPerRow*row + BytesPerChar*column);
	    ELSE
		DEC (column);
		ScreenLocation := SubtractOffset (ScreenLocation,
							BytesPerChar);
	    END (*IF*);
	END (*WITH*);
    END CursorLeft;

(************************************************************************)

PROCEDURE CursorRight (w: Window);

    (* Moves the window cursor one position right.  If it falls off the	*)
    (* right edge of the window, move to the left edge in the same row.	*)

    BEGIN
	WITH w^ DO
	    IF column = LastColumn THEN
		column := FirstColumn;
		ScreenLocation := MakePointer (ScreenSeg,
				BytesPerRow*row + BytesPerChar*column);
	    ELSE
		INC (column);
		ScreenLocation := AddOffset (ScreenLocation, BytesPerChar);
	    END (*IF*);
	END (*WITH*);
    END CursorRight;

(************************************************************************)

PROCEDURE CursorUp (w: Window);

    (* Moves the window cursor one position up.  If it falls off the	*)
    (* top edge of the window, it moves to the bottom edge in the same	*)
    (* column.								*)

    BEGIN
	WITH w^ DO
	    IF row = FirstRow THEN
		row := LastRow;
		ScreenLocation := MakePointer (ScreenSeg,
				BytesPerRow*row + BytesPerChar*column);
	    ELSE
		DEC (row);
		ScreenLocation := SubtractOffset (ScreenLocation,
							BytesPerRow);
	    END (*IF*);
	END (*WITH*);
    END CursorUp;

(************************************************************************)

PROCEDURE CursorDown (w: Window);

    (* Moves the window cursor one position down.  If it falls off the	*)
    (* bottom edge of the window, it moves to the top edge in the same	*)
    (* column.								*)

    BEGIN
	WITH w^ DO
	    IF row = LastRow THEN
		row := FirstRow;
		ScreenLocation := MakePointer (ScreenSeg,
				BytesPerRow*row + BytesPerChar*column);
	    ELSE
		INC (row);
		ScreenLocation := AddOffset (ScreenLocation,BytesPerRow);
	    END (*IF*);
	END (*WITH*);
    END CursorDown;

(************************************************************************)

PROCEDURE ScrollUp (w: Window);

    (* Scrolls window w up by one line, and space fills the last row.	*)

    VAR line: RowRange;
	current, below: POINTER TO CHAR;
	count: CARDINAL;

    BEGIN
	WITH w^ DO
	    FOR count := 1 TO BytesPerRow-1 BY BytesPerChar DO
		BlankRow[count] := CurrentAttributes;
	    END (*FOR*);
	    WITH ScrollRegion DO
		current := MakePointer (ScreenSeg, BytesPerRow*top
							+ BytesPerChar*left);
		count := BytesPerChar*(right-left+1);
		FOR line := top TO bottom-1 DO
		    below := AddOffset (current, BytesPerRow);
		    Movedown (below, current, count);
		    current := below;
		END (*FOR*);
		Movedown (ADR(BlankRow),  current, count);
	    END (*WITH*);
	END (*WITH*);
    END ScrollUp;

(************************************************************************)

PROCEDURE ScrollDown (w: Window);

    (* Scrolls window w down by one line.  The first row is filled with	*)
    (* spaces.								*)

    VAR line: RowRange;
	current, above: POINTER TO CHAR;
	count: CARDINAL;

    BEGIN
	WITH w^ DO
	    FOR count := 1 TO BytesPerRow-1 BY BytesPerChar DO
		BlankRow[count] := CurrentAttributes;
	    END (*FOR*);
	    WITH ScrollRegion DO
		current := MakePointer (ScreenSeg, BytesPerRow*bottom
							+ BytesPerChar*left);
		count := BytesPerChar*(right-left+1);
		FOR line := bottom TO top+1 BY -1 DO
		    above := SubtractOffset (current, BytesPerRow);
		    Moveup (above, current, count);
		    current := above;
		END (*FOR*);
		Movedown (ADR(BlankRow),  current, count);
	    END (*WITH*);
	END (*WITH*);
    END ScrollDown;

(************************************************************************)
(*			    MAIN OUTPUT ROUTINES			*)
(************************************************************************)

PROCEDURE WriteChar (w: Window; ch: CHAR);

    (* Writes one character to window w, and updates the cursor for	*)
    (* this window.  Characters which fall of the right edge of the	*)
    (* window are displayed in the rightmost position of the row,	*)
    (* overwriting what was already there.  This procedure does not	*)
    (* recognise the concept of a control character.  Every possible	*)
    (* value of ch produces something readable on the screen.		*)

    BEGIN
	WITH w^ DO
	    ScreenLocation^ := ch;
	    IF column < ScrollRegion.right THEN
		INC (column);
		ScreenLocation := AddOffset(ScreenLocation,BytesPerChar);
	    END (*IF*);
	END (*WITH*);
    END WriteChar;

(************************************************************************)

PROCEDURE WriteLn (w: Window);

    (* Moves the cursor of window w to the start of the next row.  If	*)
    (* we are already at the last row, the window scrolls up.		*)

    BEGIN
	WITH w^ DO
	    column := ScrollRegion.left;
	    IF row = ScrollRegion.bottom THEN
		ScrollUp (w);
	    ELSE
		INC (row);
	    END (*IF*);
	    ScreenLocation := MakePointer (ScreenSeg,
				BytesPerRow*row + BytesPerChar*column);
	END (*WITH*);
    END WriteLn;

(************************************************************************)

PROCEDURE WriteString (w: Window; text: ARRAY OF CHAR);

    (* Writes a sequence of characters, terminated either by NUL or by	*)
    (* the end of the array.						*)

    VAR j: CARDINAL;

    BEGIN
	j := 0;
	LOOP
	    IF ORD (text[j]) = 0 THEN EXIT (*LOOP*)  END (*IF*);
	    WriteChar (w, text[j]);  INC (j);
	    IF j > HIGH (text) THEN EXIT (*LOOP*)  END (*IF*);
	END (*LOOP*);
    END WriteString;

(************************************************************************)

PROCEDURE Write (w: Window; ch: CHAR);

    (* A version of procedure WriteChar which looks after some of the	*)
    (* control characters.						*)

    BEGIN
	IF ch >= " " THEN WriteChar (w, ch)
	ELSIF ORD(ch) = 8 THEN CursorLeft(w)
	ELSIF ORD(ch) = 13 THEN WriteLn(w)
	ELSE
	    WriteChar (w, "^");  WriteChar (w, CHR(ORD(ch)+64))
	END (*IF*);
    END Write;

(************************************************************************)
(*				INPUT					*)
(************************************************************************)

PROCEDURE ReadCharWithoutEcho (w: Window;  VAR (*OUT*) ch: CHAR);

    (* Read one character, displaying a blinking cursor until the	*)
    (* character has been typed.					*)

    BEGIN
	CursorOn (w^.row, w^.column);
	ch := InKey();
	CursorOff;
    END ReadCharWithoutEcho;

(************************************************************************)

PROCEDURE ReadChar (w: Window;  VAR (*OUT*) ch: CHAR);

    (* Like ReadCharWithoutEcho, but the input character is echoed.	*)

    BEGIN
	ReadCharWithoutEcho (w, ch);  Write (w, ch);
    END ReadChar;

(************************************************************************)

PROCEDURE UnreadChar (w: Window;  ch: CHAR);

    (* Returns ch back to the keyboard queue, and deletes it from the	*)
    (* the screen.  It is assumed that the last operation on window w	*)
    (* was a ReadChar, and that ch was the character returned.  If this	*)
    (* assumption is violated, there is no guarantee that the behaviour	*)
    (* of this procedure will be consistent from version to version.	*)

    BEGIN
	PutBack (ch);
	IF ORD(ch) = 8 THEN
	    CursorRight (w);
	ELSE
	    CursorLeft (w);
	    IF ORD(ch) = 13 THEN CursorUp(w)
	    ELSE
		WriteChar (w, " ");  CursorLeft(w);
		IF ch < " " THEN 
		    CursorLeft (w);  WriteChar (w, " ");  CursorLeft (w);
		END (*IF*);
	    END (*IF*);
	END (*IF*);
    END UnreadChar;

(************************************************************************)

PROCEDURE ReadString (w: Window;  VAR (*out*) result: ARRAY OF CHAR);

    (* Reads a character string, terminated by carriage return.		*)

    VAR j: CARDINAL;  ch: CHAR;

    BEGIN
	FOR j := 0 TO HIGH(result) DO
	    result[j] := " ";
	END (*FOR*);
	j := 0;
	LOOP
	    ReadChar (w, ch);
	    IF ORD(ch) = 13 THEN
		result[j] := CHR(0);  EXIT(*LOOP*)
	    ELSIF ORD(ch) = 8 THEN	(* backspace *)
		IF j > 0 THEN
		    CursorLeft(w);  WriteChar(w, " ");  CursorLeft(w);
		    DEC (j);
		END (*IF*);
	    ELSE
		result[j] := ch;
		IF j = HIGH(result) THEN EXIT(*LOOP*) END(*IF*);
		INC (j);
	    END(*IF*);
	END (*LOOP*);
    END ReadString;

(************************************************************************)

PROCEDURE EditString (w: Window;  VAR (*INOUT*) result: ARRAY OF CHAR);

    (* Reads a character string, where a default result is supplied by	*)
    (* the caller.  The final result is the state of the string at the	*)
    (* time where the keyboard user types a carriage return.  If the	*)
    (* user types Esc at any time, then we return with result[0] = Esc.	*)

    CONST Esc = 01Bx;

    VAR j, k: CARDINAL;  ch: CHAR;  limit: ColumnRange;
	startrow: RowRange;  startcolumn: ColumnRange;

    BEGIN
	limit := HIGH(result);
	SaveCursor (w, startrow, startcolumn);
	WITH w^ DO
	    IF limit > ScrollRegion.right - FirstColumn - startcolumn THEN
		limit := ScrollRegion.right - FirstColumn - startcolumn
	    END (*IF*);
	END (*WITH*);

	(* Preprocessing: for a Nul-terminated string, remove the Nul	*)
	(* and pad out the string with spaces at the right.  Otherwise	*)
	(* we get problems if, for example, the Nul is deleted.		*)

	j := 0;
	LOOP
	    IF result[j] = CHR(0) THEN
		FOR k := j TO limit DO
		    result[k] := " ";
		END (*FOR*);
		EXIT (*LOOP*);
	    END (*IF*);
	    IF j = limit THEN EXIT(*LOOP*) END(*IF*);
	    INC (j);
	END (*LOOP*);
	FOR k := limit+1 TO HIGH(result) DO
	    result[k] := " ";
	END (*FOR*);

	(* Write the string, using reverse video.	*)

	WriteString (w, result);
	ColourSwap (w, startrow, startcolumn, limit+1);
	SetCursor (w, startrow, startcolumn);

	(* Now the main editing loop.	*)

	j := 0;
	LOOP
	    ReadCharWithoutEcho (w, ch);
	    IF ORD(ch) = 0 THEN
		ReadCharWithoutEcho (w, ch);
		IF ch = "K" THEN	(* cursor left *)
		    IF j > 0 THEN
			CursorLeft(w);
			DEC (j);
		    END (*IF*);
		ELSIF ch = "M" THEN	(* cursor right *)
		    IF j <= limit THEN
			CursorRight(w);
			INC (j);
		    END (*IF*);
		ELSIF ch = "G" THEN	(* home *)
		    j := 0;
		    SetCursor (w, startrow, startcolumn);
		ELSIF ch = "O" THEN	(* end *)
		    j := limit;
		    SetCursor (w, startrow, startcolumn+j);
		ELSIF ch = "R" THEN	(* insert *)
		    FOR k := limit TO j+1 BY -1 DO
			result[k] := result[k-1];
		    END (*FOR*);
		    result[j] := " ";
		    SetCursor (w, startrow, startcolumn);
		    WriteString (w, result);
		    SetCursor (w, startrow, startcolumn+j);
		ELSIF ch = "S" THEN	(* delete *)
		    FOR k := j TO limit-1 DO
			result[k] := result[k+1];
		    END (*FOR*);
		    result[limit] := " ";
		    SetCursor (w, startrow, startcolumn);
		    WriteString (w, result);
		    SetCursor (w, startrow, startcolumn+j);
		END (*IF*);
	    ELSIF ch = Esc THEN
		result[0] := Esc;  EXIT(*LOOP*);
	    ELSIF ORD(ch) = 13 THEN
		EXIT(*LOOP*)
	    ELSIF ORD(ch) = 8 THEN	(* backspace *)
		IF j > 0 THEN
		    CursorLeft(w);
		    DEC (j);
		END (*IF*);
	    ELSIF j <= limit THEN
		result[j] := ch;  WriteChar (w, ch);
		INC (j);
	    END(*IF*);
	END (*LOOP*);

	(* Restore the original colours. *)

	ColourSwap (w, startrow, startcolumn, limit+1);
    END EditString;

(************************************************************************)
(*		    MISCELLANEOUS CONTROL OPERATIONS			*)
(************************************************************************)

PROCEDURE EraseLine (w: Window;  option: CARDINAL);

    (* Replaces some or all of the current line, except for the border,	*)
    (* with space characters.  The window cursor is not moved.		*)
    (* The options are:							*)
    (*		0	the whole of the line, except for the border	*)
    (*		1	from the current cursor position onwards	*)
    (*		2	from the start to just before the cursor	*)

    VAR first, last: ColumnRange;
	k, firstk, lastk: buffersubscript;

    BEGIN
	WITH w^ DO
	    first := FirstColumn;  last := LastColumn;
	    IF frame <> noframe THEN
		INC (first);  DEC (last);
	    END (*IF*);
	    IF option = 1 THEN first := column
	    ELSIF option = 2 THEN last := column - 1
	    END (*IF*);
	    firstk := BytesPerRow*row + BytesPerChar*first;
	    lastk := BytesPerRow*row + BytesPerChar*last;
	    FOR k := firstk TO lastk BY BytesPerChar DO
		PutChar (k, " ");
	    END (*FOR*);
	END (*WITH*);
    END EraseLine;

(************************************************************************)
(*			     TERMINATION				*)
(************************************************************************)

PROCEDURE CleanUp;

    (* Called at program termination.  Turns on the blinking cursor.	*)

    BEGIN
	CursorOn(0,0);
    END CleanUp;

(************************************************************************)
(*			    INITIALISATION				*)
(************************************************************************)

VAR j: buffersubscript;

BEGIN

    (* The BIOS video call 0FH returns the current video state.  From	*)
    (* this we can work out whether to use colour or mono.		*)
    (* WARNING: There is an assumption here that the processor is in	*)
    (* real address mode.  At some stage I should fix up the setting of	*)
    (* ScreenSeg (via a call to MemoryModel) so that this module will	*)
    (* work equally well in protected mode.				*)

    ScreenSeg := 0B000H;	(* assume mono initially *)
    BlackAndWhite := TRUE;
    WITH Registers DO
	AH := 0FH;  BIOS (VideoInt, Registers);  VideoPage := BH;
	IF AL <> 7 THEN		(* must be colour *)

	    ScreenSeg := 0B800H;  BlackAndWhite := FALSE;

	    (* Set the screen to 25*80 colour.	*)

	    AH := 0;  AL := 3;  BIOS (VideoInt, Registers);

	END (*IF*);
    END (*WITH*);

    IF BlackAndWhite THEN
	CRTCport := 03B4H;
    ELSE
	CRTCport := 03D4H;
    END (*IF*);

    SetTerminationProcedure (CleanUp);
    FOR j := 0 TO HIGH(BlankRow) DO
	BlankRow[j] := " ";
    END (*FOR*);

    (* Blank the screen, to erase otherwise annoying background stuff	*)
    (* left by other programs.						*)

    CursorOff;
    Blank (0,MaxRowNumber,0,MaxColumnNumber, NormalVideo);

END FastWindows.
