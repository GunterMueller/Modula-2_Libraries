IMPLEMENTATION MODULE GWindows;

	(********************************************************)
	(*							*)
	(*	    Windows module for screen graphics		*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	21 February 1995		*)
	(*  Status:						*)
	(*	Points, lines, and text working.		*)
	(*	Still adding more features.			*)
	(*	Now working on scrolling, setting cursor, etc.	*)
	(*							*)
	(********************************************************)

FROM Graphics IMPORT
    (* proc *)	SetMode, SetDefaultMode, GetScreenShape;

FROM Storage1 IMPORT
    (* proc *)	ALLOCATE, DEALLOCATE;

FROM ScreenGeometry IMPORT
    (* proc *)	Inside, TrimLine;

FROM Tiles IMPORT
    (* type *)	TileSet,
    (* proc *)	CreateTileSet, DiscardTileSet, AddPoint, AddLine,
		AddRectangle, AddString, ClearTileSet, ScrollContents,
		AddRotatedString, TileSetMemory;

FROM Keyboard IMPORT
    (* proc *)	InKey;

FROM TerminationControl IMPORT
    (* proc *)	SetTerminationProcedure, TerminationMessage;

(************************************************************************)

TYPE
    (* A window is described by a record with the following fields.	*)
    (*	absborder	the outside edge of the window, in absolute	*)
    (*			(i.e. screen-relative) coordinates.		*)
    (*	databorder	the rectangle, in screen-relative coordinates,	*)
    (*			outside which data should not be plotted.	*)
    (*	tiles		the set of tiles making up the window.		*)
    (*	background	the background colour for the window.		*)
    (*	foreground	the foreground colour for the window.		*)
    (*	cursor		the text cursor, in terms of points.		*)
    (*	row, column	the text cursor, in terms of characters.	*)
    (*	ScrollRegion	the "normal" region for text, measured in	*)
    (*			  terms of character positions.			*)

    Window = POINTER TO
		RECORD
		    absborder, databorder: Rectangle;
		    tiles: TileSet;
		    background, foreground: ColourType;
		    cursor: Point;

		    (* The following fields are in units of characters	*)
		    (* rather than (x,y) coordinates.			*)

		    row, column: INTEGER;
		    ScrollRegion: Rectangle;

		END (*RECORD*);

(************************************************************************)

VAR
    (* Maximum screen coordinates and colour for the selected mode.	*)
    (* (Minimum value is zero in all three cases.)			*)

    XMAX, YMAX, CharHeight: CARDINAL;
    MaxColour: ColourType;

(************************************************************************)
(*			OPENING AND CLOSING WINDOWS			*)
(************************************************************************)

PROCEDURE DrawFrame (w: Window);

    (* Draws the window frame.  Used when first opening a window, or	*)
    (* for putting back the frame after clearing the window.		*)

    VAR R: Rectangle;

    BEGIN
	WITH w^ DO
	    AddRectangle (tiles, absborder, foreground);
	    IF databorder.left <> absborder.left+1 THEN
		R := databorder;
		WITH R DO
		    DEC(left);  INC (right);
		    DEC(bottom);  INC (top);
		END (*WITH*);
		AddRectangle (tiles, R, foreground);
	    END (*IF*);
	END (*WITH*);
    END DrawFrame;

(************************************************************************)

PROCEDURE OpenWindow (VAR (*OUT*) w: Window;
				left, bottom, right, top: CARDINAL;
				Foregrnd, Backgrnd: ColourType;
				b: BorderType);

    (* Creates a new window.	*)

    VAR temp: CARDINAL;

    BEGIN
	(* Make sure that the parameters are within the legal ranges.	*)

	IF left > XMAX THEN left := XMAX END(*IF*);
	IF right > XMAX THEN right := XMAX END(*IF*);
	IF left > right THEN
	    temp := left;  left := right;  right := temp;
	END (*IF*);
	IF bottom > YMAX THEN bottom := YMAX END(*IF*);
	IF top > YMAX THEN top := YMAX END(*IF*);
	IF bottom > top THEN
	    temp := bottom;  bottom := top;  top := temp;
	END (*IF*);

	IF Backgrnd > MaxColour THEN Backgrnd := MaxColour END (*IF*);
	IF Foregrnd > MaxColour THEN Foregrnd := MaxColour END (*IF*);
	IF Foregrnd = Backgrnd THEN
	    Foregrnd := (Backgrnd + 1) MOD (MaxColour + 1);
	END (*IF*);

	NEW (w);
	WITH w^ DO
	    background := Backgrnd;  foreground := Foregrnd;
	    absborder.left := left;
	    absborder.right := right;
	    absborder.bottom := bottom;
	    absborder.top := top;
	    databorder := absborder;
	    IF b = double THEN temp := 3
	    ELSE temp := 1;
	    END (*IF*);
	    WITH databorder DO
		INC (left, temp);  DEC (right, temp);
		INC (bottom, temp);  DEC (top, temp);
	    END (*WITH*);
	    WITH ScrollRegion DO
		left := 0;  top := 0;

		(* The following two lines might need fine tuning.	*)

		right := (databorder.right - databorder.left) DIV 8 - 1;
		bottom := (databorder.top - databorder.bottom)
					DIV VAL(INTEGER,CharHeight) - 1;

	    END (*WITH*);
	    tiles := CreateTileSet (absborder, background);
	END (*WITH*);

	DrawFrame (w);
	SetCursor (w, 0, 0);

    END OpenWindow;

(************************************************************************)

PROCEDURE OpenWindowR (VAR (*OUT*) w: Window;  location: Rectangle;
					Foregrnd, Backgrnd: ColourType;
					b: BorderType);

    (* Same as OpenWindow, except for method of specifying location.	*)

    BEGIN
	WITH location DO
	    IF left < 0 THEN left := 0 END(*IF*);
	    IF right < 0 THEN right := 0 END(*IF*);
	    IF bottom < 0 THEN bottom := 0 END(*IF*);
	    IF top < 0 THEN top := 0 END(*IF*);
	    OpenWindow (w, left, bottom, right, top, Backgrnd, Foregrnd, b);
	END (*WITH*);
    END OpenWindowR;

(************************************************************************)

PROCEDURE WindowMemory (w: Window;  memory: BOOLEAN);

    (* Specifying a FALSE value for the memory parameter means that	*)
    (* subsequent data sent to this window will be written to the	*)
    (* screen but not remembered.  This saves time and memory, the only	*)
    (* penalty being that data covered by an overlapping window will	*)
    (* be lost.  Specifying TRUE restores the default condition, where	*)
    (* all window data are retained for refreshing the screen when	*)
    (* necessary.							*)

    BEGIN
	TileSetMemory (w^.tiles, memory);
    END WindowMemory;

(************************************************************************)

PROCEDURE CloseWindow (VAR (*INOUT*) w: Window);

    (* Destroys the window.	*)

    BEGIN
	DiscardTileSet (w^.tiles);
	DISPOSE (w);
    END CloseWindow;

(************************************************************************)
(*			     CLEARING A WINDOW				*)
(************************************************************************)

PROCEDURE ClearWindow (w: Window);

    (* Erases all data from w, but keeps it open.	*)

    BEGIN
	ClearTileSet (w^.tiles);
	DrawFrame (w);  SetCursor (w, 0, 0);
    END ClearWindow;

(************************************************************************)
(*			SETTING THE FOREGROUND COLOUR			*)
(************************************************************************)

PROCEDURE SetColour (w: Window;  colour: ColourType);

    (* Specifies the foreground colour to be used until further notice.	*)

    BEGIN
	w^.foreground := colour;
    END SetColour;

(************************************************************************)
(*			     PLOTTING A POINT				*)
(************************************************************************)

PROCEDURE PutPixelC (w: Window;  p: Point;  c: ColourType);

    (* Plots a dot at the point (x,y) in window w.  The coordinates are	*)
    (* relative to the bottom left of the window.  If the dot lies	*)
    (* outside the window it will be ignored.				*)

    BEGIN
	WITH w^ DO
	    INC (p.x, absborder.left);
	    INC (p.y, absborder.bottom);
	    IF Inside (p.x, p.y, databorder) THEN
		AddPoint (tiles, p, c);
	    END (*WITH*);
	END (*WITH*);
    END PutPixelC;

(************************************************************************)

PROCEDURE PutPixel (w: Window;  p: Point);

    (* Plots a dot at the point (x,y) in window w.  The coordinates are	*)
    (* relative to the bottom left of the window.  If the dot lies	*)
    (* outside the window it will be ignored.				*)

    BEGIN
	WITH w^ DO
	    INC (p.x, absborder.left);
	    INC (p.y, absborder.bottom);
	    IF Inside (p.x, p.y, databorder) THEN
		AddPoint (tiles, p, foreground);
	    END (*WITH*);
	END (*WITH*);
    END PutPixel;

(************************************************************************)

PROCEDURE PutPixel2 (w: Window;  x, y: INTEGER);

    (* Same as PutPixel, with a different way of specifying the point.	*)

    VAR p: Point;

    BEGIN
	p.x := x;  p.y := y;
	PutPixel (w, p);
    END PutPixel2;

(************************************************************************)

PROCEDURE PutPixel2C (w: Window;  x, y: INTEGER;  colour: ColourType);

    (* Same as PutPixel2, with the colour explicitly specified.	*)

    VAR p: Point;

    BEGIN
	p.x := x;  p.y := y;
	PutPixelC (w, p, colour);
    END PutPixel2C;

(************************************************************************)
(*			DRAWING A STRAIGHT LINE				*)
(************************************************************************)

PROCEDURE LineC (w: Window;  start, end: Point;  colour: ColourType);

    (* Draws a straight line.  The points are relative to the bottom	*)
    (* left corner of w.  Parts of the line lying outside the window	*)
    (* are clipped.							*)

    BEGIN
	WITH w^ DO
	    WITH absborder DO
		INC (start.x, left);  INC (start.y, bottom);
		INC (end.x, left);  INC (end.y, bottom);
	    END (*WITH*);
	    IF TrimLine (start, end, databorder) THEN
		AddLine (tiles, start, end, colour);
	    END (*IF*);
	END (*WITH*);
    END LineC;

(************************************************************************)

PROCEDURE Line (w: Window;  start, end: Point);

    (* Draws a straight line.  The points are relative to the bottom	*)
    (* left corner of w.  Parts of the line lying outside the window	*)
    (* are clipped.							*)

    BEGIN
	LineC (w, start, end, w^.foreground);
    END Line;

(************************************************************************)

PROCEDURE Line2 (w: Window;  xstart, ystart, xend, yend: INTEGER);

    (* The same operation as Line, with a different way of specifying	*)
    (* the parameters.							*)

    VAR start, end: Point;

    BEGIN
	start.x := xstart;  start.y := ystart;
	end.x := xend;  end.y := yend;
	Line (w, start, end);
    END Line2;

(************************************************************************)

PROCEDURE Line2C (w: Window;  xstart, ystart, xend, yend: INTEGER;
							colour: ColourType);

    (* The same operation as Line2, but with the colour explicitly	*)
    (* specified.							*)

    VAR start, end: Point;

    BEGIN
	start.x := xstart;  start.y := ystart;
	end.x := xend;  end.y := yend;
	LineC (w, start, end, colour);
    END Line2C;

(************************************************************************)

PROCEDURE StringLength (VAR (*IN*) text: ARRAY OF CHAR): CARDINAL;

    (* Finds the true size of the text string, given that there	*)
    (* could be a nul terminator in the middle.			*)

    VAR count: CARDINAL;

    BEGIN
	count := 0;
	WHILE (count <= HIGH(text)) AND (text[count] <> CHR(0)) DO
	    INC (count);
	END (*WHILE*);
	RETURN count;
    END StringLength;

(************************************************************************)

PROCEDURE GString (w: Window;  x, y: CARDINAL;  text: ARRAY OF CHAR);

    (* Writes a horizontal character string at graphics position (x,y)	*)
    (* relative to window w.  Characters which do not fit are not	*)
    (* displayed.  This is not considered to be a text operation since	*)
    (* the text cursor is not affected and there is no line wrap.	*)

    VAR count: CARDINAL;  place: Point;

    BEGIN
	count := StringLength (text);
	IF count = 0 THEN RETURN END(*IF*);

	(* Add the string to the TileSet.	*)

	WITH w^ DO
	    place.x := x + CARDINAL (absborder.left);
	    place.y := y + CARDINAL (absborder.bottom);
	    AddString (tiles, place, text, count, foreground, databorder);
	END (*WITH*);

    END GString;

(************************************************************************)

PROCEDURE GStringUp (w: Window;  x, y: CARDINAL;  text: ARRAY OF CHAR);

    (* Like GString, but the string is rotated counterclockwise by	*)
    (* 90 degrees, i.e. it is written in the +Y direction.		*)

    VAR count: CARDINAL;  place: Point;

    BEGIN
	count := StringLength (text);
	IF count = 0 THEN RETURN END(*IF*);

	(* Add the string to the TileSet.	*)

	WITH w^ DO
	    place.x := x + CARDINAL (absborder.left);
	    place.y := y + CARDINAL (absborder.bottom);
	    AddRotatedString (tiles, place, text, count, foreground,
							databorder);
	END (*WITH*);

    END GStringUp;

(************************************************************************)
(*			    TEXT OPERATIONS				*)
(************************************************************************)
(*									*)
(*  Every open window has a "text cursor" which is used only for text	*)
(*  operations and is independent of any operations on dots and lines.	*)
(*  The text cursor is updated after any text operation in such a way	*)
(*  that the characters follow one another in the way one would expect	*)
(*  for non-graphics windows.						*)
(*									*)
(************************************************************************)

PROCEDURE ComputeGraphicCursor (w: Window);

    (* Converts character position to graphics coordinates in w^.cursor	*)

    BEGIN
	WITH w^ DO
	    WITH cursor DO
		x := databorder.left + 8*column + 3;
		y := databorder.top - VAL(INTEGER,CharHeight)*(row+1) - 2;
	    END (*WITH*);
	END (*WITH*);
    END ComputeGraphicCursor;

(************************************************************************)

PROCEDURE SetCursor (w: Window;  row, column: CARDINAL);

    (* Sets the text cursor to the specified row and column.  The row	*)
    (* and column are measured in units of characters (not pixels),	*)
    (* with (0,0) representing the first character position at the	*)
    (* upper left of the window.					*)

    BEGIN
	w^.row := row;  w^.column := column;
	ComputeGraphicCursor (w);
    END SetCursor;

(************************************************************************)

PROCEDURE SaveCursor (w: Window;  VAR (*OUT*) row, column: CARDINAL);

    (* Returns the current row and column of the text cursor. *)

    BEGIN
	row := w^.row;  column := w^.column;
    END SaveCursor;

(************************************************************************)

PROCEDURE ScrollUp (w: Window);

    (* Moves all the text in w up by one line, discarding what falls	*)
    (* off the top.							*)

    BEGIN
	ScrollContents (w^.tiles, CharHeight, w^.databorder);
    END ScrollUp;

(************************************************************************)

PROCEDURE WriteLn (w: Window);

    (* Sets the text cursor to the start of the next text line down.	*)
    (* If the cursor reaches the bottom of the window, the text in the	*)
    (* window is scrolled.						*)

    BEGIN
	WITH w^ DO
	    column := ScrollRegion.left;
	    IF row >= ScrollRegion.bottom THEN ScrollUp (w)
	    ELSE INC (row);
	    END (*IF*);
	END (*WITH*);
	ComputeGraphicCursor (w);
    END WriteLn;

(************************************************************************)

PROCEDURE WriteString (w: Window;  text: ARRAY OF CHAR);

    (* Writes a horizontal character string at the current text cursor	*)
    (* position for window w.  Characters which do not fit on the	*)
    (* current line are wrapped around to a new row.			*)

    VAR count, amount, pos, j, canfit: CARDINAL;

    BEGIN
	amount := StringLength (text);
	pos := 0;
	WITH w^ DO
	    WITH ScrollRegion DO
		IF (amount = 0) OR (right < left) THEN RETURN END(*IF*);
	    END (*WITH*);
	    LOOP
		IF column > ScrollRegion.right THEN
		    WriteLn (w);
		END (*IF*);
		canfit := ScrollRegion.right - column + 1;
		IF amount > canfit THEN count := canfit
		ELSE count := amount;
		END (*IF*);

		AddString (tiles, cursor, text, count, foreground, databorder);
		INC (column, count);  INC (cursor.x, 8*count);

		DEC (amount, count);
		IF amount = 0 THEN EXIT(*LOOP*) END(*IF*);

		WriteLn (w);
		INC (pos, count);
		FOR j := 0 TO amount-1 DO
		    text[j] := text[count+j];
		END (*FOR*);

	    END (*LOOP*);

	END (*WITH*);

    END WriteString;

(************************************************************************)

PROCEDURE WriteChar (w: Window;  ch: CHAR);

    (* Writes a horizontal character at the current text cursor		*)
    (* position for window w.  The text cursor is updated.		*)

    VAR buffer: ARRAY [0..0] OF CHAR;

    BEGIN
	buffer[0] := ch;
	WriteString (w, buffer);
    END WriteChar;

(************************************************************************)
(*				SHUTDOWN				*)
(************************************************************************)

PROCEDURE ShutDown;

    VAR w: Window;  dummy: CHAR;
	message: ARRAY [0..79] OF CHAR;
	
    BEGIN
	(* For abnormal termination, put an error diagnostic on the	*)
	(* screen.							*)

	IF TerminationMessage(message) THEN
	    OpenWindow (w, 0,0, 300,30, 0, MaxColour, single);
	    WriteString (w, message);
	    dummy := InKey();
	    CloseWindow (w);
	END (*IF*);

    END ShutDown;

(************************************************************************)
(*			    INITIALISATION				*)
(************************************************************************)

PROCEDURE InitGraphics (mode: CARDINAL);

    (* Sets up the Graphics mode.  Optional, since the module starts up	*)
    (* with a best estimate of the "best" mode possible on the		*)
    (* available hardware.						*)

    BEGIN
	SetMode (mode, TRUE);
	GetScreenShape (XMAX, YMAX, MaxColour, CharHeight);
    END InitGraphics;

(************************************************************************)

BEGIN
    SetDefaultMode;
    GetScreenShape (XMAX, YMAX, MaxColour, CharHeight);
    SetTerminationProcedure (ShutDown);
END GWindows.
