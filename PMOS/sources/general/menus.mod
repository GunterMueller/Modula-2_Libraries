IMPLEMENTATION MODULE Menus;

	(****************************************************************)
	(*								*)
	(*	Displays menus on screen, allows terminal user to	*)
	(*			select from them.			*)
	(*								*)
	(*  Programmer:		P. Moylan				*)
	(*  Last edited:	21 February 1995			*)
	(*  Status:		OK					*)
	(*								*)
	(****************************************************************)

FROM Storage1 IMPORT
    (* proc *)	ALLOCATE, DEALLOCATE;

FROM TaskControl IMPORT
    (* type *)	Lock,
    (* proc *)	CreateLock, Obtain, Release;

FROM Keyboard IMPORT
    (* proc *)	PutBack, StuffKeyboardBuffer;

FROM Windows IMPORT
    (* const*)	MaxColumnNumber,
    (* type *)	Window, FrameType, DividerType,
    (* proc *)	OpenWindow, CloseWindow, ChangeScrollingRegion,
		GetKey, WriteChar, WriteString, SetColours, Blink,
		SetCursor, SaveCursor, ScrollUp, ScrollDown, EraseLine,
		NewScrollingRegion, ResetScrollingRegion;

FROM TextLines IMPORT
    (* type *)	LineType,
    (* proc *)	Box, HLine;

FROM Mouse IMPORT
    (* type *)	Buttons, ButtonSet,
    (* proc *)	MouseAvailable, HideMouseCursor, ShowMouseCursor;

FROM UserInterface IMPORT
    (* type *)	UIWindow, Capability, CapabilitySet,
    (* proc *)	AllowMouseControl, AddActiveRegion;

(************************************************************************)

CONST
    gap = 1;			(* space between menu columns *)
    ClickIndicator = CHR(1);	(* special code to indicate mouse click	*)
    LeftOnly = ButtonSet{LeftButton};
    Esc = CHR(01BH);		(* keyboard Escape character *)
    NilWindow = Window(NIL);

TYPE
    (* An ItemBuffer record holds the text for the screen display of	*)
    (* one menu item.  The "selpos" field shows which character acts	*)
    (* as the selection character.					*)

    ItemBuffer = RECORD
		     selpos: CARDINAL;
		     text: ItemText;
		 END (*RECORD*);

CONST
    MaxItems = MAX(CARDINAL) DIV SIZE(ItemBuffer);

TYPE
    ItemNo = [0..MaxItems];

    (* The following declaration uses a large upper subscript bound	*)
    (* because Modula-2 provides no way of declaring a variable-length	*)
    (* array (except as a procedure parameter).  The correct array size	*)
    (* will be established on a call to ALLOCATE.			*)

    TextPointer = POINTER TO ARRAY [1..MaxItems] OF ItemBuffer;

    Menu = POINTER TO MenuDetails;

    (********************************************************************)
    (*									*)
    (* The fields in a MenuDetails record have the following meaning:	*)
    (*									*)
    (*	win		The window used to display this menu		*)
    (*			 on the screen					*)
    (*	foreground,	The colours to use when displaying the menu	*)
    (*	 background,							*)
    (*	 selchar,							*)
    (*	 highforeground,						*)
    (*	 highbackground							*)
    (*	ScreenPosition	The row and column numbers which will be	*)
    (*			 occupied by the menu on the screen.  This	*)
    (*			 includes space for the border and title, if	*)
    (*			 present.  The space taken by the menu itself,	*)
    (*			 not including border and title, is given by	*)
    (*			 the LocationInWindow record - see below.	*)
    (*	heading		The text to display in the menu header		*)
    (*	NoOfItems	Number of menu items				*)
    (*	ItemsPerColumn  Number of items allocated to each column	*)
    (*	NoOfColumns	Number of columns of menu items			*)
    (*	hstep		The increment in item number resulting from	*)
    (*			 one "cursor right" operation.			*)
    (*	vstep		The increment in item number resulting from	*)
    (*			 one "cursor down" operation.  Note that hstep	*)
    (*			 and vstep depend on whether the items are	*)
    (*			 numbered horizontally or vertically, and that	*)
    (*			 in turn depends on whether we have a short fat	*)
    (*			 display or a tall thin one.			*)
    (*	LocationInWindow: The location of the menu itself, relative to	*)
    (*			 the window in which it is displayed, and not	*)
    (*			 counting the space taken by borders or a	*)
    (*			 header.  The top left character is at location	*)
    (*			 (firstrow,firstcol), the menu takes 'height'	*)
    (*			 screen lines and is 'width' characters wide.	*)
    (*			 To fit the text exactly into this space, we	*)
    (*			 would need					*)
    (*			    NoOfItems = height*NoOfColumns		*)
    (*			 If NoOfItems is smaller than this, the menu	*)
    (*			 will be displayed with some blank areas.  If	*)
    (*			 it is larger, the menu scrolls.		*)
    (*	ColumnWidth	The number of characters of TextPtr^[j] which	*)
    (*			 will be written on the screen.  Approximately	*)
    (*			 equal to LocationInWindow.width/NoOfColumns.	*)
    (*	ShowTitle	Says whether to display a menu title.		*)
    (*	ShowBorder	Says whether to put a border around the menu.	*)
    (*	CloseAfterSelection  If TRUE, specifies that the menu window	*)
    (*			 will be closed on return from SelectFromMenu.	*)
    (*	PutBackExitKey	If TRUE, the keystroke that caused a return	*)
    (*			 from SelectFromMenu remains available to the	*)
    (*			 caller.  If FALSE, that key is consumed.	*)
    (*	FastSelect	If TRUE, user can select a menu item without	*)
    (*			 having to confirm with Enter or Space.		*)
    (*	MouseControl	If TRUE, user can move the menu with the mouse.	*)
    (*	offL, offR,	What to do when the user tries to run off the	*)
    (*	  offT, offB	left/right/top/bottom edge of the menu.		*)
    (*  CurrentItemNo	The item number currently selected.		*)
    (*  row		The current menu row selected.  Note that row=1	*)
    (*			 means the top row of items displayed; this is	*)
    (*			 not necessarily the top row of the entire menu	*)
    (*			 since some items may have scrolled off the top.*)
    (*  column		The current menu column selected.		*)
    (*	ExtraAtTop	The number of rows which have disappeared off	*)
    (*				the top of the display.			*)
    (*	ExtraAtBottom	The number of rows which have disappeared off	*)
    (*				the bottom of the display.		*)
    (*	RanOffEdge	Set if a cursor movement ran us off the edge	*)
    (*				of the menu.				*)
    (*	TextPtr^[j]	The text to display for menu item j.		*)
    (*									*)
    (********************************************************************)

    MenuDetails =

	   RECORD
		(* The screen window. *)

		win: Window;
		foreground, background, selchar,
			highforeground, highbackground: Colour;
		ScreenPosition:
		    RECORD
			firstrow, lastrow: RowRange;
			firstcol, lastcol: ColumnRange;
		    END (*RECORD*);
		heading: ItemText;

		(* The layout of the menu within its window, determined	*)
		(* at the time that the menu is created.		*)

		NoOfItems: ItemNo;
		ItemsPerColumn: ItemNo;
		NoOfColumns: MenuColumn;

		(* Further layout details, determined at the time we	*)
		(* discover how much space the menu will be given on	*)
		(* the screen.						*)

		hstep, vstep: CARDINAL;
		LocationInWindow:
		    RECORD
			firstrow, firstcol: CARDINAL;
			height, width: CARDINAL;
		    END (*RECORD*);
		ColumnWidth: ColumnRange;

		(* Options. *)

		ShowTitle, ShowBorder, CloseAfterSelection,
			PutBackExitKey, FastSelect, MouseControl: BOOLEAN;
		offL, offR, offT, offB: OffEdgeOption;

		(* Information about the current state of the menu.	*)

		CurrentItemNo: ItemNo;
		row: RowRange;
		column: MenuColumn;
		ExtraAtTop, ExtraAtBottom: CARDINAL;
		RanOffEdge: BOOLEAN;

		(* Pointer to the text of the menu items.	*)

		TextPtr: TextPointer;

	    END (*RECORD*);

(************************************************************************)

VAR
    (* The following record contains the details of the last mouse	*)
    (* click.  Because users can't move a mouse very fast, we don't	*)
    (* bother to keep a queue of clicks, we just record the last seen.	*)
    (* In the event that a click arrives before the last has been	*)
    (* consumed, the earlier click is lost.  I haven't yet seen an	*)
    (* application where that has been a major problem.			*)

    LastMouseClick: RECORD
			access: Lock;
			win: Window;
			X: ColumnRange;  Y: RowRange;
			valid: BOOLEAN;
		    END (*RECORD*);

(************************************************************************)
(*			MISCELLANEOUS UTILITIES				*)
(************************************************************************)

PROCEDURE Setselpos (VAR (*INOUT*) item: ItemBuffer);

    (* Looks for a "\" in the item text, adjusts item.selpos if found.	*)

    VAR j, k, high: CARDINAL;

    BEGIN
	j := 0;  high := HIGH(item.text);
	LOOP
	    IF (j > high) OR (ORD(item.text[j]) = 0) THEN
		EXIT (*LOOP*);
	    ELSIF item.text[j] = "\" THEN
		item.selpos := j;
		FOR k := j TO high-1 DO
		    item.text[k] := item.text[k+1];
		END (*FOR*);
		item.text[high] := CHR(0);
		EXIT (*LOOP*);
	    ELSE
		INC (j);
	    END (*IF*);
	END (*LOOP*);
    END Setselpos;

(************************************************************************)

PROCEDURE resize (VAR (*INOUT*) text: ItemText;  size: CARDINAL);

    (* Makes text equal to the given size, by space filling on the	*)
    (* right and inserting a Nul to terminate the text.			*)

    VAR j: CARDINAL;

    BEGIN
	j := 0;
	WHILE (j < size) AND (ORD(text[j]) <> 0) DO
	    INC(j);
	END (*WHILE*);
	WHILE j < size DO text[j] := " "; INC(j) END (*WHILE*);
	IF j <= MaxColumnNumber THEN text[j] := CHR(0) END (*IF*);
    END resize;

(************************************************************************)
(*			   CREATING A MENU				*)
(************************************************************************)

PROCEDURE CreateMenu (VAR (*OUT*) M: Menu; columns: MenuColumn;
			VAR (*IN*) Messages: ARRAY OF ItemText;
			NumberOfItems: CARDINAL);

    (* Introduces a menu into the system, but does not display it yet.	*)

    VAR j, count: CARDINAL;

    BEGIN
	NEW (M);
	WITH M^ DO
	    win := NilWindow;
	    NoOfColumns := columns;

	    (* Store the header text, with space fill.	*)

	    heading := Messages[0];
	    resize (heading, MaxColumnNumber);

	    (* Work out how many menu items there are.	*)

	    count := NumberOfItems;
	    IF (count = 0) OR (count > HIGH (Messages)) THEN
		count := HIGH (Messages);
	    END (*IF*);
	    NoOfItems := count;
	    ItemsPerColumn := (count + NoOfColumns - 1) DIV NoOfColumns;
	    CurrentItemNo := 1;

	    (* Store the item text.	*)

	    ALLOCATE (TextPtr, NoOfItems*SIZE(ItemBuffer));
	    FOR j := 1 TO NoOfItems DO
		WITH TextPtr^[j] DO
		    selpos := 0;
		    text := Messages[j];
		END (*WITH*);
		Setselpos (TextPtr^[j]);
	    END (*FOR*);

	    (* Set default options. *)

	    ShowTitle := TRUE;  ShowBorder := TRUE;
	    CloseAfterSelection := TRUE;  PutBackExitKey := FALSE;
	    FastSelect := FALSE;  MouseControl := MouseAvailable();
	    offL := stick;  offR := stick;  offT := stick;  offB := stick;

	END (*WITH*);

	(* Give the menu a default initial position, size, and colour.	*)

	PositionMenu (M, 0, 10, 0, MaxColumnNumber);
	MenuColours (M, blue, white, black, cyan, red);

    END CreateMenu;

(************************************************************************)

PROCEDURE MenuColours (M: Menu;  fore, back, hfore, hback, select: Colour);

    (* Set the colours for the screen display of the menu.  The colours	*)
    (* fore and back are used as the normal foreground and background	*)
    (* colours, and the highlighted menu item is displayed in colours	*)
    (* hfore, hback.  The "select" colour is for highlighting the	*)
    (* selection character.						*)

    BEGIN
	WITH M^ DO
	    foreground := fore;  background := back;
	    highforeground := hfore;  highbackground := hback;
	    selchar := select;
	END (*WITH*);
    END MenuColours;

(************************************************************************)

PROCEDURE SetOptions (M: Menu;  options: MO);

    (* See the MenuOption declaration for the possible options. *)

    BEGIN
	WITH M^ DO
	    IF MTitle IN options THEN ShowTitle := TRUE END (*IF*);
	    IF MNoTitle IN options THEN ShowTitle := FALSE END (*IF*);
	    IF MBorder IN options THEN ShowBorder := TRUE END (*IF*);
	    IF MNoBorder IN options THEN ShowBorder := FALSE END (*IF*);
	    IF MClose IN options THEN CloseAfterSelection := TRUE END (*IF*);
	    IF MNoClose IN options THEN CloseAfterSelection := FALSE END (*IF*);
	    IF MKeyBack IN options THEN PutBackExitKey := TRUE END (*IF*);
	    IF MNoKeyBack IN options THEN PutBackExitKey := FALSE END (*IF*);
	    IF MFastSelect IN options THEN FastSelect := TRUE END (*IF*);
	    IF MNoFastSelect IN options THEN FastSelect := FALSE END (*IF*);
	    IF MMouse IN options THEN MouseControl := MouseAvailable() END (*IF*);
	    IF MNoMouse IN options THEN MouseControl := FALSE END (*IF*);
	END (*WITH*);
    END SetOptions;

(************************************************************************)

PROCEDURE OffEdge (M: Menu;  top, bottom, left, right: OffEdgeOption);

    (* Sets the menu behaviour when the user runs the cursor off the	*)
    (* edge of the menu.  There is one parameter for each edge of the	*)
    (* menu.								*)
    (* See the OffEdgeOption type declaration for the possible options.	*)

    BEGIN
	WITH M^ DO
	    offT := top;  offB := bottom;
	    offL := left;  offR := right;
	END (*WITH*);
    END OffEdge;

(************************************************************************)
(*			  POSITIONING A MENU				*)
(************************************************************************)

PROCEDURE SetRelativeLocation (M: Menu;  row1, col1, rows, columns: CARDINAL);

    (* Gives initial values to M^.LocationInWindow and M^.ColumnWidth,	*)
    (* and resizes the item text to the space available.  Also sets	*)
    (* M^.hstep and M^.vstep, based on the following criterion: if the	*)
    (* display will be wider than it is tall then we use row major	*)
    (* ordering (hstep = 1), whereas for tall narrow menus we use	*)
    (* column major ordering (vstep = 1).  This distinction is actually	*)
    (* irrelevant to the caller, but it affects the appearance of the	*)
    (* menu, and the decision taken here seems to give a result which	*)
    (* someone reading the screen would consider intuitively logical.	*)

    VAR j: ItemNo;

    BEGIN
	WITH M^ DO
	    WITH LocationInWindow DO
		firstrow := row1;  firstcol := col1;
		height := rows;  width := columns;
		IF ItemsPerColumn <= height THEN
		    height := ItemsPerColumn;
		END (*IF*);
		IF NoOfColumns > height THEN
		    hstep := 1;  vstep := NoOfColumns;
		ELSE
		    hstep := ItemsPerColumn;  vstep := 1;
		END (*IF*);
	    END (*WITH*);

	    resize (heading, columns);
	    ColumnWidth := (columns - (NoOfColumns-1)*gap) DIV NoOfColumns;
	    FOR j := 1 TO NoOfItems DO
		resize (TextPtr^[j].text, ColumnWidth);
	    END (*FOR*);

	END (*WITH*);

    END SetRelativeLocation;

(************************************************************************)

PROCEDURE PositionMenu (M: Menu;  startline, endline: RowRange;
				leftcol, rightcol: ColumnRange);

    (* Sets the screen location of the window which will hold the menu.	*)

    VAR row1, col1, height, width: CARDINAL;

    BEGIN
	WITH M^ DO

	    (* Work out the space available on the screen.	*)

	    WITH ScreenPosition DO
		firstrow := startline;  lastrow := endline;
		firstcol := leftcol;  lastcol := rightcol;
	    END (*WITH*);

	    (* How much of this space is used for the actual menu? *)

	    row1 := 0;  col1 := 0;
	    height := endline - startline + 1;
	    width := rightcol - leftcol + 1;
	    IF ShowBorder THEN
		INC (row1);  INC(col1);
		DEC (height, 2);  DEC (width, 2);
	    END(*IF*);
	    IF ShowTitle THEN
		INC (row1,2);  DEC (height, 2);
	    END(*IF*);
	    SetRelativeLocation (M, row1, col1, height, width);

	END (*WITH*);
    END PositionMenu;

(************************************************************************)
(*			    CLOSING A MENU				*)
(************************************************************************)

PROCEDURE DestroyMenu (M: Menu);

    (* Removes a menu from the system, freeing up the space it used.	*)

    BEGIN
	WITH M^ DO
	    IF (win <> NilWindow) AND CloseAfterSelection THEN
		CloseWindow (win);
	    END (*IF*);
	    DEALLOCATE (TextPtr, NoOfItems*SIZE(ItemBuffer));
	END (*WITH*);
	DISPOSE (M);
    END DestroyMenu;

(************************************************************************)
(*			    SCREEN DISPLAY				*)
(************************************************************************)

PROCEDURE NewColours (M: Menu;  fore, back, select: Colour);

    (* Changes the foreground and background colours of the current	*)
    (* menu item.  The "select" colour is for highlighting the		*)
    (* selection character.						*)

    BEGIN
	IF MouseAvailable() THEN HideMouseCursor END (*IF*);
	WITH M^ DO
	    SetColours (win, LocationInWindow.firstrow+row-1,
		(column-1)*(ColumnWidth+gap) + LocationInWindow.firstcol,
			ColumnWidth, fore, back);
	    IF CurrentItemNo <= NoOfItems THEN
		SetColours (win, LocationInWindow.firstrow+row-1,
			(column-1)*(ColumnWidth+gap) + LocationInWindow.firstcol
					+ TextPtr^[CurrentItemNo].selpos,
			1, select, back);
	    END (*IF*);
	END (*WITH*);
	IF MouseAvailable() THEN ShowMouseCursor END (*IF*);
    END NewColours;

(************************************************************************)

PROCEDURE Highlight (M: Menu);

    (* Highlights the current menu item.	*)

    BEGIN
	NewColours (M, M^.highforeground, M^.highbackground, M^.selchar);
    END Highlight;

(************************************************************************)

PROCEDURE Unhighlight (M: Menu);

    (* Removes any highlighting from the current menu item.	*)

    BEGIN
	NewColours (M, M^.foreground, M^.background, M^.selchar);
    END Unhighlight;

(************************************************************************)

PROCEDURE RefreshRow (M: Menu);

    (* Refreshes the current menu row.	*)

    VAR screenrow: RowRange;  j: ColumnRange;  savecurrent: ItemNo;
	savecolumn: CARDINAL;

    BEGIN
	WITH M^ DO
	    savecurrent := CurrentItemNo;  savecolumn := column;
	    WITH LocationInWindow DO
		screenrow := firstrow + row - 1;
		j := LocationInWindow.firstcol;
	    END (*WITH*);
	    SetCursor (win, screenrow, j);  EraseLine (win, 1);
	    column := 1;
	    CurrentItemNo := 1 + vstep*(row + ExtraAtTop - 1);
	    LOOP
		WriteString (win, TextPtr^[CurrentItemNo].text);
		Unhighlight (M);
		IF (column = NoOfColumns)
			OR (CurrentItemNo+hstep > NoOfItems) THEN
		    EXIT (*LOOP*);
		END (*IF*);
		INC (column);
		INC (CurrentItemNo, hstep);  INC (j, ColumnWidth + gap);
		SetCursor (win, screenrow, j);
	    END (*LOOP*);
	    column := savecolumn;  CurrentItemNo := savecurrent;
	END (*WITH*);
    END RefreshRow;

(************************************************************************)

PROCEDURE DisplayMOREatTop (M: Menu);

    BEGIN
	WITH M^ DO
	    IF ShowTitle OR (ShowBorder AND NOT MouseControl) THEN
		WITH LocationInWindow DO
		    SetCursor (win, firstrow-1, firstcol+width-6);
		END (*WITH*);
		WriteString (win, "*MORE*");
	    END (*IF*);
	END (*WITH*);
    END DisplayMOREatTop;

(************************************************************************)

PROCEDURE RemoveMOREatTop (M: Menu);

    CONST DoubleBar = 'Í';  SingleBar = 'Ä';

    VAR j: [1..6];  bar: CHAR;

    BEGIN
	WITH M^ DO
	    IF ShowTitle OR (ShowBorder AND NOT MouseControl) THEN
		WITH LocationInWindow DO
		    SetCursor (win, firstrow-1, firstcol+width-6);
		END (*WITH*);
		IF ShowTitle THEN bar := DoubleBar
		ELSE bar := SingleBar
		END (*IF*);
		FOR j := 1 TO 6 DO
		    WriteChar (win, bar);
		END (*FOR*);
	    END (*IF*);
	END (*WITH*);
    END RemoveMOREatTop;

(************************************************************************)

PROCEDURE DisplayMOREatBottom (M: Menu);

    BEGIN
	WITH M^ DO
	    IF ShowBorder THEN
		WITH LocationInWindow DO
		    SetCursor (win, firstrow+height, firstcol+width-6);
		END (*WITH*);
		WriteString (win, "*MORE*");
	    END (*IF*);
	END (*WITH*);
    END DisplayMOREatBottom;

(************************************************************************)

PROCEDURE RemoveMOREatBottom (M: Menu);

    CONST HorizontalBar = 'Ä';

    VAR j: [1..6];

    BEGIN
	WITH M^ DO
	    IF ShowBorder THEN
		WITH LocationInWindow DO
		    SetCursor (win, firstrow+height, firstcol+width-6);
		END (*WITH*);
		FOR j := 1 TO 6 DO
		    WriteChar (win, HorizontalBar);
		END (*FOR*);
	    END (*IF*);
	END (*WITH*);
    END RemoveMOREatBottom;

(************************************************************************)

PROCEDURE BlinkCurrent (M: Menu);

    (* Toggles the blinking state of the current menu item.	*)

    BEGIN
	WITH M^ DO
	    Blink (win, LocationInWindow.firstrow+row-1,
		(column-1)*(ColumnWidth+gap) + LocationInWindow.firstcol,
			ColumnWidth);
	END (*WITH*);
    END BlinkCurrent;

(************************************************************************)
(*			    CURSOR MOVEMENTS				*)
(************************************************************************)

PROCEDURE DownARow (M: Menu);  FORWARD;

(************************************************************************)

PROCEDURE UpARow (M: Menu);

    (* Moves to the next item up, if present.  Scrolls if necessary.	*)

    BEGIN
	WITH M^ DO
	    IF row > 1 THEN
		DEC (row);  DEC (CurrentItemNo, vstep);
	    ELSIF ExtraAtTop > 0 THEN
		DEC (CurrentItemNo, vstep);
		ScrollDown (win);
		DEC (ExtraAtTop);  INC (ExtraAtBottom);
		RefreshRow (M);
		IF MouseAvailable() THEN HideMouseCursor END (*IF*);
		IF ExtraAtTop = 0 THEN RemoveMOREatTop(M) END (*IF*);
		IF ExtraAtBottom = 1 THEN DisplayMOREatBottom(M) END(*IF*);
		IF MouseAvailable() THEN ShowMouseCursor END (*IF*);
	    ELSE
		CASE offT OF
		  | stick:	(* no action needed*) ;
		  | wrap:	WHILE (row < LocationInWindow.height)
					OR (ExtraAtBottom > 0) DO
				    DownARow(M);
				END (*WHILE*);
		  | escape:	CurrentItemNo := 0;  RanOffEdge := TRUE;
		  | return:	RanOffEdge := TRUE;
		END (*CASE*);
	    END (*IF*);
	END (*WITH*);
    END UpARow;

(************************************************************************)

PROCEDURE DownARow (M: Menu);

    (* Moves to the next item down, if present.  Scrolls if necessary.	*)

    BEGIN
	WITH M^ DO
	    IF row < LocationInWindow.height THEN
		INC (row);  INC (CurrentItemNo, vstep);
	    ELSIF ExtraAtBottom > 0 THEN
		INC (CurrentItemNo, vstep);  ScrollUp (win);
		INC (ExtraAtTop);  DEC (ExtraAtBottom);
		RefreshRow (M);
		IF MouseAvailable() THEN HideMouseCursor END (*IF*);
		IF ExtraAtTop = 1 THEN DisplayMOREatTop(M) END (*IF*);
		IF ExtraAtBottom = 0 THEN RemoveMOREatBottom(M) END (*IF*);
		IF MouseAvailable() THEN ShowMouseCursor END (*IF*);
	    ELSE
		CASE offB OF
		  | stick:	(* no action needed*) ;
		  | wrap:	WHILE (row > 1) OR (ExtraAtTop > 0) DO
				    UpARow (M);
				END (*WHILE*);
		  | escape:	CurrentItemNo := 0;  RanOffEdge := TRUE;
		  | return:	RanOffEdge := TRUE;
		END (*CASE*);
	    END (*IF*);
	END (*WITH*);
    END DownARow;

(************************************************************************)

PROCEDURE MoveRight (M: Menu);

    (* Moves to the next item right, if present.	*)

    BEGIN
	WITH M^ DO
	    IF column < NoOfColumns THEN
		INC (column);  INC (CurrentItemNo, hstep);
	    ELSE
		CASE offR OF
		  | stick:	(* no action needed*) ;
		  | wrap:	DEC (CurrentItemNo, hstep*(column-1));
				column := 1;
		  | escape:	CurrentItemNo := 0;  RanOffEdge := TRUE;
		  | return:	RanOffEdge := TRUE;
		END (*CASE*);
	    END (*IF*);
	END (*WITH*);
    END MoveRight;

(************************************************************************)

PROCEDURE MoveLeft (M: Menu);

    (* Moves to the next item left, if present.	*)

    BEGIN
	WITH M^ DO
	    IF column > 1 THEN
		DEC (column);  DEC (CurrentItemNo, hstep);
	    ELSE
		CASE offL OF
		  | stick:	(* no action needed*) ;
		  | wrap:	INC (CurrentItemNo, hstep*(NoOfColumns-1));
				column := NoOfColumns;
		  | escape:	CurrentItemNo := 0;  RanOffEdge := TRUE;
		  | return:	RanOffEdge := TRUE;
		END (*CASE*);
	    END (*IF*);
	END (*WITH*);
    END MoveLeft;

(************************************************************************)

PROCEDURE GotoItem (M: Menu;  newitem: ItemNo);

    (* Moves to the menu item whose number is specified.  We move a row	*)
    (* at a time, rather than taking one big leap, since this is less	*)
    (* disconcerting to the user.					*)

    BEGIN
	WITH M^ DO
	    IF newitem <> CurrentItemNo THEN
		IF vstep = 1 THEN	(* we are using column major order *)
		    column := ((newitem-1) DIV hstep) + 1;
		ELSE			(* we are using row major order *)
		    column := ((newitem-1) MOD vstep) + 1;
		END (*IF*);
		CurrentItemNo := (column-1)*hstep
					+ (ExtraAtTop+row-1)*vstep + 1;
		WHILE CurrentItemNo > newitem DO UpARow(M) END (*WHILE*);
		WHILE CurrentItemNo < newitem DO DownARow(M) END (*WHILE*);
	    END (*IF*);
	END (*WITH*);
    END GotoItem;

(************************************************************************)

PROCEDURE RepositionTo (M: Menu;  searchchar: CHAR): BOOLEAN;

    (* Finds the next menu item whose selection character matches	*)
    (* selchar, and adjusts the display appropriately.  Returns TRUE if	*)
    (* searchchar was actually found; otherwise the current menu item	*)
    (* doesn't change and the function result is FALSE.			*)

    VAR j: ItemNo;  found: BOOLEAN;

    BEGIN
	WITH M^ DO
	    j := CurrentItemNo;
	    REPEAT
		IF j >= NoOfItems THEN j := 1
		ELSE INC (j)
		END (*IF*);
		WITH TextPtr^[j] DO
		    found := CAP(text[selpos]) = searchchar;
		END (*WITH*);
	    UNTIL found OR (j = CurrentItemNo);
	END (*WITH*);
	GotoItem (M, j);
	RETURN found;
    END RepositionTo;

(************************************************************************)

PROCEDURE HandleFunctionKey (M: Menu;  VAR (*INOUT*) option: CHAR);

    (* Deals with the case where the user typed a function key - i.e.	*)
    (* any key which produces a two-code sequence where the first code	*)
    (* is CHR(0).  On entry, the CHR(0) has already been read.		*)

    VAR count: CARDINAL;

    BEGIN
	WITH M^ DO
	    option := GetKey(win);
	    IF option = "H" THEN UpARow(M)		(* cursor up *)
	    ELSIF option = "P" THEN DownARow(M)		(* cursor down *)
	    ELSIF option = "M" THEN MoveRight(M)	(* cursor right *)
	    ELSIF option = "K" THEN MoveLeft(M)		(* cursor left *)
	    ELSIF option = "G" THEN			(* home *)
		GotoItem (M, 1);
	    ELSIF option = "O" THEN			(* end *)
		GotoItem (M, NoOfColumns*ItemsPerColumn);
		GotoItem (M, NoOfItems);
	    ELSIF option = "I" THEN			(* page up *)
		IF row = 1 THEN
		    IF ExtraAtTop > 0 THEN
			count := LocationInWindow.height;
			REPEAT
			    UpARow(M);  DEC (count);
			UNTIL (count=0) OR (ExtraAtTop=0);
		    END (*IF*)
		ELSE
		    WHILE row > 1 DO UpARow(M) END (*WHILE*)
		END (*IF*)
	    ELSIF option = "Q" THEN			(* page down *)
		IF row = LocationInWindow.height THEN
		    IF ExtraAtBottom > 0 THEN
			count := LocationInWindow.height;
			REPEAT
			    DownARow(M);  DEC (count);
			UNTIL (count=0) OR (ExtraAtBottom=0);
		    END (*IF*)
		ELSE
		    WHILE row < LocationInWindow.height DO
			DownARow(M);
		    END (*WHILE*)
		END (*IF*)
	    END (*IF*);
	END (*WITH*);
    END HandleFunctionKey;

(************************************************************************)
(*			DEALING WITH MOUSE CLICKS			*)
(************************************************************************)

PROCEDURE SelectItemAt (M: Menu;  r: RowRange;  c: ColumnRange);

    VAR NewItemNo: ItemNo;  OnAnItem: BOOLEAN;

    BEGIN
	WITH M^ DO
	    WITH LocationInWindow DO
		DEC (r, firstrow);  DEC (c, firstcol);
	    END (*WITH*);
	    OnAnItem := c MOD (ColumnWidth+gap) < ColumnWidth;
	    c := c DIV (ColumnWidth+gap);
	    IF c >= NoOfColumns THEN OnAnItem := FALSE END(*IF*);

	    IF OnAnItem THEN
		(* We have now reduced (r,c) to be the coordinates of	*)
		(* an item in the visible part of the array, with (0,0)	*)
		(* corresponding to the top left position.		*)

		NewItemNo := c*hstep + (ExtraAtTop+r)*vstep + 1;

		(* The first click on an item simply means that we	*)
		(* should go to that item; a second click on the same	*)
		(* item means that we should accept it as the result.	*)
		(* If the FastSelect option is enabled, the first click	*)
		(* will select the item as the result.			*)

		IF FastSelect OR (NewItemNo = CurrentItemNo) THEN
		    PutBack (" ");
		END (*IF*);
		HideMouseCursor;
		GotoItem (M, NewItemNo);
		ShowMouseCursor;
	    END (*IF*);

	END (*WITH*);

    END SelectItemAt;

(************************************************************************)

PROCEDURE InterpretMouseClick (M: Menu);

    (* This procedure is called when we know that a mouse click has	*)
    (* been detected and its details stored in LastMouseClick.  This	*)
    (* procedure checks whether the click is relevant to menu M, and	*)
    (* takes the appropriate action if so.				*)

    VAR OK: BOOLEAN;  row: RowRange;  column: ColumnRange;

    BEGIN
	WITH LastMouseClick DO
	    Obtain (access);
	    OK := valid AND (win = M^.win);
	    IF OK THEN
		column := X;  row := Y;
	    END (*IF*);
	    valid := FALSE;
	    Release (access);
	END (*WITH*);
	IF OK THEN
	    SelectItemAt (M, row, column);
	END (*IF*);
    END InterpretMouseClick;

(************************************************************************)

PROCEDURE RecordClick (w: Window;  row: RowRange;  col: ColumnRange);

    (* This procedure is called asynchronously as the result of a mouse	*)
    (* click.  The parameters tell us which window was clicked on, and	*)
    (* where in that window the click occurred, but they don't tell us	*)
    (* which menu is involved.  Rather than work that out here, we	*)
    (* stuff a special character into the keyboard.  Procedure		*)
    (* MakeTheSelection will pick up that special character and from	*)
    (* that deduce that it needs to look at the LastMouseClick data.	*)

    BEGIN
	WITH LastMouseClick DO
	    Obtain (access);
	    win := w;
	    X := col;  Y := row;
	    valid := TRUE;
	    Release (access);
	    StuffKeyboardBuffer (ClickIndicator);
	END (*WITH*);
    END RecordClick;

(************************************************************************)

PROCEDURE ClickOnTop (w: Window;  row: RowRange;  col: ColumnRange);

    (* This procedure is triggered by a mouse click on the window	*)
    (* divider.  We turn this into a "cursor up" command.		*)

    BEGIN
	StuffKeyboardBuffer (CHR(0));
	StuffKeyboardBuffer ("H");
    END ClickOnTop;

(************************************************************************)

PROCEDURE ClickOnTopMORE (w: Window;  row: RowRange;  col: ColumnRange);

    (* This procedure is triggered by a mouse click on the top *MORE*	*)
    (* indicator.  We turn this into a "page up" command.		*)

    BEGIN
	StuffKeyboardBuffer (CHR(0));
	StuffKeyboardBuffer ("I");
    END ClickOnTopMORE;

(************************************************************************)

PROCEDURE ClickOnBottom (w: Window;  row: RowRange;  col: ColumnRange);

    (* This procedure is triggered by a mouse click on the bottom	*)
    (* of the window frame.  We turn this into a "cursor down" command.	*)

    BEGIN
	StuffKeyboardBuffer (CHR(0));
	StuffKeyboardBuffer ("P");
    END ClickOnBottom;

(************************************************************************)

PROCEDURE ClickOnBottomMORE (w: Window;  row: RowRange;  col: ColumnRange);

    (* This procedure is triggered by a mouse click on the bottom	*)
    (* *MORE* indicator.  We turn this into a "page down" command.	*)

    BEGIN
	StuffKeyboardBuffer (CHR(0));
	StuffKeyboardBuffer ("Q");
    END ClickOnBottomMORE;

(************************************************************************)
(*			    SCREEN DISPLAY				*)
(************************************************************************)

PROCEDURE OpeningDisplay (M: Menu;  initialvalue: CARDINAL);

    (* Sets up the initial state of the display of M.  It is assumed	*)
    (* that window M^.win is already open and that the position of the	*)
    (* menu has already been set.  It is also assumed that the border	*)
    (* and title, if needed, have already been displayed.		*)

    VAR j: RowRange;

    BEGIN
	WITH M^ DO
	    IF (initialvalue = 0) OR (initialvalue > NoOfItems) THEN
		initialvalue := 1;
	    END (*IF*);
	    RanOffEdge := FALSE;
	    CurrentItemNo := 1;  column := 1;  ExtraAtTop := 0;
	    ExtraAtBottom := ItemsPerColumn - LocationInWindow.height;
	    FOR j := 1 TO LocationInWindow.height DO
		row := j;  RefreshRow (M);
	    END (*FOR*);
	    row := 1;
	    IF (ExtraAtBottom > 0) THEN
		IF MouseAvailable() THEN HideMouseCursor END (*IF*);
		DisplayMOREatBottom (M);
		IF MouseAvailable() THEN ShowMouseCursor END (*IF*);
	    END (*IF*);
	END (*WITH*);
	GotoItem (M, initialvalue);
    END OpeningDisplay;

(************************************************************************)

PROCEDURE DisplayMenu (w: Window;  M: Menu;
				rows, columns, initialvalue: CARDINAL);

    (* Displays menu M at the current cursor position in window w,	*)
    (* with initialvalue specifying a field to highlight.		*)

    VAR row1, col1: CARDINAL;

    BEGIN
	SaveCursor (w, row1, col1);
	WITH M^ DO
	    win := w;
	    IF ShowBorder THEN
		Box (w, row1, col1, columns-1, rows-1, single);
		INC (row1);  INC (col1);
		DEC (rows, 2);  DEC (columns, 2);
	    END (*IF*);
	    IF ShowTitle THEN
		INC (row1, 2);  DEC (rows, 2);
		IF ShowBorder THEN
		    HLine (w, row1-1, col1-1, col1+columns, double);
		ELSE
		    HLine (w, row1-1, col1, col1+columns-1, double);
		END (*IF*);
	    END (*IF*);
	    SetRelativeLocation (M, row1, col1, rows, columns);
	    IF ShowTitle THEN
		SetCursor (w, row1-2, col1);  WriteString (w, heading);
	    END (*IF*);
	END (*WITH*);
	NewScrollingRegion (w, row1, row1+rows-1, col1, col1+columns-1);
	OpeningDisplay (M, initialvalue);
	Highlight (M);  ResetScrollingRegion (w);
    END DisplayMenu;

(************************************************************************)
(*		     MAKING A SELECTION FROM A MENU			*)
(************************************************************************)

PROCEDURE MakeTheSelection (M: Menu);

    (* Allows the keyboard user to alter the state of menu M by use of	*)
    (* the cursor control keys, or by typing the initial letter of a	*)
    (* menu item.  Returns when Space or Enter is typed, also returns	*)
    (* with M^.CurrentItem=0 if Esc is typed.  In Special Mode only,	*)
    (* also returns if a cursor movement key would run us off the edge	*)
    (* of the menu.  (In Normal Mode, any attempt to run off the edge	*)
    (* is ignored.)  In addition, in Special Mode the final key typed	*)
    (* remains available (e.g. by InKey()) to the caller.		*)

    TYPE CHARSET = SET OF CHAR;

    CONST CR = CHR(0DH);

    VAR option: CHAR;

    BEGIN
	WITH M^ DO
	    RanOffEdge := FALSE;
	    LOOP
		Highlight (M);
		option := GetKey (M^.win);
		Unhighlight (M);
		IF option = ClickIndicator THEN
		    InterpretMouseClick(M);
		ELSIF option = " " THEN EXIT(*LOOP*)
		ELSIF option = CR THEN EXIT (*LOOP*)
		ELSIF option = Esc THEN
		    CurrentItemNo := 0;  EXIT (*LOOP*)
		ELSIF option IN CHARSET{"0".."9", "A".."Z", "a".."z"} THEN
		    IF RepositionTo(M, CAP(option)) AND FastSelect THEN
			EXIT (*LOOP*);
		    END (*IF*);
		ELSIF option = CHR(0) THEN
		    HandleFunctionKey (M, option);
		    IF RanOffEdge THEN
			IF PutBackExitKey THEN
			    PutBack (option);  option := CHR(0);
			END (*IF*);
			EXIT (*LOOP*);
		    END (*IF*);
		END (*IF*);
	    END (*LOOP*);
	    IF PutBackExitKey THEN
		PutBack (option);
	    END (*IF*);
	    IF CurrentItemNo > NoOfItems THEN
		CurrentItemNo := 0;
	    END (*IF*);
	END (*WITH*);
    END MakeTheSelection;

(************************************************************************)

PROCEDURE SelectFromMenu (M: Menu): CARDINAL;

    (* Displays menu M on the screen, allows terminal user to use	*)
    (* cursor keys to move about the menu and the ENTER key to select	*)
    (* an item.  (The space bar is also accepted, as an alternative to	*)
    (* the ENTER key, to select an item).  An item may also be selected	*)
    (* by typing its initial letter, followed by space or ENTER.	*)
    (* Returns the number of the item which was selected.		*)
    (* (Item numbers start from 1).  An answer of 0 indicates that the	*)
    (* user typed the ESC key to return without selecting anything.	*)

    (* Remark: it is possible with the cursor keys to "select" a	*)
    (* nonexistent item at the bottom of the last column.  The result	*)
    (* will be 0 in this case.  Although this might appear to be a bug,	*)
    (* it is deliberate.  I found by experiment that the more "logical"	*)
    (* approach of stopping the user from moving the cursor into a	*)
    (* blank region was a nuisance for the user.			*)

    VAR UIW: UIWindow;  frame: FrameType;
	capabilities: CapabilitySet;  framesize: CARDINAL;

    BEGIN
	WITH M^ DO

	    (* Open the window, unless it's already open. *)

	    IF win = NilWindow THEN
		framesize := 0;
		IF MouseAvailable() THEN HideMouseCursor END(*IF*);
		WITH ScreenPosition DO
		    IF ShowBorder THEN
			frame := simpleframe;  INC(framesize);
		    ELSE frame := noframe
		    END (*IF*);
		    OpenWindow (win, foreground, background,
				firstrow, lastrow, firstcol, lastcol,
				frame, doubledivider);
		    IF ShowTitle THEN
			WriteString (win, heading);
			ChangeScrollingRegion (win, 2+framesize,
						lastrow-firstrow-framesize);
		    END (*IF*);
		END (*WITH*);
		IF MouseAvailable() THEN
		    IF ShowBorder AND MouseControl THEN
			capabilities := CapabilitySet {wshow, wmove, wescape};
		    ELSE
			capabilities := CapabilitySet {wshow};
		    END (*IF*);
		    UIW := AllowMouseControl (win, heading, capabilities);
		    WITH LocationInWindow DO
			AddActiveRegion (UIW, firstrow, firstrow+height-1,
				firstcol, firstcol+width-1,
				LeftOnly, RecordClick);
			IF ShowTitle OR (ShowBorder AND NOT MouseControl) THEN
			    AddActiveRegion (UIW, firstrow-1, firstrow-1,
				firstcol, firstcol+width-7,
				LeftOnly, ClickOnTop);
			    AddActiveRegion (UIW, firstrow-1, firstrow-1,
				firstcol+width-6, firstcol+width-1,
				LeftOnly, ClickOnTopMORE);
			END (*IF*);
			IF ShowBorder THEN
			    AddActiveRegion (UIW, firstrow+height, firstrow+height,
				firstcol, firstcol+width-7,
				LeftOnly, ClickOnBottom);
			    AddActiveRegion (UIW, firstrow+height, firstrow+height,
				firstcol+width-6, firstcol+width-1,
				LeftOnly, ClickOnBottomMORE);
			END (*IF*);
		    END (*WITH*);
		    ShowMouseCursor;
		END (*IF*);
		OpeningDisplay (M, 1);
	    END (*IF*);

	    WITH LocationInWindow DO
		NewScrollingRegion (win, firstrow, firstrow+height-1,
					firstcol, firstcol+width-1);
	    END (*WITH*);

	    (* Window is now open, let the user make a selection. *)

	    MakeTheSelection (M);
	    IF CurrentItemNo > 0 THEN
		Highlight (M);
	    END (*IF*);
	    ResetScrollingRegion (win);

	    (* Close the window, if appropriate. *)

	    IF CloseAfterSelection THEN
		IF MouseAvailable() THEN HideMouseCursor END(*IF*);
		CloseWindow (win);  win := NilWindow;
		IF MouseAvailable() THEN ShowMouseCursor END(*IF*);
	    END (*IF*);

	    RETURN CurrentItemNo;

	END (*WITH*);

    END SelectFromMenu;

(************************************************************************)

BEGIN
    WITH LastMouseClick DO
        CreateLock (access);
        valid := FALSE;
    END (*WITH*);
END Menus.
