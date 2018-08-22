IMPLEMENTATION MODULE UserInterface;

	(****************************************************************)
	(*								*)
	(*		Text User Interface for PMOS			*)
	(*								*)
	(*		Original version by M. Walsh			*)
	(*		 This version by P. Moylan			*)
	(*								*)
	(*	Last Edited:	24 February 1995			*)
	(*	Status:		OK					*)
	(*								*)
	(****************************************************************)

FROM TerminationControl IMPORT
    (* proc *)	SetTerminationProcedure;

FROM TaskControl IMPORT
    (* type *)	Lock,
    (* proc *)	CreateTask, CreateLock, DestroyLock, Obtain, Release;

FROM Semaphores IMPORT
    (* type *)	Semaphore,
    (* proc *)	Wait, Signal, CreateSemaphore;

FROM Storage1 IMPORT
    (* proc *)	ALLOCATE, DEALLOCATE;

FROM MiscPMOS IMPORT
    (* proc *)	CopyString;

FROM Keyboard IMPORT
    (* proc *)	StuffKeyboardBuffer;

FROM Windows IMPORT
    (* type *)	ColumnRange, RowRange, Rectangle, DisplayPage,
		Colour, Window, FrameType, DividerType, DisplayPage,
    (* proc *)	OpenWindow, SetCursor, WriteChar, WriteString, Hide,
		ShiftWindowAbs, CloseWindow, PutOnTop, IdentifyTopWindow,
		WindowLocation, PageOf, SaveCursor, InstallCloseHandler,
		RequestPageChangeNotification, PutOnPage;

FROM Mouse IMPORT
    (* type *)	Events, EventSet, Buttons,
    (* proc *)	MouseAvailable, InstallEventHandler, SetTextMousePage,
		ShowMouseCursor, HideMouseCursor, SetMouseCursorLimits,
		SetTextMousePosition;

(************************************************************************)

CONST
    LeftOnly = ButtonSet {LeftButton};
    RightOnly = ButtonSet {RightButton};
    LeftOrRight = ButtonSet {LeftButton, RightButton};

TYPE	
    UIWindow = POINTER TO WindowData;

    ActiveRegionPtr = POINTER TO ActiveRegion;

    (* An ActiveRegion is a rectangle within a window which has had a	*)
    (* "mouse click" action defined for it.				*)

    ActiveRegion = RECORD
			region: Rectangle;
			LiveButtons: ButtonSet;
			ActionToBeTaken: Action;
			NextActiveRegion: ActiveRegionPtr;
		   END (*RECORD*);

    (* A UIWindow is a pointer to a WindowData record.  This refers to	*)
    (* a Window (as defined in module Windows), together with some	*)
    (* extra information that this module keeps concerning the Window.	*)
    (* The fields are:							*)
    (*	win			the Window itself			*)
    (*	access			critical section protection Lock	*)
    (*	page			the display page on which the window	*)
    (*				resides					*)
    (*	ActiveRegionList	head of linked list of active regions	*)
    (*				associated with this window		*)
    (*	next, previous		used to keep the set of all UIWindows	*)
    (*				as a linked list			*)
    (*	title			title to use when the list of all	*)
    (*				windows is displayed.			*)

    WindowData =    RECORD
    			win: Window;
			access: Lock;
			page: DisplayPage;
			ActiveRegionList: ActiveRegionPtr;
			next, previous: UIWindow;
			title: ARRAY [0..19] OF CHAR;
		    END (*RECORD*);

(************************************************************************)

VAR
    (* For each display page we keep all UIWindows in a linked list,	*)
    (* and FirstUIW[page] is the head of this list.			*)

    FirstUIW: ARRAY DisplayPage OF UIWindow;

    (* Lock to control access to the lists of all windows.	*)

    ListAccess: Lock;

    (* DefaultUIW is a special UIWindow which does not go into the	*)
    (* master list, and which is associated with those areas of the	*)
    (* screen which are not otherwise covered by a UIWindow.		*)

    DefaultUIW: UIWindow;

    (* The hardware display page currently active. *)

    CurrentDisplayPage: DisplayPage;

    (* The number of UIWindows in each hardware page. *)

    WindowCount: ARRAY DisplayPage OF CARDINAL;

    (* WindowList[p] is a special Window, used to display the titles of	*)
    (* all the other windows on display page p.				*)

    WindowList: ARRAY DisplayPage OF Window;

    (* Flag to indicate that the list of all windows is on the screen.	*)

    WindowListOpen: ARRAY DisplayPage OF BOOLEAN;

    (* A semaphore to signal the event-handling task that a mouse	*)
    (* event has occurred.						*)

    MouseEvent: Semaphore;

    (* EventData records the triggering event, the button status, and	*)
    (* the mouse cursor position when a mouse event occurs.		*)

    EventData:	RECORD
		    access: Lock;
		    Trigger: EventSet;
		    ButtonStatus: ButtonSet;
		    Xpos: ColumnRange;
		    Ypos: RowRange;
		END (*RECORD*);

    (* MoveInProgress is set iff a "move window" operation is being	*)
    (* carried out.							*)

    MoveInProgress: BOOLEAN;

    (* While a move is in progress, we need to carry some information	*)
    (* forward from one event to the next.  These data don't need	*)
    (* critical section protection, since it's impossible for more than	*)
    (* one move to be in progress at a time.				*)

    MoveData:	RECORD
		    window: Window;
		    rowoffset: RowRange;
		    coloffset: ColumnRange;
		END (*RECORD*);

(************************************************************************)
(*			THE MOUSE CLICK HANDLER				*)
(************************************************************************)


(*<TopSpeed3*)
(*# save, call(c_conv => off, same_ds => off, near_call => off) *)
(*# call(reg_param => (ax,bx,cx,dx,st0,st6,st5,st4,st3)) *)
(*>*)

PROCEDURE MouseClickHandler (A: EventSet;  B: ButtonSet;  X, Y: CARDINAL);

    (* This procedure is called by the mouse driver whenever there is	*)
    (* a mouse event, and that event is in the event set specified at	*)
    (* the time this handler was activated.  Parameter A shows the	*)
    (* event(s) that triggered the call; B shows the current up/down	*)
    (* state of the mouse buttons; and (X,Y) is the mouse cursor	*)
    (* position.							*)

    BEGIN
	WITH EventData DO
	    Obtain (access);
	    Trigger := A;
	    ButtonStatus := B;
	    Xpos := X DIV 8;  Ypos := Y DIV 8;
	    Release (access);
	END (*WITH*);
	Signal (MouseEvent);
    END MouseClickHandler;

(*<TopSpeed3*) (*# restore *) (*>*)

(************************************************************************)
(*			MISCELLANEOUS PROCEDURES			*)
(************************************************************************)

PROCEDURE Inside (row: RowRange;  col: ColumnRange;  R: Rectangle): BOOLEAN;

    (* Returns TRUE iff point (row,col) is in (or on the border of) R.	*)

    BEGIN
	WITH R DO
	    RETURN (col >= left) AND (col <= right)
				AND (row >= top) AND (row <= bottom);
	END (*WITH*);
    END Inside;

(************************************************************************)

PROCEDURE FindOwner (w: Window): UIWindow;

    (* Returns the UIWindow with which w is associated, or returns NIL	*)
    (* if there is no such UIWindow.					*)

    VAR result: UIWindow;  page: DisplayPage;

    BEGIN
	Obtain (ListAccess);
	page := 0;
	LOOP
	    result := FirstUIW[page];
	    LOOP
		IF result = NIL THEN EXIT (*LOOP*)
		ELSIF result^.win = w THEN EXIT (*LOOP*)
		ELSE result := result^.next;
		END (*IF*);
	    END (*LOOP*);
	    IF (result <> NIL) OR (page = MAX(DisplayPage)) THEN
		EXIT (*LOOP*);
	    END (*IF*);
	    INC (page);
	END (*LOOP*);
	Release (ListAccess);
	RETURN result;
    END FindOwner;

(************************************************************************)
(*			SHUTTING DOWN A UIWINDOW			*)
(************************************************************************)

PROCEDURE CloseWindowList (w: Window;  r: RowRange;  c: ColumnRange);

    (* Closes w - which happens to be the window list for some display	*)
    (* page, because of the way this procedure is called.		*)

    VAR page: DisplayPage;

    BEGIN
	page := PageOf(w);
	HideMouseCursor;
	WindowListOpen[page] := FALSE;

	(* Subtle point: we don't explicitly destroy the UIWindow to	*)
	(* which w belongs, since it is automatically destroyed (by	*)
	(* procedure UnregisterWindow) as a side-effect of CloseWindow.	*)

	(* The complications caused by side-effects disturb me a little	*)
	(* on philosophical grounds.  One day I should try to invent	*)
	(* a better substitute for call-back procedures.  They're	*)
	(* really designed for a language like C or C++ whose		*)
	(* programmers are trained to expect hidden gotchas.		*)

	CloseWindow (w);
	ShowMouseCursor;

    END CloseWindowList;

(************************************************************************)

PROCEDURE DestroyUIWindow (VAR (*INOUT*) UIW: UIWindow);

    (* Destroys UIW, without closing its window. *)

    VAR ActiveRegionNext, ActiveRegionToDispose: ActiveRegionPtr;
	page: DisplayPage;

    BEGIN
	page := UIW^.page;

	(* If the window list is currently open, close it, since its	*)
	(* entries are about to become invalid.  Hidden trap: watch out	*)
	(* for a recursive close of the window list.			*)

	IF WindowListOpen[page] AND (UIW^.win <> WindowList[page]) THEN
	    CloseWindowList (WindowList[page], 0, 0);
	END (*IF*);

	Obtain (UIW^.access);

	(* Reclaim the space used in the ActiveRegionList linked list. *)

	ActiveRegionNext := UIW^.ActiveRegionList;
	WHILE ActiveRegionNext <> NIL DO
	    ActiveRegionToDispose := ActiveRegionNext;
	    ActiveRegionNext := ActiveRegionNext^.NextActiveRegion;
	    DISPOSE (ActiveRegionToDispose);
	END (*WHILE *);
	Release (UIW^.access);
	DestroyLock (UIW^.access);

	(* Remove this UIWindow from the master list. *)

	Obtain (ListAccess);
	DEC (WindowCount[page]);
	IF WindowCount[CurrentDisplayPage] = 0 THEN
	    HideMouseCursor;
	END(*IF*);
	IF UIW^.previous = NIL THEN
	    FirstUIW[page] := UIW^.next;
	ELSE
	    UIW^.previous^.next := UIW^.next;
	END (*IF*);
	IF UIW^.next <> NIL THEN
	    UIW^.next^.previous := UIW^.previous;
	END (*IF*);
	Release (ListAccess);
	DISPOSE (UIW);

    END DestroyUIWindow;

(************************************************************************)

PROCEDURE UnregisterWindow (w: Window;  dummy: DisplayPage);

    (* Removes w from the set of Windows controlled by this module.	*)
    (* The second parameter is a dummy for compatibility with		*)
    (* procedure InstallCloseHandler.					*)

    VAR UIW: UIWindow;

    BEGIN
	UIW := FindOwner (w);
	IF UIW <> NIL THEN
	    DestroyUIWindow (UIW);
	END (*IF*);
    END UnregisterWindow;

(************************************************************************)
(*			MANIPULATING ACTIVE REGIONS			*)
(************************************************************************)

PROCEDURE AddActiveRegion (UIW: UIWindow; Top, Bottom: RowRange;
			Left, Right: ColumnRange;  ButtonsEnabled: ButtonSet;
			ActionProc: Action);

    (* After a call to this procedure, any mouse click on a button in	*)
    (* ButtonsEnabled, and in the rectangle defined by the other	*)
    (* parameters, will cause ActionProc to be called.			*)

    VAR NewActiveRegion: ActiveRegionPtr;

    BEGIN
	IF UIW = NIL THEN RETURN END(*IF*);

	NEW (NewActiveRegion);
	WITH NewActiveRegion^ DO
	    WITH region DO
		top := Top;  bottom := Bottom;
		left := Left;  right := Right;
	    END (*WITH*);
	    LiveButtons := ButtonsEnabled;
	    ActionToBeTaken := ActionProc;
	END (*WITH*);

	(* Insert the new active region at the head of the UIWindow's	*)
	(* active region list.  Putting it at the head gives it		*)
	(* priority over other (possibly overlapping) regions which	*)
	(* were earlier added, and this is exactly the effect we want.	*)

	WITH UIW^ DO
	    Obtain (access);
	    NewActiveRegion^.NextActiveRegion := ActiveRegionList;
	    ActiveRegionList := NewActiveRegion;
	    Release (access);
	END (*WITH*);

    END AddActiveRegion;

(************************************************************************)
(*		THE BUILT-IN ACTIVE REGION HANDLERS			*)
(************************************************************************)

PROCEDURE MoveWindow (w: Window;  yrel: RowRange;  xrel: ColumnRange);

    (* An ActiveRegion procedure to move a window on the screen.  Most	*)
    (* of the work is actually done by procedure ContinueMove, given	*)
    (* later.  This procedure just sets up the right conditions for	*)
    (* ContinueMove to be called by the event dispatcher.		*)

    VAR R: Rectangle;

    BEGIN
	R := WindowLocation(w);
	WITH R DO
	    SetMouseCursorLimits (yrel, MAX(RowRange) - bottom + yrel + top,
				xrel, MAX(ColumnRange) - right + xrel + left);
	END (*WITH*);

	WITH MoveData DO
	    window := w;
	    rowoffset := yrel;  coloffset := xrel;
	END (*WITH*);

	InstallEventHandler (EventSet {LeftUp, RightUp, Motion},
						MouseClickHandler);
	MoveInProgress := TRUE;

    END MoveWindow;

(***********************************************************************)

PROCEDURE HideWindow (w: Window;  r: RowRange;  c: ColumnRange);

    (* An ActiveRegion procedure to hide a window. *)

    BEGIN
	HideMouseCursor;
	Hide (w);
	ShowMouseCursor;
    END HideWindow;

(***********************************************************************)

PROCEDURE ShowWindow (w: Window;  r: RowRange;  c: ColumnRange);

    (* An ActiveRegion procedure to put a window on top. *)

    BEGIN
	HideMouseCursor;
	PutOnTop (w);
	ShowMouseCursor;
    END ShowWindow;

(************************************************************************)

PROCEDURE ContinueMove (x: ColumnRange;  y: RowRange);

    (* Called by the mouse event handler when a "move window" operation	*)
    (* is in progress.  If the global variable MoveInProgress is FALSE,	*)
    (* we've just reached the end of the move.				*)

    BEGIN
	IF MoveInProgress THEN
	    WITH MoveData DO
		HideMouseCursor;
		ShiftWindowAbs (window, y-rowoffset, x-coloffset);
		ShowMouseCursor;
	    END (*WITH*);
	ELSE
	    (* End of move.  Restore the normal event handler. *)
	    SetMouseCursorLimits (0, MAX(RowRange), 0, MAX(ColumnRange));
	    InstallEventHandler (EventSet {LeftDown, RightDown},
						MouseClickHandler);
	END (*IF*);
    END ContinueMove;

(************************************************************************)

PROCEDURE StuffEscape (w: Window;  r: RowRange;  c: ColumnRange);

    (* An ActiveRegion procedure to stuff an Esc character into the	*)
    (* keyboard buffer.							*)

    CONST Esc = CHR(27);

    BEGIN
	StuffKeyboardBuffer (Esc);
    END StuffEscape;

(************************************************************************)
(*			LOOKING AFTER THE WINDOW LIST			*)
(************************************************************************)

PROCEDURE SelectFromWindowList (dummy: Window;
					row: RowRange;  col: ColumnRange);

    (* Called when the user clicks on a window name in the window list.	*)
    (* Brings that window to the top, and closes the window list.	*)

    VAR p: UIWindow;  j: RowRange;

    BEGIN
	HideMouseCursor;
	Obtain (ListAccess);
	p := FirstUIW[CurrentDisplayPage];
	FOR j := 1 TO WindowCount[CurrentDisplayPage]-row DO
	    p := p^.next;
	END (*FOR*);
	Release (ListAccess);
	PutOnTop (p^.win);
	CloseWindowList (WindowList[CurrentDisplayPage], 0, 0);
	ShowMouseCursor;
    END SelectFromWindowList;

(************************************************************************)

PROCEDURE DisplayWindowList (dummy: Window;  Y: RowRange;  X: ColumnRange);

    (* Displays the list of all windows which this module knows about,	*)
    (* and sets up active regions which will allow the user to select	*)
    (* a window (possibly hidden) with a mouse click.			*)

    VAR ListUIW, UIW: UIWindow;  j, count: CARDINAL;  p: DisplayPage;

    BEGIN
	p := CurrentDisplayPage;
	count := WindowCount[p];
	IF WindowListOpen[p] THEN DEC(count) END(*IF*);

	(* Adjust the cursor position, if necessary, to ensure that the	*)
	(* window list won't fall outside the screen boundaries.	*)

	IF Y > MAX(RowRange) - count - 1 THEN
	    Y := MAX(RowRange) - count - 1;
	END (*IF*);
	IF X < 2 THEN
	    X := 2;
	ELSIF X > MAX(ColumnRange) - 19 THEN
	    X := MAX(ColumnRange) - 19;
	END (*IF*);
	HideMouseCursor;
	SetTextMousePosition (X, Y);

	IF WindowListOpen[p] THEN
	    ShiftWindowAbs (WindowList[p], Y, X-2);
	ELSE
	    OpenWindow (WindowList[p], black, white, Y, Y+count+1,
				X-2, X+19, doubleframe, nodivider);
	    PutOnPage (WindowList[p], p);
	    ListUIW := AllowMouseControl (WindowList[p], "",
				CapabilitySet {wmove, whide, wshow});
	
	    (* Overlay the HideWindow ActiveRegion - which we specified	*)
	    (* above only in order to get its button displayed - with a	*)
	    (* CloseWindowList region.					*)

	    AddActiveRegion (ListUIW,0,0,19,19,LeftOnly,CloseWindowList);

	    (* Display the names of all the windows we control		*)
	    (* (ignoring the first, which is the WindowList itself).	*)

	    SetCursor (WindowList[p], 0, 5);
	    WriteString (WindowList[p], "Window List");
	    Obtain (ListAccess);
	    UIW := FirstUIW[p]^.next;
	    FOR j := count TO 1 BY -1 DO
		SetCursor (WindowList[p], j, 1);
		WriteString (WindowList[p], UIW^.title);
		UIW := UIW^.next;
	    END (*FOR*);
	    Release (ListAccess);

	    (* Create an ActiveRegion which will allow the user to	*)
	    (* click on the list of names.				*)

	    AddActiveRegion (ListUIW,1,count,1,20,LeftOnly,
						SelectFromWindowList);
	    WindowListOpen[p] := TRUE;
	END (*IF*);
	ShowMouseCursor;
	MoveWindow (WindowList[p], 0, 2);

    END DisplayWindowList;

(************************************************************************)
(*			CREATING A UI WINDOW				*)
(************************************************************************)

PROCEDURE MakeDefaultUIWindow;

    (* Creates a special UIWindow which does not live on any page, and	*)
    (* which does not have any Window associated with it.  The sole	*)
    (* purpose of this special UIWindow is to allow us to create	*)
    (* active regions on parts of the screen not occupied by any	*)
    (* Window known to this module.					*)

    BEGIN
	NEW (DefaultUIW);
	WITH DefaultUIW^ DO
	    win := Window(NIL);
	    CreateLock (access);
	    page := 0;
	    ActiveRegionList := NIL;
	    previous := NIL;  next := NIL;
	    title := "Default UIWindow";
	END (*WITH*);
    END MakeDefaultUIWindow;

(************************************************************************)

PROCEDURE MakeUIWindow (p: DisplayPage;  Label: ARRAY OF CHAR): UIWindow;

    (* Creates a new UIWindow on page p, puts it into the list of all	*)
    (* UIWindows.							*)

    VAR result: UIWindow;

    BEGIN
	(* If the window list is currently open, close it, since its	*)
	(* entries are about to become invalid.				*)

	IF WindowListOpen[p] THEN
	    CloseWindowList (WindowList[p], 0, 0);
	END (*IF*);

	(* Create a new UIWindow record, and fill in the details.	*)

	NEW (result);
	WITH result^ DO
	    CreateLock (access);
	    page := p;
	    ActiveRegionList := NIL;
	    CopyString (Label, title);
	END (*WITH*);

	(* Link the new record into the master list. *)

	Obtain (ListAccess);
	INC (WindowCount[p]);
	result^.next := FirstUIW[p];
	result^.previous := NIL;
	IF FirstUIW[p] <> NIL THEN
	    FirstUIW[p]^.previous := result;
	END (*IF*);
	FirstUIW[p] := result;
	Release (ListAccess);

	IF (p = CurrentDisplayPage) AND (WindowCount[p] = 1) THEN
	    ShowMouseCursor;
	END (*IF*);

	RETURN result;

    END MakeUIWindow;

(************************************************************************)

PROCEDURE AddCapabilities (UIW: UIWindow;  CS: CapabilitySet);

    (* Activates the specified set of user-controlled capabilities.	*)
    (* This includes drawing the "buttons" on the border, therefore	*)
    (* UIW^.win must already be open.					*)

    VAR w: Window;  R: Rectangle;  maxrow, maxcol: CARDINAL;
	oldrow, oldcol: CARDINAL;

    BEGIN
	w := UIW^.win;
	SaveCursor (w, oldrow, oldcol);
	R := WindowLocation (w);
	WITH R DO
	    maxrow := bottom - top;
	    maxcol := right - left;
	END (*WITH*);

	IF wshow IN CS THEN
	    AddActiveRegion (UIW,0,maxrow,0,maxcol,LeftOnly,ShowWindow);
	END (*IF*);

	IF wmove IN CS THEN
	    SetCursor (w, 0, 1);
	    WriteChar (w, '[');
	    WriteChar (w, CHAR(29));
	    WriteChar (w, ']');
	    AddActiveRegion (UIW, 0, 0, 2, 2, LeftOrRight, MoveWindow);
	END (*IF*);

	IF whide IN CS THEN
	    SetCursor(w, 0, maxcol-3);
	    WriteChar(w, '[');
	    WriteChar(w, CHAR(15));
	    WriteChar(w, ']');
	    AddActiveRegion (UIW,0,0,maxcol-2,maxcol-2,LeftOnly, HideWindow);
	END (*IF*);

	IF wescape IN CS THEN
	    SetCursor(w, 0, maxcol-3);
	    WriteChar(w, '[');
	    WriteChar(w, CHAR(15));
	    WriteChar(w, ']');
	    AddActiveRegion (UIW,0,0,maxcol-2,maxcol-2,LeftOnly, StuffEscape);
	END (*IF*);

	SetCursor (w, oldrow, oldcol);

    END AddCapabilities;

(************************************************************************)
(*		ACTIVATING MOUSE CONTROL FOR A WINDOW			*)
(************************************************************************)

PROCEDURE AllowMouseControl (w: Window;  Title: ARRAY OF CHAR;
				OptionsEnabled: CapabilitySet): UIWindow;

    (* Adds w to the set of windows which this module is allowed to	*)
    (* manipulate.							*)

    VAR result: UIWindow;

    BEGIN
	IF MouseAvailable() THEN
	    result := MakeUIWindow (PageOf(w), Title);
	    result^.win := w;
	    InstallCloseHandler (w, UnregisterWindow);
	    AddCapabilities (result, OptionsEnabled);
	ELSE
	    result := NIL;
	END (*IF*);
	RETURN result;
    END AllowMouseControl;

(************************************************************************)
(*			SETTING THE DISPLAY PAGE			*)
(************************************************************************)

PROCEDURE SelectPage (page: DisplayPage);

    (* Switches the mouse focus to the given display page (but we don't	*)
    (* update the display, it's assumed that the caller is taking care	*)
    (* of that).  Also aborts any move in progress.			*)

    BEGIN
	IF WindowCount[CurrentDisplayPage] > 0 THEN
	    HideMouseCursor;
	END(*IF*);
	SetTextMousePage (page);
	MoveInProgress := FALSE;
	CurrentDisplayPage := page;
	IF WindowCount[page] > 0 THEN
	    ShowMouseCursor;
	END(*IF*);
    END SelectPage;

(************************************************************************)
(*			THE EVENT DISPATCHER				*)
(************************************************************************)

PROCEDURE DoWindowAction (col: ColumnRange;  row: RowRange;  BS: ButtonSet);

    (* This procedure is called when a mouse button is pressed, with	*)
    (* (col,row) being the current mouse cursor position.  We check	*)
    (* whether any action has been defined for this position, and if so	*)
    (* we call the appropriate action handler.				*)

    CONST EmptySet = ButtonSet {};

    VAR w: Window;
	UIW: UIWindow;
	CurrentRegion: ActiveRegionPtr;
	RegionFound: BOOLEAN;

    BEGIN
	RegionFound := FALSE;  UIW := NIL;

	IF IdentifyTopWindow (w, row, col) THEN
	    UIW := FindOwner (w);
	ELSE
	    UIW := DefaultUIW;
	END (*IF*);

	(* If a window has been found at this screen position, check	*)
	(* whether an ActiveRegion for that window includes the cursor	*)
	(* position.  Note: a side-effect of the call to procedure	*)
	(* IdentifyTopWindow is that (col,row) are now window-relative.	*)

	IF UIW <> NIL THEN
	    CurrentRegion := UIW^.ActiveRegionList;
	    WHILE  (CurrentRegion <> NIL) AND (NOT RegionFound) DO
		IF Inside (row, col, CurrentRegion^.region)
			AND (BS*CurrentRegion^.LiveButtons <> EmptySet) THEN
		    RegionFound := TRUE;
		ELSE
		    CurrentRegion := CurrentRegion^.NextActiveRegion;
		END (*IF*);
	    END (*WHILE*);
	END (*IF *);

	IF RegionFound THEN
	    CurrentRegion^.ActionToBeTaken (w, row, col);
	END (*IF*);

    END DoWindowAction;

(************************************************************************)

PROCEDURE EventDispatcher;

    (* Runs as a separate task, whose function is to call the		*)
    (* appropriate event handler whenever a mouse event occurs.		*)

    VAR X: ColumnRange;  Y: RowRange;  A: EventSet;  B: ButtonSet;

    BEGIN
	LOOP
	    Wait (MouseEvent);
	    WITH EventData DO
		Obtain (access);
		A := Trigger;
		B := ButtonStatus;
		X := Xpos;  Y := Ypos;
		Release (access);
	    END (*WITH*);
	    IF MoveInProgress THEN
		IF (LeftUp IN A) OR (RightUp IN A) THEN
		    MoveInProgress := FALSE;
		END (*IF*);
		ContinueMove (X, Y);
	    ELSE
		DoWindowAction (X, Y, B);
	    END (*IF*);
	END (*LOOP*);
    END EventDispatcher;

(************************************************************************)
(*				TERMINATION				*)
(************************************************************************)

PROCEDURE Shutdown;

    (* Creates a new UIWindow, puts it into the list of all UIWindows. *)

    VAR p: DisplayPage;

    BEGIN
	FOR p := 0 TO MAX(DisplayPage) DO
	    IF WindowListOpen[p] THEN
		CloseWindowList (WindowList[p], 0, 0);
	    END (*IF*);
	END (*FOR*);
    END Shutdown;

(************************************************************************)
(*			  MODULE INITIALISATION				*)
(************************************************************************)			

VAR p: DisplayPage;

BEGIN
    SetTerminationProcedure (Shutdown);
    FOR p := 0 TO MAX(DisplayPage) DO
	FirstUIW[p] := NIL;  WindowCount[p] := 0;
	WindowListOpen[p] := FALSE;
    END (*FOR*);
    MakeDefaultUIWindow;
    MoveInProgress := FALSE;
    CurrentDisplayPage := 0;
    CreateLock (ListAccess);
    IF MouseAvailable() THEN
	RequestPageChangeNotification (SelectPage);
	CreateSemaphore (MouseEvent, 0);
	CreateLock (EventData.access);
	CreateTask (EventDispatcher, 8, "Mouse Events");
	InstallEventHandler (EventSet {LeftDown, RightDown}, MouseClickHandler);
	SelectPage (0);
    END (*IF*);
    AddActiveRegion (DefaultUIW, 0, MAX(RowRange), 0, MAX(ColumnRange),
					RightOnly, DisplayWindowList);

END UserInterface.
