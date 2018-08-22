IMPLEMENTATION MODULE Mouse;

	(********************************************************)
	(*							*)
	(*		    Mouse driver			*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	17 August 1994			*)
	(*  Status:		OK				*)
	(*							*)
	(*	This module is actually an intermediary		*)
	(*	between the user and the true mouse driver;	*)
	(*	this is so that I can easily experiment		*)
	(*	with different mouse drivers.			*)
	(*							*)
	(*	Note: this module contains preprocessor		*)
	(*	directives and should be processed by PP.	*)
	(*							*)
	(********************************************************)

FROM
(*<~UseMouse DummyMouse >*)
(*<MouseKind=INT33*) Mouse33 (*>*)
(*<UseMouse&(MouseKind<>INT33) SerialMouse >*)
  IMPORT
    (* proc *)	InitialiseMouseDriver, Reset, SetCursorPos, SetPage,
		GetPosBut, ShowCursor, HideCursor, SetEventHandler,
		SetHorizontalLimits, SetVerticalLimits;

(************************************************************************)

VAR
    HaveMouse: BOOLEAN;
    NumberOfButtons: CARDINAL;

(************************************************************************)

PROCEDURE MouseAvailable (): BOOLEAN;

    (* Returns TRUE iff a mouse driver is loaded, a mouse exists, and	*)
    (* mouse operation is permitted in module ConfigurationOptions.	*)

    BEGIN
	RETURN HaveMouse AND (NumberOfButtons > 0);
    END MouseAvailable;

(************************************************************************)

PROCEDURE ResetMouse (VAR (*OUT*) MousePresent: BOOLEAN;
			VAR (*OUT*) NumberOfButtons: CARDINAL);

    (* Initializes mouse, returning MousePresent as FALSE if no mouse	*)
    (* available and as TRUE if it is, and NumberOfButtons as the	*)
    (* number of buttons for the mouse if installed.			*)

    BEGIN
	Reset (MousePresent, NumberOfButtons);
    END ResetMouse;

(************************************************************************)

PROCEDURE GetTextMousePosition (VAR (*OUT*) Xposition: ColumnRange;
				VAR (*OUT*) Yposition: RowRange);

    (* Returns the current position of the mouse cursor. *)

    VAR Buttons: ButtonSet;  X, Y: CARDINAL;

    BEGIN
	GetPosBut (Buttons, X, Y);
	Xposition := X DIV 8;
	Yposition := Y DIV 8;
    END GetTextMousePosition;

(************************************************************************)

PROCEDURE GetTextMouseStatus (VAR (*OUT*) buttons: ButtonSet;
				VAR (*OUT*) Xposition: ColumnRange;
				VAR (*OUT*) Yposition: RowRange);

    (* Returns the current mouse position and state of the buttons.	*)

    VAR X, Y: CARDINAL;

    BEGIN
	GetPosBut (buttons, X, Y);
	Xposition := X DIV 8;
	Yposition := Y DIV 8;
    END GetTextMouseStatus;

(************************************************************************)

PROCEDURE SetTextMousePosition (Xposition: ColumnRange; Yposition: RowRange);

    (* Initialises the mouse position. *)

    BEGIN
	SetCursorPos (8*Xposition, 8*Yposition);
    END SetTextMousePosition;

(************************************************************************)

PROCEDURE SetTextMousePage (page: CARDINAL);

    (* Sets the hardware screen page where the mouse is visible. *)

    BEGIN
	SetPage (page);
    END SetTextMousePage;

(************************************************************************)

PROCEDURE SetMouseCursorLimits (top, bottom: RowRange;
					left, right: ColumnRange);

    (* Specifies a rectangular region outside which the mouse cursor	*)
    (* may not go.							*)

    VAR X: ColumnRange;  Y: RowRange;

    BEGIN
	SetHorizontalLimits (8*left, 8*right);
	SetVerticalLimits (8*top, 8*bottom);
    END SetMouseCursorLimits;

(************************************************************************)

PROCEDURE ShowMouseCursor;

    (* Makes the mouse cursor visible on the screen. *)

    BEGIN
	ShowCursor;
    END ShowMouseCursor;

(************************************************************************)

PROCEDURE HideMouseCursor;

    (* Makes the mouse cursor invisible. *)

    BEGIN
	HideCursor;
    END HideMouseCursor;

(************************************************************************)

PROCEDURE InstallEventHandler (DetectedEvents: EventSet;
					Handler: EventHandler);

    (* Nominates the procedure to be called whenever an event in the	*)
    (* set DetectedEvents occurs.					*)

    BEGIN
	SetEventHandler (DetectedEvents, Handler);
    END InstallEventHandler;

(************************************************************************)

BEGIN
    HaveMouse := InitialiseMouseDriver();
    IF HaveMouse THEN
	ResetMouse (HaveMouse, NumberOfButtons);
    ELSE
	NumberOfButtons := 0;
    END (*IF*);
END Mouse.
