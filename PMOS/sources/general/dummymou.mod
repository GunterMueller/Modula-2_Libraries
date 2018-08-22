IMPLEMENTATION MODULE DummyMouse;

	(********************************************************)
	(*							*)
	(*		  Dummy mouse driver			*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	17 August 1994			*)
	(*  Status:		OK				*)
	(*							*)
	(*	This module acts as the "mouse driver" for	*)
	(*	the case in which we don't want mouse support.	*)
	(*	It consists entirely of dummy procedures.	*)
	(*							*)
	(********************************************************)

FROM Mouse0 IMPORT
    (* type *)	ButtonSet, EventSet, EventHandler;

(************************************************************************)

PROCEDURE InitialiseMouseDriver(): BOOLEAN;

    (* Does all initialisation needed for this module.  We make this a	*)
    (* procedure rather than an initialisation section because module	*)
    (* Mouse has to decide which mouse driver to use.  The function	*)
    (* result indicates success; if it is FALSE, none of the following	*)
    (* procedures will work.  Note: this is not an end-user procedure,	*)
    (* it's intended to be called only by module Mouse.			*)

    BEGIN
	RETURN FALSE;
    END InitialiseMouseDriver;

(************************************************************************)

PROCEDURE Reset (VAR (*OUT*) MousePresent: BOOLEAN;
			VAR (*OUT*) NumberOfButtons: CARDINAL);

    (* Resets the mouse, returning MousePresent as FALSE if no mouse	*)
    (* available and as TRUE if it is, and NumberOfButtons as the	*)
    (* number of buttons for the mouse if installed.			*)

    BEGIN
	MousePresent := FALSE;  NumberOfButtons := 0;
    END Reset;

(************************************************************************)

PROCEDURE SetCursorPos (X, Y : CARDINAL);

    (* Sets the mouse cursor position. *)

    BEGIN
    END SetCursorPos;

(************************************************************************)

PROCEDURE GetPosBut (VAR (*OUT*) buttons: ButtonSet;
				VAR (*OUT*) Xposition, Yposition: CARDINAL);

    (* Returns the current mouse position and the state of the buttons.	*)
    (* Note: the units here are not the same as for procedure		*)
    (* GetTextMousePosition.  In both this procedure and in the event	*)
    (* handlers the position is presented in units of 1/8th of a	*)
    (* character width or height.					*)

    BEGIN
	buttons := ButtonSet{};
	Xposition := 0;  Yposition := 0;
    END GetPosBut;

(************************************************************************)

PROCEDURE SetPage (page: CARDINAL);

    (* Sets the hardware screen page where the mouse is visible. *)

    BEGIN
    END SetPage;

(************************************************************************)

PROCEDURE SetHorizontalLimits (MinX, MaxX : CARDINAL);

    BEGIN
    END SetHorizontalLimits;

(************************************************************************)

PROCEDURE SetVerticalLimits (MinY, MaxY : CARDINAL);

    BEGIN
    END SetVerticalLimits;

(************************************************************************)

PROCEDURE ShowCursor;

    (* Makes the mouse cursor visible on the screen.  Note: we allow	*)
    (* nesting in ShowCursor/HideCursor calls, so that for example if	*)
    (* you've called HideCursor twice then you need to call ShowCursor	*)
    (* twice to make the cursor reappear.				*)

    BEGIN
    END ShowCursor;

(************************************************************************)

PROCEDURE HideCursor;

    (* Makes the mouse cursor invisible. *)

    BEGIN
    END HideCursor;

(************************************************************************)

PROCEDURE SetEventHandler (DetectedEvents: EventSet;
					Handler: EventHandler);

    (* Nominates the procedure to be called whenever an event in the	*)
    (* set DetectedEvents occurs.  Note: the Handler is like an		*)
    (* interrupt procedure, in that it is executing in the context of	*)
    (* an unknown task; typically it should probably restrict its	*)
    (* actions to fairly elementary things, like a Signal to wake up	*)
    (* the task that really wants to know about the event.		*)

    BEGIN
    END SetEventHandler;

(************************************************************************)

END DummyMouse.
