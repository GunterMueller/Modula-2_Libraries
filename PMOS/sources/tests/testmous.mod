MODULE TestMouse;

	(****************************************************************)
	(*								*)
	(*		    Test of the Mouse module			*)
	(*	Demonstrates simple use of the mouse and event handler	*)
	(*								*)
	(*	Author:		P.D. Terry, Rhodes University,		*)
	(*			 Sun  01-10-1993			*)
	(*	Modified by:	Peter Moylan				*)
	(*			(to run with PMOS and TopSpeed v3)	*)
	(*	Last edited:	16 March 1995				*)
	(*	Status:		Working					*)
	(*								*)
	(*	    Two known but minor bugs:				*)
	(*	     1.	(Fixed)						*)
	(*	     2.	Mouse cursor disappears if you leave it sitting	*)
	(*		at a screen location which is being rewritten.	*)
	(*	    Problem #2 is fixed in this program by surrounding	*)
	(*	    every screen write by "HideMouseCursor" and		*)
	(*	    "ShowMouseCursor" operations; but a general		*)
	(*	    solution would probably require modifications to my	*)
	(*	    Windows module, and I'm reluctant to take such a	*)
	(*	    step because in principle module Windows should not	*)
	(*	    need to know anything about mice.			*)
	(*								*)
	(****************************************************************)

FROM Mouse IMPORT
    (* type *)	Buttons, ButtonSet, Events, EventSet,
    (* proc *)	ResetMouse, InstallEventHandler, GetTextMouseStatus,
		HideMouseCursor, ShowMouseCursor;

	(* Note to users of PMOS version 2.0: in version 2.0 module	*)
	(* TaskControl does not support the Lock datatype.  To resolve	*)
	(* this, either upgrade to version 2.1 or replace every Lock	*)
	(* by a Semaphore.						*)

FROM TaskControl IMPORT
    (* type *)	Lock,
    (* proc *)	CreateTask, CreateLock, Obtain, Release;

FROM Semaphores IMPORT
    (* type *)	Semaphore,
    (* proc *)	CreateSemaphore, Wait, Signal;

FROM SoundEffects IMPORT
    (* proc *)	Beep;

FROM Keyboard IMPORT
    (* proc *)	InKey;

FROM Windows IMPORT
    (* type *)	Window, Colour, FrameType, DividerType,
		RowRange, ColumnRange,
    (* proc *)	OpenWindow, CloseWindow, WriteString, WriteLn,
		ChangeScrollingRegion, SetCursor;

FROM NumericIO IMPORT
    (* proc *)	WriteCard, WriteRJCard;

(************************************************************************)

VAR
    (* HaveMouse is TRUE iff a mouse and mouse driver are present.	  *)
    (* NumberOfButtons is the number of mouse buttons known to the driver.*)

    HaveMouse : BOOLEAN;
    NumberOfButtons: CARDINAL;

    (* The following counters count the events picked up by our handler.*)

    EventCount: ARRAY [LeftDown..MiddleUp] OF CARDINAL;

    (* Our record of the current mouse position and button status. *)

    MouseState:	RECORD
		    access: Lock;
		    Xpos: ColumnRange;
		    Ypos: RowRange;
		    LeftPressed, MiddlePressed, RightPressed: BOOLEAN;
		END (*RECORD*);

    (* Semaphore to signal the fact that a mouse event has happened.	*)

    MouseEvent: Semaphore;

(************************************************************************)

PROCEDURE DisplayPosition;

    (* Runs as a separate task. *)

    VAR X, Y: CARDINAL;  Left, Middle, Right: BOOLEAN;
	w1, w2: Window;
	DownUp: ARRAY BOOLEAN OF ARRAY [0..3] OF CHAR;

    BEGIN
	DownUp[FALSE] := "UP  ";
	DownUp[TRUE] := "DOWN";

	(* Create a window to display the mouse position. *)

	OpenWindow (w1,black,green,2,6,60,79,simpleframe,doubledivider);
	WriteString (w1, "  Mouse position");
	ChangeScrollingRegion (w1, 3, 3);
	SetCursor (w1, 3, 7);  WriteString (w1, ",");

	(* Create a window to display the button state. *)

	OpenWindow (w2,black,green,8,14,60,79,simpleframe,doubledivider);
	WriteString (w2, "     Buttons");
	ChangeScrollingRegion (w2, 3, 7);
	WriteString (w2, "Left");  WriteLn (w2);
	WriteString (w2, "Middle");  WriteLn (w2);
	WriteString (w2, "Right");  WriteLn (w2);

	LOOP	(* forever *)
	    ShowMouseCursor;
	    Wait (MouseEvent);

	    (* Pick up the mouse state. *)

	    WITH MouseState DO
		Obtain (access);
		X := Xpos;  Y := Ypos;
		Left := LeftPressed;  Middle := MiddlePressed;
		Right := RightPressed;
		Release (access);
	    END (*WITH*);

	    (* Turn off mouse cursor while writing to the screen. *)

	    HideMouseCursor;

	    (* Display mouse position. *)

	    SetCursor (w1, 3, 3);  WriteRJCard (w1, X, 4);
	    SetCursor (w1, 3, 9);  WriteRJCard (w1, Y, 4);

	    (* Display button status. *)

	    SetCursor (w2, 3, 9);  WriteString (w2, DownUp[Left]);
	    IF NumberOfButtons > 1 THEN
		SetCursor (w2, 5, 9);  WriteString (w2, DownUp[Right]);
	    END (*IF*);
	    IF NumberOfButtons > 2 THEN
		SetCursor (w2, 4, 9);  WriteString (w2, DownUp[Middle]);
	    END (*IF*);
	END (*LOOP*);

    END DisplayPosition;

(************************************************************************)

(*# save, call(c_conv => off, same_ds => off, near_call => off) *)
(*# call(reg_param => (ax,bx,cx,dx,st0,st6,st5,st4,st3)) *)

PROCEDURE Handler (A: EventSet;  B: ButtonSet;  X, Y : CARDINAL);

    (*   +++++ Don't attempt any DOS calls inside one of these ++++++	*)
    (* This procedure is called by the mouse driver whenever there is	*)
    (* a mouse event, and that event is in the event set specified at	*)
    (* the time this handler was activated.				*)

    VAR j: Events;

    BEGIN
	FOR j := LeftDown TO MiddleUp DO
	    IF j IN A THEN
		INC (EventCount[j]);
	    END (*IF*);
	END (*FOR*);

	WITH MouseState DO
	    Obtain (access);
	    Xpos := X DIV 8;  Ypos := Y DIV 8;
	    LeftPressed := LeftButton IN B;
	    MiddlePressed := MiddleButton IN B;
	    RightPressed := RightButton IN B;
	    Release (access);
	END (*WITH*);
	Signal (MouseEvent);

    END Handler;

(*# restore *)

(************************************************************************)

PROCEDURE RunTheTest;

    (* Starts event handler, starts DisplayPosition task, and lets them	*)
    (* run until the user hits a keyboard key.  Before exiting, writes	*)
    (* a summary of the mouse events.					*)

    CONST DetectedEvents = EventSet {LeftDown, LeftUp, MiddleDown, MiddleUp,
						RightDown, RightUp, Motion};

    VAR w: Window;  ch: CHAR;  B: ButtonSet;

    BEGIN
	WITH MouseState DO
	    CreateLock (access);
	    GetTextMouseStatus (B, Xpos, Ypos);
	    LeftPressed := LeftButton IN B;
	    MiddlePressed := MiddleButton IN B;
	    RightPressed := RightButton IN B;
	END (*WITH*);
	Signal (MouseEvent);
	HideMouseCursor;
	OpenWindow (w, blue, cyan, 2, 14, 0, 55, simpleframe, nodivider);
	WriteString(w, 'Mouse detected with ');
	WriteCard(w, NumberOfButtons); WriteString(w, ' buttons.');
	WriteLn (w);
	WriteString(w, 'Move mouse, press buttons, hit any key to finish.');
	WriteLn (w);
	ShowMouseCursor;

	InstallEventHandler (DetectedEvents, Handler);

	CreateTask (DisplayPosition, 4, "Position dis");

	(* From now on, all the work is done by Handler and the task	*)
	(* called Handler.  We stop when the user hits the keyboard.	*)

	ch := InKey();

	(* Write a summary to the screen. *)

	HideMouseCursor;
	WriteLn(w);
	WriteCard(w, EventCount[LeftDown]);
	WriteString(w, ' left presses, ');
	WriteCard (w, EventCount[LeftUp]);
	WriteString(w, ' left releases');
	WriteLn (w);
	WriteCard (w, EventCount[MiddleDown]);
	WriteString (w, ' middle presses, ');
	WriteCard (w, EventCount[MiddleUp]);
	WriteString (w, ' middle releases');
	WriteLn (w);
	WriteCard (w, EventCount[RightDown]);
	WriteString (w, ' right presses, ');
	WriteCard (w, EventCount[RightUp]);
	WriteString (w, ' right releases.');
	WriteLn (w);
	WriteLn (w);  WriteString (w, 'Hit any key to finish.');
	ShowMouseCursor;
	ch := InKey();
	CloseWindow (w);

    END RunTheTest;

(************************************************************************)
(*				MAIN PROGRAM				*)
(************************************************************************)

VAR j: Events;

BEGIN
    FOR j := LeftDown TO MiddleUp DO
	EventCount[j] := 0;
    END (*FOR*);

    CreateSemaphore (MouseEvent, 0);
    ResetMouse (HaveMouse, NumberOfButtons);

    IF HaveMouse THEN
	ShowMouseCursor;  RunTheTest;
    ELSE
	Beep;  Beep;  Beep;
    END (*IF*);

END TestMouse.
