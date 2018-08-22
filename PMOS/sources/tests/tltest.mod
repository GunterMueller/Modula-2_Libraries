MODULE TLTest;

	(********************************************************)
	(*							*)
	(*		Test of module TextLines		*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	23 June 1994			*)
	(*  Status:		Working				*)
	(*							*)
	(********************************************************)

FROM Windows IMPORT
    (* type *)	Window, Colour, FrameType, DividerType,
    (* proc *)	OpenWindow, CloseWindow, PressAnyKey, SetCursor;

FROM TextLines IMPORT
    (* type *)	LineType,
    (* proc *)	HLine, VLine, Box;

(************************************************************************)

PROCEDURE Pause (w: Window);

    BEGIN
	SetCursor (w, 0, 0);
	PressAnyKey (w);
    END Pause;

(************************************************************************)

PROCEDURE RunTheTest;

    VAR w: Window;

    BEGIN
	OpenWindow (w, black, green, 0, 24, 0, 79, noframe, nodivider);

	(* Start with a few sample lines, which intersect. *)

	HLine (w, 10, 20, 50, single);
	HLine (w, 15, 20, 50, double);
	VLine (w, 25, 7, 18, single);
	VLine (w, 40, 7, 18, double);

	(* Draw a box, add a divider bar, then delete divider bar. *)

	Pause(w);
	Box (w, 12, 30, 15, 10, double);
	HLine (w, 14, 30, 45, single);
	Pause (w);
	HLine (w, 14, 30, 45, none);

	(* Add another box, and fiddle with its divider bar. *)

	Pause(w);
	Box (w, 5, 35, 15, 8, single);
	HLine (w, 7, 35, 50, double);
	Pause (w);
	HLine (w, 7, 35, 50, single);
	Pause (w);
	HLine (w, 7, 35, 50, none);

	(* Delete the first box. *)

	Pause(w);
	Box (w, 12, 30, 15, 10, none);

	Pause(w);
	CloseWindow (w);
    END RunTheTest;

(************************************************************************)

BEGIN
    RunTheTest;
END TLTest.
