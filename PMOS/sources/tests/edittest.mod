MODULE EditTest;

	(********************************************************)
	(*							*)
	(*		Test of module ScreenEditor		*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	14 December 1994		*)
	(*  Status:		Working but incomplete		*)
	(*							*)
	(*	This program tests only a subset of the		*)
	(*	options supported by ScreenEditor.  More	*)
	(*	work needed on further tests.			*)
	(*							*)
	(********************************************************)

FROM Windows IMPORT
    (* type *)	Window, Colour, FrameType, DividerType,
    (* proc *)	OpenWindow, CloseWindow, SetCursor, WriteString, PressAnyKey;

FROM NumericIO IMPORT
    (* proc *)	WriteRJCard;

FROM RealIO IMPORT
    (* proc *)	WriteReal;

FROM Menus IMPORT
    (* type *)	Menu, ItemText, MenuOption, MO,
    (* proc *)	CreateMenu, SetOptions;

FROM ScreenEditor IMPORT
    (* type *)	Structure,
    (* proc *)	CardinalField, RealField, MenuField, Combine, MakeArray,
		ScreenEdit;

(************************************************************************)

CONST maxsubscript = 10;

TYPE
    subscript = [1..maxsubscript];
    TestRecord = RECORD
		    part1: REAL;
		 END (*RECORD*);

VAR
    TestData: ARRAY subscript OF TestRecord;

(************************************************************************)

PROCEDURE CreateAMenu(): Menu;

    (* Creates a menu. *)

    VAR menu: Menu;
	menutext: ARRAY [0..25] OF ItemText;

    BEGIN
	menutext[0] := "MENU 1";
	menutext[1] := "The";
	menutext[2] := "quick";
	menutext[3] := "brown";
	menutext[4] := "fox";
	menutext[5] := "jumps";
	menutext[6] := "over";
	menutext[7] := "the";
	menutext[8] := "lazy";
	menutext[9] := "dog";
	menutext[10] := "Now";
	menutext[11] := "is";
	menutext[12] := "the";
	menutext[13] := "time";
	menutext[14] := "for";
	menutext[15] := "all";
	menutext[16] := "good";
	menutext[17] := "men";
	menutext[18] := "to";
	menutext[19] := "come";
	menutext[20] := "to";
	menutext[21] := "the";
	menutext[22] := "aid";
	menutext[23] := "of";
	menutext[24] := "the";
	menutext[25] := "party";
	CreateMenu (menu, 3, menutext, 25);
	SetOptions (menu, MO{MNoBorder});
	RETURN menu;
    END CreateAMenu;

(****************************************************************)

PROCEDURE RunTheTest;

    VAR w: Window;  R, S, count: Structure;  abort: BOOLEAN;
	x: CARDINAL;  y: REAL;  z: REAL;  graphics: BOOLEAN;
	menuvar: CARDINAL;  row: CARDINAL;

    BEGIN
	OpenWindow (w, black, green, 1, 18, 10, 69, simpleframe, nodivider);

	(* Start with a structure containing a few simple variables. *)

	x := 1234;  y := 3.14159;  graphics := FALSE;
	SetCursor (w, 3, 2);  WriteString (w, "x      [");
	SetCursor (w, 3, 18);  WriteString (w, "]");
	SetCursor (w, 5, 2);  WriteString (w, "y      [");
	SetCursor (w, 5, 20);  WriteString (w, "]");
	SetCursor (w, 5, 30);  WriteString (w, "z      [");
	SetCursor (w, 5, 50);  WriteString (w, "]");
	(*
	SetCursor (w, 7, 2);  WriteString (w, "graphics");
	SetCursor (w, 7, 10);  WriteString (w, "OFF");
	*)

	R := CardinalField (x, 3, 10, 8);
	Combine (R, RealField (z, 5, 40, 10));
	Combine (R, RealField (y, 5, 10, 10));

   (*	AddBooleanField (S, graphics, 7, 10, "NO", "YES");  *)

   (*	ScreenEdit (w, R, abort);   *)

	(* Include elements of an array. *)

	Combine (R, RealField (TestData[1].part1, 1, 3, 10));
	Combine (R, RealField (TestData[2].part1, 2, 3, 10));

	(* Add a menu field. *)

	menuvar := 2;
	Combine (R, MenuField (menuvar, 7, 5, 6, 30, CreateAMenu()));

	(* Go to it: do some editing. *)

	ScreenEdit (w, R, abort);

	SetCursor (w, 14, 1);  WriteString (w, "x, y, z = ");
	WriteRJCard (w, x, 8);  WriteString (w, ", ");
	WriteReal (w, y, 10);  WriteString (w, ", ");
	WriteReal (w, z, 10);
	SetCursor (w, 15, 1);  WriteString (w, "graphics is o");
	IF graphics THEN WriteString (w, "n");
	ELSE WriteString (w, "ff");
	END (*IF*);

	(* Test 3: repeating structure. *)
(*
	S := MakeArray (R, maxsubscript, SIZE(TestRecord), 1, 0);
	ScreenEdit (w, S);
*)
	PressAnyKey (w);
	PressAnyKey (w);
	CloseWindow (w);
    END RunTheTest;

(************************************************************************)
(*				MAIN PROGRAM				*)
(************************************************************************)

BEGIN
    RunTheTest;
END EditTest.
