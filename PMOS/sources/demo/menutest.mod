MODULE MenuTest;

	(********************************************************)
	(*							*)
	(*		Test of the "Menus" module.		*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	13 December 1994		*)
	(*  Status:		Working				*)
	(*							*)
	(********************************************************)

FROM Windows IMPORT
    (* type *)	Window, Colour, FrameType, DividerType,
    (* proc *)	OpenWindow, CloseWindow, WriteString, WriteLn;

FROM NumericIO IMPORT
    (* proc *)	WriteCard;

FROM Mouse IMPORT
    (* proc *)	MouseAvailable;

FROM UserInterface IMPORT
    (* type *)	UIWindow, Capability, CapabilitySet,
    (* proc *)	AllowMouseControl;

FROM Menus IMPORT
    (* type *)	Menu, ItemText, MO, MenuOption, OffEdgeOption,
    (* proc *)	CreateMenu, SetOptions, OffEdge, PositionMenu, DestroyMenu,
		SelectFromMenu;

(****************************************************************)

PROCEDURE DisplayAnswer (w: Window; number: CARDINAL);

    BEGIN
	WriteString (w, "The result is ");
	WriteCard (w, number);  WriteLn (w);
    END DisplayAnswer;

(****************************************************************)

PROCEDURE CreateMenu0(): Menu;

    (* Creates the top-level menu. *)

    VAR menu: Menu;
	menutext: ARRAY [0..6] OF ItemText;

    BEGIN
	menutext[0] := "MENU 0";
	menutext[1] := " \Default";
	menutext[2] := " \Border only";
	menutext[3] := " \Title only";
	menutext[4] := " \Naked";
	menutext[5] := "";
	menutext[6] := " E\xit";
	CreateMenu (menu, 6, menutext, 6);
	SetOptions (menu, MO{MNoTitle,MNoBorder,MNoClose,MFastSelect});
	OffEdge (menu, stick, return, wrap, wrap);
	PositionMenu (menu, 0, 0, 0, 79);
	RETURN menu;
    END CreateMenu0;

(****************************************************************)

PROCEDURE CreateMenu1(): Menu;

    (* Creates submenu number 1. *)

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
	PositionMenu (menu, 1, 8, 0, 25);
	RETURN menu;
    END CreateMenu1;

(****************************************************************)

PROCEDURE CreateMenu2(): Menu;

    (* Creates submenu number 2. *)

    VAR menu: Menu;
	menutext: ARRAY [0..6] OF ItemText;

    BEGIN
	menutext[0] := "MENU 2";
	menutext[1] := "Mary";
	menutext[2] := "had";
	menutext[3] := "a";
	menutext[4] := "little";
	menutext[5] := "roast";
	menutext[6] := "beef";
	CreateMenu (menu, 4, menutext, 6);
	SetOptions (menu, MO{MNoTitle,MNoMouse});
	OffEdge (menu, escape, wrap, wrap, wrap);
	PositionMenu (menu, 1, 3, 12, 40);
	RETURN menu;
    END CreateMenu2;

(****************************************************************)

PROCEDURE CreateMenu3(): Menu;

    (* Creates submenu number 3. *)

    VAR menu: Menu;
	menutext: ARRAY [0..3] OF ItemText;

    BEGIN
	menutext[0] := " MENU 3";
	menutext[1] := " Y\et";
	menutext[2] := " An\other";
	menutext[3] := " \menu";
	CreateMenu (menu, 1, menutext, 3);
	SetOptions (menu, MO{MNoBorder,MFastSelect});
	OffEdge (menu, escape, wrap, stick, stick);
	PositionMenu (menu, 1, 5, 28, 36);
	RETURN menu;
    END CreateMenu3;

(****************************************************************)

PROCEDURE CreateMenu4(): Menu;

    (* Creates submenu number 4. *)

    VAR menu: Menu;
	menutext: ARRAY [0..12] OF ItemText;

    BEGIN
	menutext[0] := "MENU 4";
	menutext[1] := "This";
	menutext[2] := "is";
	menutext[3] := "an";
	menutext[4] := "example";
	menutext[5] := "of";
	menutext[6] := "a";
	menutext[7] := "menu";
	menutext[8] := "without";
	menutext[9] := "a";
	menutext[10] := "border";
	menutext[11] := "or";
	menutext[12] := "title";
	CreateMenu (menu, 2, menutext, 0);
	SetOptions (menu, MO{MNoBorder,MNoTitle});
	OffEdge (menu, escape, wrap, wrap, wrap);
	PositionMenu (menu, 1, 6, 39, 55);
	RETURN menu;
    END CreateMenu4;

(****************************************************************)

PROCEDURE RunTheTest;

    VAR menu0, menu1, menu2, menu3, menu4: Menu;
	result: CARDINAL;
	resultwindow: Window;  UIW: UIWindow;

    BEGIN
	OpenWindow (resultwindow, white, magenta, 10, 24, 32, 70,
			doubleframe, nodivider);
	IF MouseAvailable() THEN
	    UIW := AllowMouseControl (resultwindow, "Menu selection result",
			CapabilitySet {wshow, wmove, whide});
	END (*IF*);
	menu0 := CreateMenu0();
	menu1 := CreateMenu1();
	menu2 := CreateMenu2();
	menu3 := CreateMenu3();
	menu4 := CreateMenu4();
	LOOP
	    result := SelectFromMenu (menu0);
	    WriteString (resultwindow, "Submenu ");
	    WriteCard (resultwindow, result);
	    WriteString (resultwindow, ": ");
	    CASE result OF
		| 0,6:	EXIT (*LOOP*);
		| 1:	result := SelectFromMenu (menu1);
		| 2:	result := SelectFromMenu (menu2);
		| 3:	result := SelectFromMenu (menu3);
		| 4:	result := SelectFromMenu (menu4);
		| 5:	WriteString (resultwindow, "Not a valid menu");
			WriteLn (resultwindow);
	    END (*CASE*);
	    DisplayAnswer (resultwindow, result);
	END (*LOOP*);
	DestroyMenu (menu4);
	DestroyMenu (menu3);
	DestroyMenu (menu2);
	DestroyMenu (menu1);
	DestroyMenu (menu0);
	CloseWindow (resultwindow);
    END RunTheTest;

(****************************************************************)
(*			    MAIN PROGRAM			*)
(****************************************************************)

BEGIN
    RunTheTest;
END MenuTest.
