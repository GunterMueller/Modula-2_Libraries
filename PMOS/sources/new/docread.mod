MODULE DocRead;

	(********************************************************)
	(*							*)
	(*	Program to display documentation files		*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	11 December 1994		*)
	(*  Status:		Just started, incomplete	*)
	(*							*)
	(*	At present I'm just playing around with ideas;	*)
	(*	it would take a lot of work to turn this into	*)
	(*	a hypertext reader.				*)
	(*	Thought: we need to distinguish between		*)
	(*	hypertext files and "raw text" files - e.g. in	*)
	(*	the latter we should keep the existing line	*)
	(*	breaks, in the former we should treat carriage	*)
	(*	return characters like whitespace.		*)
	(*							*)
	(*	Such details can however wait until I've done	*)
	(*	improvements to the menu system, i.e.  for now	*)
	(*	this is a test program for menus.		*)
	(*							*)
	(********************************************************)

FROM TaskControl IMPORT
    (* proc *)	CreateTask;

FROM Semaphores IMPORT
    (* type *)	Semaphore,
    (* proc *)	CreateSemaphore, Wait, Signal;

FROM Windows IMPORT
    (* type *)	Window, Colour, FrameType, DividerType, Rectangle,
    (* proc *)	OpenWindow, CloseWindow, WriteString, PressAnyKey,
		WriteChar, WriteLn, WindowLocation;

FROM NumericIO IMPORT
    (* proc *)	WriteCard;

FROM Menus IMPORT
    (* type *)	Menu, ItemText, MenuOption, MO, OffEdgeOption,
    (* proc *)	CreateMenu, PositionMenu, SelectFromMenu, DestroyMenu,
		SetOptions, OffEdge;

FROM IOErrorCodes IMPORT
    (* type *)	ErrorCode;

FROM FileSys IMPORT
    (* type *)	File,
    (* proc *)	OpenFile, CloseFile, EOF, ReadByte,
		SetPosition, SavePosition;

(************************************************************************)

TYPE
    CharSet = SET OF CHAR;

CONST
    alphanum = CharSet {"A".."Z", "a".."z", "0".."9"};

VAR
    done: Semaphore;

(************************************************************************)
(*			FILE DISPLAY PROCEDURES				*)
(************************************************************************)

PROCEDURE BackSpace (f: File);

    (* Moves backwards by one byte in file f. *)

    VAR dummy: ErrorCode;

    BEGIN
	dummy := SetPosition (f, SavePosition(f) - 1);
    END BackSpace;

(************************************************************************)

PROCEDURE SkipBlanks (f: File);

    (* Skips over nonsignificant characters in file f. *)

    CONST BlankChars = CharSet {CHR(0), CHR(10), " "};

    BEGIN
	WHILE CHAR(ReadByte(f)) IN BlankChars DO
	    (* nothing *)
	END (*WHILE*);
	BackSpace (f);
    END SkipBlanks;

(************************************************************************)

PROCEDURE ReadWord (f: File;  VAR (*OUT*) word: ARRAY OF CHAR;
				VAR (*OUT*) terminator: CHAR): CARDINAL;

    (* Reads an alphanumeric string, returns the string in "word" and	*)
    (* the trailing nonalphanumeric character in "terminator".  The	*)
    (* function result is the length of "word".				*)

    CONST Nul = CHR(0);

    VAR j: CARDINAL;  ch: CHAR;

    BEGIN
	SkipBlanks(f);
	j := 0;  terminator := CHR(0);
	LOOP
	    IF j > HIGH(word) THEN EXIT(*LOOP*) END(*IF*);
	    ch := ReadByte (f);
	    IF ch IN alphanum THEN
		word[j] := ch;  INC(j);
	    ELSE
		terminator := ch;  word[j] := Nul;  EXIT (*LOOP*);
	    END (*IF*);
	END (*LOOP*);
	RETURN j;
    END ReadWord;

(************************************************************************)

PROCEDURE DisplayFile (w: Window;  f: File);

    (* Displays file f in window w.  The file and window must already	*)
    (* be open.								*)

    CONST Space = " ";

    VAR R: Rectangle;
	row, col, maxrow, maxcol, length: CARDINAL;
	word: ARRAY [0..79] OF CHAR;
	terminator: CHAR;

    BEGIN
	WriteLn (w);
	WriteString (w, "Procedure DisplayFile not yet written.");
	R := WindowLocation (w);
	maxrow := R.bottom - R.top + 1;
	maxcol := R.right - R.left + 1;
	row := 1;  col := 0;
	LOOP
	    IF EOF(f) THEN EXIT(*LOOP*) END (*IF*);
	    IF row > maxrow THEN EXIT (*LOOP*) END(*IF*);
	    length := ReadWord (f, word, terminator);
	    IF terminator <> Space THEN INC(length) END(*IF*);
	    IF col+length > maxcol THEN
		WriteLn (w);  INC (row);  col := 0;
	    END (*IF*);
	    WriteString (w, word);
	    IF terminator <> Space THEN
		WriteChar (w, terminator);
	    END (*IF*);
	    INC (col, length);
	    IF (word[0] <> CHR(0)) AND (col < maxcol) THEN
		WriteChar (w, Space);  INC(col);
	    END (*IF*);
	END (*LOOP*);
    END DisplayFile;

(************************************************************************)

PROCEDURE DoDisplay (filename: ARRAY OF CHAR);

    (* Displays the given file. *)

    VAR w: Window;  f: File;  status: ErrorCode;

    BEGIN
	OpenWindow (w, white, magenta, 5, 20, 0, 79, noframe, nodivider);
	status := OpenFile (f, filename, FALSE);
	WriteLn (w);  WriteString (w, "Entered procedure DoDisplay");
	IF status <> OK THEN
	    WriteLn (w);  WriteString (w, "Can't open file ");
	    WriteString (w, filename);
	ELSE
	    DisplayFile (w, f);
	END (*IF*);
	PressAnyKey (w);
	CloseFile (f);
	CloseWindow (w);

    END DoDisplay;

(************************************************************************)

PROCEDURE DisplayTask;

    BEGIN
	DoDisplay ("..\doc\status.doc");
	Signal (done);
    END DisplayTask;

(************************************************************************)
(*				MENU OPERATIONS				*)
(*	At present this part of the program is just a test of Menus.	*)
(************************************************************************)

PROCEDURE Actions2 (option: CARDINAL);

    (* Implements the actions on submenu number 2. *)

    BEGIN
    END Actions2;

(************************************************************************)

PROCEDURE MenuTask;

    (* Runs as a separate task, picking up menu commands. *)

    VAR topmenu, menu1, menu2: Menu;  w: Window;
	topmenutext: ARRAY [0..5] OF ItemText;
	menu1text: ARRAY [0..1] OF ItemText;
	menu2text: ARRAY [0..2] OF ItemText;
	result: CARDINAL;

    BEGIN
	OpenWindow (w, black, white, 21, 24, 50, 79, noframe, nodivider);

	(* Create top-level menu. *)

	topmenutext[0] := "TOP MENU";
	topmenutext[1] := " \File";
	topmenutext[2] := " \Options";
	topmenutext[3] := " dummy\3";
	topmenutext[4] := " dummy\4";
	topmenutext[5] := " dummy\5";
	CreateMenu (topmenu, 5, topmenutext, 5);
	SetOptions (topmenu, MO{MNoTitle,MNoBorder,MNoClose,MNoKeyBack,
				MFastSelect});
	OffEdge (topmenu, stick, return, wrap, wrap);
	PositionMenu (topmenu, 0, 0, 0, 79);

	(* Create subsidiary menus. *)

	menu1text[0] := "MENU 1";
	menu1text[1] := "E\xit";
	CreateMenu (menu1, 1, menu1text, 1);
	SetOptions (menu1, MO{MNoTitle,MFastSelect});
	OffEdge (menu1, escape, wrap, stick, escape);
	PositionMenu (menu1, 1, 3, 0, 10);

	menu2text[0] := "MENU 2";
	menu2text[1] := "Page \up";
	menu2text[2] := "Page \down";
	CreateMenu (menu2, 1, menu2text, 2);
	SetOptions (menu2, MO{MNoTitle,MFastSelect});
	OffEdge (menu2, escape, wrap, escape, escape);
	PositionMenu (menu2, 1, 4, 14, 24);

	REPEAT
	    result := SelectFromMenu (topmenu);
	    WriteLn (w);
	    WriteString (w, "Selected top menu item ");
	    WriteCard (w, result);
	    CASE result OF
		| 1:	result := SelectFromMenu(menu1);
			result := 1 - result;
		| 2:	Actions2 (SelectFromMenu(menu2));
	    END (*CASE*);
	UNTIL result = 0;
	DestroyMenu (menu1);
	DestroyMenu (topmenu);
	(* PressAnyKey (w); *)
	CloseWindow (w);
	Signal (done);

    END MenuTask;

(************************************************************************)
(*				MAIN PROGRAM				*)
(************************************************************************)

BEGIN
    CreateSemaphore (done, 0);
    (* CreateTask (DisplayTask, 2, "display task"); *)
    CreateTask (MenuTask, 3, "menu task");
    Wait (done);
    (* Wait (done); *)
END DocRead.
