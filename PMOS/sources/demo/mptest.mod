MODULE MPTest;

	(********************************************************)
	(*							*)
	(*	Test of the "maintenance page" facility		*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	14 February 1994		*)
	(*  Status:		Working				*)
	(*	Bugs:						*)
	(*		None known at present			*)
	(*	Shortcomings:					*)
	(*	   1.	(Fixed)					*)
	(*	   2.	Mapping the same window to several	*)
	(*		different groups is almost, but not	*)
	(*		quite, supported.  I think that the	*)
	(*		only enhancement needed to support this	*)
	(*		is a better way to decide whether to	*)
	(*		hide a newly introduced window.		*)
	(*	   3.	Need to look into whether the order of	*)
	(*		Unhide operations is satisfactory.	*)
	(*							*)
	(********************************************************)

FROM TaskControl IMPORT
    (* proc *)	CreateTask;

FROM Timer IMPORT
    (* proc *)	Sleep;

FROM Semaphores IMPORT
    (* type *)	Semaphore,
    (* proc *)	CreateSemaphore, Wait;

FROM Keyboard IMPORT
    (* proc *)	HotKey;

IMPORT Floppy;	(* for the sake of displaying an extra maintenance page *)

FROM Windows IMPORT
    (* type *)	Window, Colour, FrameType, DividerType,
    (* proc *)	OpenWindow, ChangeScrollingRegion, CloseWindow, PressAnyKey,
		WriteChar, WriteString, WriteLn, ShiftWindowRel,
		ReadChar, ReadCharWithoutEcho;

FROM MaintenancePages IMPORT
    (* type *)	MaintenancePage,
    (* proc *)	CreateMaintenancePage, Associate;

FROM UserInterface IMPORT
    (* type *)	UIWindow, Capability, CapabilitySet,
    (* proc *)	AllowMouseControl;

FROM RandCard IMPORT
    (* proc *)	RandCardinal;

(************************************************************************)

VAR page1, page2: MaintenancePage;

(************************************************************************)

PROCEDURE Task1;

    VAR w1: Window;  dummy: UIWindow;

    BEGIN
	OpenWindow (w1, blue, cyan, 6, 12, 8, 34, simpleframe, nodivider);
	Associate (w1, page1);
	dummy := AllowMouseControl (w1, "Task 1",
				CapabilitySet {wshow, whide, wmove});
	LOOP
	   WriteString (w1, "Message from Task 1 ... ");
	   Sleep (400);
	END (*LOOP*);
	PressAnyKey (w1);
	CloseWindow (w1);
    END Task1;

(************************************************************************)

PROCEDURE Task2;

    VAR w2: Window;  dummy: UIWindow;

    BEGIN
	OpenWindow (w2, white, magenta, 12, 22, 20, 63, doubleframe, nodivider);
	Associate (w2, page2);
	dummy := AllowMouseControl (w2, "Task 2",
				CapabilitySet {wshow, whide, wmove});
	LOOP
	    WriteString (w2, "This is output from Task 2 on Page 2 ... ");
	    Sleep (600);
	END (*LOOP*);
	PressAnyKey (w2);
	CloseWindow (w2);
    END Task2;

(************************************************************************)

PROCEDURE Task3;

    VAR w1: Window;  dummy: UIWindow;

    BEGIN
	OpenWindow (w1, black, green, 10, 20, 50, 78, doubleframe, nodivider);
	Associate (w1, page1);
	dummy := AllowMouseControl (w1, "Task 3",
				CapabilitySet {wshow, whide, wmove});
	LOOP
	   WriteString (w1, "Task 3 also uses page 1 ... ");
	   Sleep (200);
	END (*LOOP*);
	PressAnyKey (w1);
	CloseWindow (w1);
    END Task3;

(************************************************************************)

PROCEDURE Random(): INTEGER;

    (* Returns a small unsigned random number.	*)

    BEGIN
	RETURN INTEGER (RandCardinal() MOD 3) - 1;
    END Random;

(************************************************************************)

PROCEDURE Task4;

    VAR w1: Window;  count: CARDINAL;  flip: BOOLEAN;

    BEGIN
	OpenWindow (w1, brown, black, 3, 6, 60, 66, simpleframe, nodivider);
	Associate (w1, page1);
	count := 0;  flip := FALSE;
	LOOP
	    INC (count);
	    IF count >= 5 THEN
		ShiftWindowRel (w1, Random(), Random());
		count := 0;
	    END (*IF*);
	    flip := NOT flip;
	    IF flip THEN WriteString (w1, "/\/\/")
	    ELSE WriteString (w1, "\/\/\")
	    END (*IF*);
	    Sleep (400);
	END (*LOOP*);
	PressAnyKey (w1);
	CloseWindow (w1);
    END Task4;

(************************************************************************)

PROCEDURE InputTask1;

    VAR w1: Window;  dummy: UIWindow;  ch: CHAR;

    BEGIN
	OpenWindow (w1, black, white, 14, 20, 3, 30, doubleframe, nodivider);
	Associate (w1, page1);
	dummy := AllowMouseControl (w1, "Input Task 1",
				CapabilitySet {wshow, whide, wmove});
	LOOP
	   WriteLn (w1);
	   WriteString (w1, "Press any key");
	   ReadCharWithoutEcho (w1, ch);
	   WriteLn (w1);
	   WriteString (w1, "The key you pressed was ");
	   WriteChar (w1, ch);
	END (*LOOP*);
	PressAnyKey (w1);
	CloseWindow (w1);
    END InputTask1;

(************************************************************************)

PROCEDURE InputTask2;

    VAR w1: Window;  dummy: UIWindow;  ch: CHAR;

    BEGIN
	OpenWindow (w1, magenta, black, 4, 9, 40, 70,simpleframe,simpledivider);
	Associate (w1, page1);
	dummy := AllowMouseControl (w1, "Input Task 2",
				CapabilitySet {wshow, whide, wmove});
	WriteString (w1, "You can type into this window");
	ChangeScrollingRegion (w1, 3, 4);
	LOOP
	   ReadChar (w1, ch);
	END (*LOOP*);
	PressAnyKey (w1);
	CloseWindow (w1);
    END InputTask2;

(************************************************************************)

PROCEDURE RunTheTest;

    CONST Esc = CHR(1BH);

    VAR w0, header1, header2: Window;
	closedown: Semaphore;

    BEGIN
	CreateSemaphore (closedown, 0);
	HotKey (FALSE, Esc, closedown);
	OpenWindow (w0, yellow, red, 8, 13, 20, 59, simpleframe, nodivider);
	WriteString (w0, "TEST OF THE MAINTENANCE PAGE FACILITY");
	WriteLn (w0);
	WriteString (w0, "Type ^P to toggle maintenance function");
	WriteLn (w0);
	WriteString (w0, "  Then F6 to cycle through the pages");
	WriteLn (w0);
	WriteString (w0, "         Esc to exit program");
	CreateMaintenancePage (page1);
	OpenWindow (header1, white, black, 1, 3, 30, 49, doubleframe, nodivider);
	Associate (header1, page1);
	WriteString (header1, "MAINTENANCE PAGE 1");
	CreateMaintenancePage (page2);
	OpenWindow (header2, white, black, 1, 3, 30, 49, doubleframe, nodivider);
	Associate (header2, page2);
	WriteString (header2, "MAINTENANCE PAGE 2");
	CreateTask (Task1, 2, "Task 1");
	CreateTask (Task2, 2, "Task 2");
	CreateTask (Task3, 2, "Task 3");
	CreateTask (Task4, 2, "Task 4");
	CreateTask (InputTask1, 2, "Input Task 1");
	CreateTask (InputTask2, 2, "Input Task 2");
	Wait (closedown);
    END RunTheTest;

(************************************************************************)
(*				MAIN PROGRAM				*)
(************************************************************************)

BEGIN
    RunTheTest;
END MPTest.
