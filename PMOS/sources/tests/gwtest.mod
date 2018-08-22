MODULE GWTest;

	(********************************************************)
	(*							*)
	(*	Very elementary test of GWindows: opens some	*)
	(*	graphics windows, and then closes them.  The	*)
	(*	purpose is to check the tiling algorithms.	*)
	(*							*)
	(*	(If you run this program "as is", all you'll	*)
	(*	see is a brief flash on the screen.)		*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	5 March 1995			*)
	(*  Status:		OK				*)
	(*							*)
	(********************************************************)

FROM GWindows IMPORT
    (* type *)	Window, BorderType,
    (* proc *)	OpenWindow, WriteString, CloseWindow;

FROM Graphics IMPORT
    (* proc *)	SetMode, GraphicsOff;

FROM Keyboard IMPORT
    (* proc *)	InKey;

(************************************************************************)
(*		    "PRESS ANY KEY TO CONTINUE"				*)
(************************************************************************)

PROCEDURE Pause;

    VAR dummy: CHAR;  w: Window;

    BEGIN
	OpenWindow(w,280,30,510,45,0,3,single);
	WriteString (w, "  Press any key to continue");
	dummy := InKey();
	CloseWindow (w);
    END Pause;

(************************************************************************)

PROCEDURE DebugPause (message: ARRAY OF CHAR);

    VAR dummy: CHAR;

    BEGIN
(*
	GlassTTY.SetCursor (24, 0);
	GlassTTY.WriteString (message);
	dummy := InKey();
*)
    END DebugPause;

(************************************************************************)

PROCEDURE RunTheTest;

    VAR w1, w2, w3, w4: Window;

    BEGIN
	DebugPause ("About to open first window");
	OpenWindow(w1,300,100,600,260,4,7,single);
	OpenWindow(w2,20,20,400,180,0,2,single);
	OpenWindow(w3,50,120,500,140,0,3,double);
	OpenWindow(w4,350,10,370,300,0,5,single);
	CloseWindow (w1);
	CloseWindow (w2);
	CloseWindow (w3);
	CloseWindow (w4);
    END RunTheTest;

(************************************************************************)

BEGIN
    (* SetMode(3,TRUE); *)
    (* GraphicsOff (TRUE); *)
    RunTheTest;
END GWTest.
