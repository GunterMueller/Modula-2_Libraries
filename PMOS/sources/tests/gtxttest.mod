MODULE GTxtTest;

	(********************************************************)
	(*							*)
	(*	Test of text operations in graphics window	*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	30 December 1994		*)
	(*  Status:		Working				*)
	(*							*)
	(********************************************************)

FROM GWindows IMPORT
    (* type *)	Window, BorderType,
    (* proc *)	WriteChar, WriteString, WriteLn,
		OpenWindow, CloseWindow;

FROM Keyboard IMPORT
    (* proc *)	InKey;

(************************************************************************)

PROCEDURE Pause (w: Window);

    VAR dummy: CHAR;

    BEGIN
	WriteLn (w);
	WriteString (w, "Press any key to continue");
	dummy := InKey();
    END Pause;

(************************************************************************)

PROCEDURE RunTheTest;

    CONST TestLength = 3000;

    VAR w: Window;  j: CARDINAL;  ch: CHAR;

    BEGIN
	OpenWindow (w, 0, 0, 620, 400, 0, 2, double);
	WriteString (w,'Test of text operations in a graphics window.');
	Pause (w);
	WriteLn(w);

	ch := CHR(0);
	FOR j := 0 TO TestLength DO
	    WriteChar (w, ch);
	    IF ch = MAX(CHAR) THEN ch := CHR(0)
	    ELSE INC(ch)
	    END (*IF*);
	END (*FOR*);

	Pause (w);
	CloseWindow (w);

    END RunTheTest;

(************************************************************************)

BEGIN
    RunTheTest;
END GTxtTest.
