MODULE ColourTest;

	(****************************************************************)
	(*								*)
	(*	To check out how various colours look on a		*)
	(*	monochrome display.					*)
	(*								*)
	(*	Conclusions so far:					*)
	(*	1.	The following combinations are OK:		*)
	(*								*)
	(*	2.	The following combinations are not OK:		*)
	(*								*)
	(*	3.	Anything else - not yet tested.			*)
	(*								*)
	(****************************************************************)

FROM Windows IMPORT
    (* type *)	Colour, Window, FrameType, DividerType,
    (* proc *)	OpenWindow, CloseWindow, WriteString, SetCursor;

FROM Trace IMPORT
    (* proc *)	NYI, Pause;

(************************************************************************)

PROCEDURE WriteColour (w: Window; col: Colour);

    BEGIN
	CASE col OF
	    black:	WriteString (w, "black     ");
	|   blue:	WriteString (w, "blue      ");
	|   green:	WriteString (w, "green     ");
	|   cyan:	WriteString (w, "cyan      ");
	|   red:	WriteString (w, "red       ");
	|   magenta:	WriteString (w, "magenta   ");
	|   brown:	WriteString (w, "brown     ");
	|   white:	WriteString (w, "white     ");
	    ELSE	WriteString (w, "unknown   ");
	END (*CASE*);
    END WriteColour;

(************************************************************************)

PROCEDURE DoTheTest;

    VAR test, monitor: Window;
	foreground, background: Colour;

    BEGIN
	OpenWindow (monitor, white, black, 5, 10, 10, 70,
			simpleframe, nodivider);
	FOR background := black TO white DO
	    SetCursor (monitor, 1, 1);
	    WriteString (monitor, "Background code is ");
	    WriteColour (monitor, background);
	    FOR foreground := black TO white DO
		SetCursor (monitor, 2, 1);
		WriteString (monitor, "Foreground code is ");
		WriteColour (monitor, foreground);
		OpenWindow (test, foreground, background, 15, 20, 10, 70,
						simpleframe, nodivider);
		WriteString (test, "Test message");
		Pause;
		CloseWindow (test);
	    END (*FOR*);
	END (*FOR*);
    END DoTheTest;

(************************************************************************)
(*				MAIN PROGRAM				*)
(************************************************************************)

BEGIN
    DoTheTest;
END ColourTest.
