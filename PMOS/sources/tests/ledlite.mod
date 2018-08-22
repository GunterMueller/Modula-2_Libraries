MODULE LedLite;

	(********************************************************)
	(*							*)
	(*	Test program to display a pattern on the	*)
	(*		    keyboard LEDs			*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	20 January 1993			*)
	(*  Status:		OK				*)
	(*							*)
	(********************************************************)

FROM KBdriver IMPORT
    (* proc *)	PutLEDs, CheckScanCode, ClearLED;

FROM Timer IMPORT
    (* proc *)	Sleep;

FROM LowLevel IMPORT
    (* proc *)	IAND;

FROM TerminationControl IMPORT
    (* proc *)	SetTerminationProcedure;

(************************************************************************)

VAR
    GoingLeft: BOOLEAN;

(************************************************************************)
(*	THIS PROCEDURE DEFINES THE SEQUENCE OF THE PATTERN		*)
(************************************************************************)

PROCEDURE Next (x: CARDINAL): CARDINAL;

    (* Produces the next number in the sequence to be displayed.	*)
    (* Alter this code to change the pattern sequence.			*)

    BEGIN
	IF GoingLeft THEN
	    IF x >= 4 THEN GoingLeft := FALSE END (*IF*)
	ELSE
	    IF x = 1 THEN GoingLeft := TRUE; END (*IF*);
	END (*IF*);
	IF GoingLeft THEN
	    IF x = 0 THEN x := 1 ELSE x := 2*x END (*IF*);
	ELSE
	    x := x DIV 2;
	END (*IF*);
	RETURN x;
    END Next;

(************************************************************************)
(*			     MAIN PROCEDURE				*)
(************************************************************************)

PROCEDURE Display;

    (* Displays a pattern on the keyboard LEDs.  *)

    CONST delay = 400;	(* milliseconds *)
	  EscRelease = BYTE(081H);	(* scan code for Esc key release *)

    VAR x: CARDINAL;

    BEGIN
	x := 1;  GoingLeft := TRUE;
	REPEAT
	    PutLEDs (BYTE(IAND(x,7)));
	    Sleep (delay);
	    x := Next (x);
	UNTIL CheckScanCode() = EscRelease;
    END Display;

(************************************************************************)
(*			     FINAL CLEANUP				*)
(************************************************************************)

PROCEDURE ClearAllLEDs;

    BEGIN
	ClearLED (7);
    END ClearAllLEDs;

(************************************************************************)
(*			     MAIN PROGRAM				*)
(************************************************************************)

BEGIN
    SetTerminationProcedure (ClearAllLEDs);
    Display;
END LedLite.
