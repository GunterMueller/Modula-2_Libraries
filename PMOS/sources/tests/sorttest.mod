MODULE SortTest;

	(********************************************************)
	(*							*)
	(*		Test of module QuickSort		*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	6 March 1995			*)
	(*  Status:		Working				*)
	(*							*)
	(********************************************************)

FROM QuickSortModule IMPORT
    (* proc *)	QuickSort;

FROM RandCard IMPORT
    (* proc *)	RandCardinal;

FROM Windows IMPORT
    (* type *)	Window, Colour, FrameType, DividerType,
    (* proc *)	OpenWindow, CloseWindow, WriteLn, PressAnyKey,
		WriteString, OpenSimpleWindow;

FROM NumericIO IMPORT
    (* proc *)	WriteRJLongCard;

(************************************************************************)

CONST testing = TRUE;
VAR debug: Window;

TYPE
    Datum = LONGCARD;
    DataPtr = POINTER TO Datum;

(************************************************************************)

PROCEDURE GreaterOrEqual (p1, p2: DataPtr): BOOLEAN;

    (* Comparison of two data elements.  Returns TRUE iff p1^ >= p2^.	*)

    BEGIN
(*
	IF testing THEN
	    WriteLn (debug);  WriteString (debug, "Comparing ");
	    WriteRJCard (debug, p1^, 8);
	    WriteRJCard (debug, p2^, 8);
	END (*IF*);
*)
	RETURN p1^ >= p2^;
    END GreaterOrEqual;

(************************************************************************)

PROCEDURE RunTheTest;

    CONST TestSize = 300;

    VAR data: ARRAY [0..TestSize-1] OF Datum;
	w: Window;
	j: CARDINAL;

    BEGIN
	OpenWindow (w, cyan, blue, 0, 24, 0, 79,
					noframe, nodivider);

	(* Generate some test data *)

	FOR j := 0 TO TestSize-1 DO
	    data[j] := RandCardinal();
	END (*FOR*);

	(* Write out the test data *)

	WriteLn (w);  WriteString (w, "Original data");
	FOR j := 0 TO TestSize-1 DO
	    IF j MOD 5 = 0 THEN
		WriteLn (w);
	    END (*IF*);
	    WriteRJLongCard (w, data[j], 14);
	END (*FOR*);
	WriteLn (w);
	PressAnyKey (w);

	(* Perform the sort *)

	QuickSort (data, TestSize-1, SIZE(Datum), GreaterOrEqual);

	(* Write out the sorted array *)

	WriteLn (w);  WriteString (w, "Sorted data");
	FOR j := 0 TO TestSize-1 DO
	    IF j MOD 5 = 0 THEN
		WriteLn (w);
	    END (*IF*);
	    WriteRJLongCard (w, data[j], 14);
	END (*FOR*);

	PressAnyKey (w);
	CloseWindow (w);
    END RunTheTest;

(************************************************************************)
(*				MAIN PROGRAM				*)
(************************************************************************)

BEGIN
    IF testing THEN
	OpenSimpleWindow (debug, 8, 15, 0, 79);
    END (*IF*);
    RunTheTest;
END SortTest.
