MODULE RanTest;

	(********************************************************)
	(*							*)
	(*	    Test of random number generator		*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	17 March 1995			*)
	(*  Status:		OK				*)
	(*							*)
	(********************************************************)

FROM Random IMPORT
    (* proc *)	RANDOM, Randomize;

FROM RandCard IMPORT
    (* var  *)	seed;

FROM Windows IMPORT
    (* type *)	Window,
    (* proc *)	OpenSimpleWindow, CloseWindow, WriteString, WriteLn,
		PressAnyKey;

FROM NumericIO IMPORT
    (* proc *)	WriteCard, WriteRJLongCard;

FROM RealIO IMPORT
    (* proc *)	WriteReal;

(************************************************************************)
(*				SPEED TEST				*)
(************************************************************************)

PROCEDURE SpeedTest (TestSize: CARDINAL);

    (* Generates a lot of random numbers.  Relies on the keyboard user	*)
    (* having a stopwatch, because the timing is not built into this	*)
    (* module.								*)

    VAR j: CARDINAL;  result: REAL;

    BEGIN
	FOR j := 1 TO TestSize DO
	    result := RANDOM();
	END (*FOR*);
    END SpeedTest;

(************************************************************************)
(*			    PLAUSIBILITY TESTS				*)
(************************************************************************)

PROCEDURE TenThousandTest (w: Window);

    (* Starting with seed = 1, after 10000 calls we should end up with	*)
    (* seed = 1043618065.						*)

    VAR j: CARDINAL;  dummy: REAL;

    BEGIN
	Randomize(1);
	FOR j := 1 TO 10000 DO dummy := RANDOM(); END (*FOR*);
	WriteString (w, "The current value of seed is ");
	WriteRJLongCard (w, seed, 11);
	WriteString (w, ".  It should be 1043618065");
	WriteLn (w);
    END TenThousandTest;

(************************************************************************)

PROCEDURE DisplayNumbers (w: Window);

    CONST TestSize = 100;

    VAR j: CARDINAL;  result: REAL;

    BEGIN
	WriteString (w, "Start of plausibility test");  WriteLn (w);
	FOR j := 1 TO TestSize DO
	    result := RANDOM();
	    WriteReal (w, result, 6);
	    IF j MOD 8 <> 0 THEN
		WriteString (w, "  ");
	    ELSE
		WriteLn (w);
	    END (*IF*);
	END (*FOR*);
	WriteLn (w);
	WriteString (w, "End of plausibility test");  WriteLn (w);
    END DisplayNumbers;

(************************************************************************)

PROCEDURE RunTheTests;

    CONST TestSize = 50000;

    VAR w: Window;

    BEGIN
	OpenSimpleWindow (w, 0, 24, 0, 79);
	DisplayNumbers (w);  TenThousandTest (w);
	WriteString (w, "Start of speed test");  WriteLn (w);
	PressAnyKey (w);
	SpeedTest (TestSize);
	WriteString (w, "End of speed test, ");
	WriteCard (w, TestSize);  WriteString (w, " numbers generated");
	WriteLn (w);
	PressAnyKey (w);  CloseWindow (w);
    END RunTheTests;

(************************************************************************)
(*				MAIN PROGRAM				*)
(************************************************************************)

BEGIN
    RunTheTests;
END RanTest.
