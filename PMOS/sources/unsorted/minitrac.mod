IMPLEMENTATION MODULE MiniTrace;

	(************************************************)
	(*						*)
	(*	    Trace routines for Modula-2		*)
	(*		program development.		*)
	(*						*)
	(*  Programmer:		P. Moylan		*)
	(*  Last edited:	16 August 1992		*)
	(*  Status:		OK			*)
	(*						*)
	(************************************************)

FROM MyWindows IMPORT
    (* type *)	Window,
    (* proc *)	OpenSimpleWindow, CloseWindow, WriteString, WriteLn,
		ReadChar, EraseLine, CursorUp;

VAR TraceWindowOpen, TraceEnabled: BOOLEAN;
    nesting: CARDINAL;  PauseLength: CARDINAL;
    TraceWindow: Window;

(************************************************************************)

PROCEDURE Pause;

    (* Types a "Press any key to continue" message.	*)
    (* A temporary trace window is opened if necessary.	*)

    VAR dummy: CHAR;

    BEGIN
	IF NOT TraceWindowOpen THEN
	    OpenSimpleWindow (TraceWindow, 22, 24, 0, 28);
	END (*IF*);
	WriteLn (TraceWindow);
	WriteString (TraceWindow, "Press any key to continue.");
	ReadChar (TraceWindow, dummy);
	IF TraceWindowOpen THEN    (* i.e. not just the temporary window *)
	    EraseLine (TraceWindow, 0);  CursorUp(TraceWindow);
	ELSE
	    CloseWindow (TraceWindow);
	END (*IF*);
    END Pause;

(************************************************************************)

PROCEDURE NYI (name: ARRAY OF CHAR);

    (* Types a "not yet implemented" message.  A trace window is opened	*)
    (* if necessary.							*)

    BEGIN
	IF NOT TraceWindowOpen THEN
	    OpenSimpleWindow (TraceWindow, 21, 24, 1, 45);
	    TraceWindowOpen := TRUE;
	END (*IF*);
	WriteLn (TraceWindow);
	WriteString (TraceWindow, name);
	WriteString (TraceWindow, " is not yet implemented.");
    END NYI;

(************************************************************************)

PROCEDURE DeliberatePause;

    (* Inserts a delay in execution, for situations where the trace	*)
    (* messages would otherwise flash by on the screen too quickly	*)
    (* to read.								*)

    VAR j,k: CARDINAL;

    BEGIN
	FOR j := 1 TO PauseLength DO
	    FOR k := 0 TO 1000 DO
		(*NOTHING*)
	    END(*FOR*);
	END (*FOR*);
    END DeliberatePause;

(************************************************************************)

PROCEDURE InTrace (name: ARRAY OF CHAR);

    (* To be called when entering a procedure.	*)

    VAR j: CARDINAL;

    BEGIN
	IF TraceEnabled THEN
	    WriteLn (TraceWindow);
	    FOR j := 1 TO nesting DO
		WriteString (TraceWindow, "   ");
	    END (*FOR*);
	    WriteString (TraceWindow, "Entering ");
	    WriteString (TraceWindow, name);
	    DeliberatePause;
	END (*IF*);
	INC (nesting);
    END InTrace;

(************************************************************************)

PROCEDURE OutTrace (name: ARRAY OF CHAR);

    (* To be called when leaving a procedure.	*)

    VAR j: CARDINAL;

    BEGIN
	DEC (nesting);
	IF TraceEnabled THEN
	    WriteLn (TraceWindow);
	    FOR j := 1 TO nesting DO
		WriteString (TraceWindow, "   ");
	    END (*FOR*);
	    WriteString (TraceWindow, "Leaving ");
	    WriteString (TraceWindow, name);
	    DeliberatePause;
	END (*IF*);
    END OutTrace;

(************************************************************************)

PROCEDURE TraceOn (firstrow, lastrow: RowRange;
			firstcol, lastcol: ColumnRange;
			SlowDownFactor: CARDINAL);

    (* Turns tracing on.		*)

    BEGIN
	IF TraceWindowOpen THEN

	    (* Close any previous trace window. *)

	    CloseWindow (TraceWindow);

	END (*IF*);
	PauseLength := SlowDownFactor;
	OpenSimpleWindow (TraceWindow, firstrow, lastrow, firstcol, lastcol);
	TraceWindowOpen := TRUE;  TraceEnabled := TRUE;
    END TraceOn;

(************************************************************************)

PROCEDURE TraceOff;

    (* Turns tracing off.		*)

    BEGIN
	IF TraceEnabled THEN
	    CloseWindow (TraceWindow);  TraceWindowOpen := FALSE;
	END (*IF*);
	TraceEnabled := FALSE;
    END TraceOff;

(************************************************************************)

PROCEDURE TraceStatus (): BOOLEAN;

    (* Says whether tracing is currently on.		*)

    BEGIN
	RETURN TraceEnabled;
    END TraceStatus;

(************************************************************************)
(*			    INITIALISATION				*)
(************************************************************************)

BEGIN
    TraceEnabled := FALSE;  nesting := 0;  TraceWindowOpen := FALSE;
END MiniTrace.
