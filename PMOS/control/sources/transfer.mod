IMPLEMENTATION MODULE TransferFunctions;

	(********************************************************)
	(*							*)
	(*		Second order transfer functions		*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	21 February 1995		*)
	(*  Status:		Working on screen editing	*)
	(*							*)
	(********************************************************)

FROM Trace IMPORT
    (* proc *)	NYI, Pause, InTrace, OutTrace;

FROM Storage1 IMPORT
    (* proc *)	ALLOCATE, DEALLOCATE;

FROM Windows IMPORT
    (* type *)	Window, Colour, FrameType, DividerType,
    (* proc *)	OpenWindow, CloseWindow, WriteChar, WriteString,
		SetCursor, SaveCursor, CursorUp, CursorDown;

FROM RealIO IMPORT
    (* proc *)	WriteReal;

FROM Menus IMPORT
    (* type *)	Menu, ItemText,
    (* proc *)	CreateMenu, PositionMenu, DestroyMenu, SelectFromMenu;

FROM ScreenEditor IMPORT
    (* type *)	Structure,
    (* proc *)	MenuField, RealField, Combine, ScreenEdit, DeleteStructure;

FROM TaskControl IMPORT
    (* type *)	Lock,
    (* proc *)	CreateLock, DestroyLock, Obtain, Release;

(************************************************************************)

TYPE
    power = [0..2];

    (* We represent a second order polynomial by its coefficients, in	*)
    (* ascending power order.						*)

    Poly = ARRAY power OF REAL;

    (* A rational function is the ratio of two polynomials.	*)

    Rational =	RECORD
		    numerator, denominator: Poly
		END (*RECORD*);

    (* A transfer function has a state, and we represent it by two	*)
    (* rational functions: a Laplace function form (SForm) and a Delta	*)
    (* form (DForm).  This is redundant, since SForm and DForm are	*)
    (* related by a bilinear transformation, but it tends to speed up	*)
    (* the computations to have both forms available at all times.	*)
    (* Remark: State[2] is a dummy which is never used.			*)

    TransferFunction = POINTER TO
			RECORD
			    access: Lock;
			    delta: REAL;
			    State: Poly;
			    SForm, DForm: Rational;
			END (*RECORD*);

(************************************************************************)
(*			OPERATIONS ON POLYNOMIALS			*)
(************************************************************************)

PROCEDURE Fill (VAR (*OUT*) p: Poly;  value: REAL);

    (* Fills all coefficients to the same given value. *)

    VAR j: power;

    BEGIN
	FOR j := MIN(power) TO MAX(power) DO
	    p[j] := value;
	END (*FOR*);
    END Fill;

(************************************************************************)

PROCEDURE StoDelta (P: Poly;  interval: REAL): Poly;

    (* Converts an s-form polynomial to delta form. *)

    VAR result: Poly;

    BEGIN
	InTrace ("StoDelta");
	result[0] := P[0];
	result[1] := P[1] + 2.0*interval*P[0];
	result[2] := P[2] + interval * (P[1] + interval*P[0]);
	OutTrace ("StoDelta");
	RETURN result;
    END StoDelta;

(************************************************************************)

PROCEDURE DisplayPoly (w: Window;  P: Poly);

    (* Writes a polynomial to the screen, starting at the current	*)
    (* screen location.							*)

    BEGIN
	InTrace ("DisplayPoly");
	WriteReal (w, P[0], 6);  WriteString (w, " +   ");
	WriteReal (w, P[1], 6);  WriteString (w, " s + ");
	WriteReal (w, P[2], 6);  WriteString (w, " s");
	CursorUp (w);  WriteChar (w, "2");
	OutTrace ("DisplayPoly");
    END DisplayPoly;

(************************************************************************)
(*		     OPERATIONS ON RATIONAL FUNCTIONS			*)
(************************************************************************)

PROCEDURE DisplayRational (w: Window;  R: Rational);

    (* Writes a transfer function to the screen, starting at the	*)
    (* current screen location.						*)

    VAR row, col: CARDINAL;

    BEGIN
	InTrace ("DisplayRational");
	SaveCursor (w, row, col);
	DisplayPoly (w, R.numerator);
	SetCursor (w, row+1, col);
	WriteString (w, "_______________________________");
	SetCursor (w, row+3, col);
	DisplayPoly (w, R.denominator);
	OutTrace ("DisplayRational");
    END DisplayRational;

(************************************************************************)

PROCEDURE DiscreteVersion (SForm: Rational;  delta: REAL): Rational;

    (* Computes the delta-transform equivalent of an s-domain function.	*)

    VAR result: Rational;

    BEGIN
	InTrace ("DiscreteVersion");
	result.numerator := StoDelta (SForm.numerator, delta);
	result.denominator := StoDelta (SForm.denominator, delta);
	OutTrace ("DiscreteVersion");
	RETURN result;
    END DiscreteVersion;

(************************************************************************)

PROCEDURE SetToUnity (VAR (*INOUT*) TF: Rational);

    (* Sets the value of TF to unity. *)

    BEGIN
	WITH TF DO
	    Fill (numerator, 1.0);
	    denominator := numerator;
	END (*WITH*);
    END SetToUnity;

(************************************************************************)
(*		     OPERATIONS ON TRANSFER FUNCTIONS			*)
(************************************************************************)

PROCEDURE CreateTransferFunction (VAR (*OUT*) G: TransferFunction;
					SamplingInterval: REAL);

    (* Creates a new transfer function, with value initially set to unity. *)

    BEGIN
	NEW (G);
	WITH G^ DO
	    CreateLock (access);
	    delta := SamplingInterval;
	    Fill (State, 0.0);
	    SetToUnity (SForm);
	    SetToUnity (DForm);
	END (*WITH*);
    END CreateTransferFunction;

(************************************************************************)

PROCEDURE DestroyTransferFunction (VAR (*INOUT*) G: TransferFunction);

    (* Discards a transfer function. *)

    BEGIN
	Obtain (G^.access);  DestroyLock (G^.access);
	DISPOSE (G);
    END DestroyTransferFunction;

(************************************************************************)

PROCEDURE UpdateSamplingInterval (VAR (*INOUT*) G: TransferFunction;
					SamplingInterval: REAL);

    (* Recomputes the internal details of the transfer function to	*)
    (* allow for a change in sampling interval.				*)

    BEGIN
	WITH G^ DO
	    Obtain (access);
	    delta := SamplingInterval;
	    DForm := DiscreteVersion (SForm, delta);
	    Release (access);
	END (*WITH*);
    END UpdateSamplingInterval;

(************************************************************************)

PROCEDURE Filter (VAR (*INOUT*) G: TransferFunction;  input: REAL): REAL;

    (* Computes the output from G with the given input.  Remark: G is	*)
    (* an inout parameter because its internal state is affected.	*)

    VAR temp, result: REAL;

    BEGIN
	WITH G^ DO
	    Obtain (access);
	    WITH DForm DO
		temp := State[0] + delta * (input - denominator[1]*State[0]
						- denominator[0]*State[1]);
		State[1] := State[1] + delta*State[0];
		State[0] := temp;
		result :=
		    (numerator[1] - denominator[1]*numerator[2]) * State[0]
		    + (numerator[0] - denominator[0]*numerator[2]) * State[1]
		    + numerator[2]*input;
	    END (*WITH*);
	    Release (access);
	END (*WITH*);
	RETURN result;
    END Filter;

(************************************************************************)

PROCEDURE EditTransferFunction (VAR (*INOUT*) G: TransferFunction;
						caption: ARRAY OF CHAR);

    (* Allows the keyboard user to alter a transfer function.	*)

    TYPE optionrange = [0..4];

    VAR TF, oldTF: Rational;  w: Window;
	j: power;
	S: Structure;  M: Menu;
	Messages: ARRAY optionrange OF ItemText;
	option: CARDINAL;  abort: BOOLEAN;

    BEGIN
	InTrace ("EditTransferFunction");

	OpenWindow (w, white, magenta, 14, 24, 10, 69, simpleframe, nodivider);
	WriteString (w, caption);

	Messages[0] := "Transfer function";
	Messages[1] := "Accept";
	Messages[2] := "Set to unity";
	Messages[3] := "Cancel changes";
	Messages[4] := "Edit";
	CreateMenu (M, 1, Messages, MAX(optionrange));
	PositionMenu (M, 2, 5+MAX(optionrange), 50, 75);

	(* Note that all the editing is done on a copy, with the actual	*)
	(* transfer function updated only after the user is satisfied	*)
	(* with the changes.						*)

	WITH G^ DO
	    Obtain (access);  TF := SForm;  Release (access);
	END (*WITH*);

	(* Create the editing structure used by ScreenEditor.	*)

	WITH TF DO
	    S := RealField (numerator[0], 4, 2, 6);
	    Combine (S, RealField (denominator[0], 7, 2, 6));
	    FOR j := 1 TO 2 DO
		Combine (S, RealField (numerator[j], 4, 2+11*j, 6));
		Combine (S, RealField (denominator[j], 7, 2+11*j, 6));
	    END (*FOR*);
	END (*WITH*);

	REPEAT
	    SetCursor (w, 4, 2);
	    DisplayRational (w, TF);
	    option := SelectFromMenu (M);

	    IF option = 2 THEN		(* Set to unity *)
		SetToUnity (TF);
	    ELSIF option = 3 THEN	(* Cancel changes *)
		WITH G^ DO
		    Obtain (access);  TF := SForm;  Release (access);
		END (*WITH*);
	    ELSIF option = 4 THEN	(* Edit *)
		oldTF := TF;
		ScreenEdit (w, S, abort);
		IF abort THEN
		    TF := oldTF;
		END (*IF*);
	    END (*IF*);


	UNTIL option < 2;

	IF option = 1 THEN		(* Accept *)
	    WITH G^ DO
		Obtain (access);
		SForm := TF;
		DForm := DiscreteVersion (TF, delta);
		Release (access);
	    END (*WITH*);
	END (*IF*);

	DestroyMenu (M);  DeleteStructure (S);
	CloseWindow (w);
	OutTrace ("EditTransferFunction");
    END EditTransferFunction;

(************************************************************************)

END TransferFunctions.
