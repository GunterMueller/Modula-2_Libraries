IMPLEMENTATION MODULE RealArithmetic;

	(********************************************************)
	(*							*)
	(*	Procedures to perform addition, subtraction,	*)
	(*		etc., on real numbers.			*)
	(*							*)
	(*	This module was originally introduced to	*)
	(*	get around a bug in FTL real arithmetic:	*)
	(*	the lack of re-entrancy.  The present version	*)
	(*	is for TopSpeed Modula-2, which doesn't have	*)
	(*	that bug but which has a fault in TRUNC.	*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	10 October 1992			*)
	(*  Status:		Working				*)
	(*							*)
	(********************************************************)

FROM Semaphores IMPORT
    (* type *)	Semaphore,
    (* proc *)	CreateSemaphore, Wait, Signal;

VAR mutex: Semaphore;

(************************************************************************)

PROCEDURE Add (VAR (*INOUT*) x: LONGREAL;  y: LONGREAL);

    (* Computes  x := x+y	*)

    BEGIN
	x := x + y;
    END Add;

(************************************************************************)

PROCEDURE Subtract (VAR (*INOUT*) x: LONGREAL;  y: LONGREAL);

    (* Computes  x := x-y	*)

    BEGIN
	x := x - y;
    END Subtract;

(************************************************************************)

PROCEDURE Multiply (VAR (*INOUT*) x: LONGREAL;  y: LONGREAL);

    (* Computes  x := x*y	*)

    BEGIN
	x := x*y;
    END Multiply;

(************************************************************************)

PROCEDURE Divide (VAR (*INOUT*) x: LONGREAL;  y: LONGREAL);

    (* Computes  x := x/y	*)

    BEGIN
	x := x/y;
    END Divide;

(************************************************************************)

PROCEDURE Trunc (x: LONGREAL): CARDINAL;

    (* Returns the integer part of a nonnegative real number.	*)

    VAR result: CARDINAL;

    BEGIN
	result := TRUNC(x);
	IF LONGREAL(result) > x THEN DEC(result) END(*IF*);
	RETURN result;
    END Trunc;

(************************************************************************)

PROCEDURE LongTrunc (x: LONGREAL): LONGCARD;

    (* Like Trunc, but returns a long answer.	*)
    (* N.B. This function needs more work, at present it's a very	*)
    (* crude and inefficient implementation.				*)

    VAR result: LONGCARD;
	
    BEGIN
	result := LONGCARD(x);
	IF LONGREAL(result) > x THEN DEC(result) END(*IF*);
	RETURN result;
    END LongTrunc;

(************************************************************************)

PROCEDURE Float (j: LONGCARD): LONGREAL;

    (* Converts cardinal to real.	*)

    VAR result: LONGREAL;

    BEGIN
	RETURN LONGREAL(j);
    END Float;

(************************************************************************)

PROCEDURE Negate (VAR (*INOUT*) x: LONGREAL);

    (* Computes  x := -x	*)

    BEGIN
	x := -x;
    END Negate;

(************************************************************************)

PROCEDURE Square (a: LONGREAL): LONGREAL;

    (* Returns  a*a	*)

    VAR result: LONGREAL;

    BEGIN
	RETURN a*a;
    END Square;

(************************************************************************)

PROCEDURE Equal (x, y: LONGREAL): BOOLEAN;

    (* Tests the condition  x = y	*)

    VAR result: BOOLEAN;

    BEGIN
	RETURN x = y;
    END Equal;

(************************************************************************)

PROCEDURE Greater (x, y: LONGREAL): BOOLEAN;

    (* Tests the condition  x > y	*)

    VAR result: BOOLEAN;

    BEGIN
	RETURN x > y;
    END Greater;

(************************************************************************)

PROCEDURE GreaterOrEqual (x, y: LONGREAL): BOOLEAN;

    (* Tests the condition  x >= y	*)

    VAR result: BOOLEAN;

    BEGIN
	RETURN x >= y;
    END GreaterOrEqual;

(************************************************************************)

PROCEDURE LessThan (x, y: LONGREAL): BOOLEAN;

    (* Tests the condition  x < y	*)

    VAR result: BOOLEAN;

    BEGIN
	RETURN x < y;
    END LessThan;

(************************************************************************)
(*		    PROTECTION OF A GROUP OF OPERATIONS			*)
(************************************************************************)

PROCEDURE ProtectReal;

    BEGIN
	Wait (mutex);
    END ProtectReal;

(************************************************************************)

PROCEDURE UnProtectReal;

    BEGIN
	Signal (mutex);
    END UnProtectReal;

(************************************************************************)

BEGIN
    CreateSemaphore (mutex, 1);
END RealArithmetic.
