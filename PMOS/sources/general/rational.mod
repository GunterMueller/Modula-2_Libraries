IMPLEMENTATION MODULE Rationals;

	(********************************************************)
	(*							*)
	(*		Arithmetic on rational numbers		*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	2 May 1994			*)
	(*  Status:		OK				*)
	(*							*)
	(********************************************************)


PROCEDURE gcd (x, y: CARDINAL): CARDINAL;

    (* Returns the greatest common divisor of x and y. *)

    VAR temp: CARDINAL;

    BEGIN
	IF x < y THEN
	    temp := x;  x := y;  y := temp;
	END (*IF*);
	WHILE y <> 0 DO
	    temp := x MOD y;  x := y;  y := temp;
	END (*WHILE*);
	RETURN x;
    END gcd;

(****************************************************************)

PROCEDURE Reduce (VAR (*INOUT*) x: Rational);

    (* Removes common factors between the numerator and the denominator. *)

    VAR top, factor: CARDINAL;

    BEGIN
	top := ABS(x.num);
	factor := gcd (top, x.denom);
	x.num := x.num DIV INTEGER(factor);
	x.denom := x.denom DIV factor;
    END Reduce;

(****************************************************************)

PROCEDURE Zero (): Rational;

    (* Returns a representation of zero. *)

    VAR result: Rational;

    BEGIN
	result.num := 0;  result.denom := 1;
	RETURN result;
    END Zero;

(****************************************************************)

PROCEDURE Unity (): Rational;

    (* Returns a representation of the number 1. *)

    VAR result: Rational;

    BEGIN
	result.num := 1;  result.denom := 1;
	RETURN result;
    END Unity;

(****************************************************************)

PROCEDURE Add (x, y: Rational): Rational;

    (* Returns x+y. *)

    VAR result: Rational;

    BEGIN
	result.num := x.num*INTEGER(y.denom) + y.num*INTEGER(x.denom);
	result.denom := x.denom * y.denom;
	Reduce (result);
	RETURN result;
    END Add;

(****************************************************************)

PROCEDURE Subtract (x, y: Rational): Rational;

    (* Returns x-y. *)

    VAR result: Rational;

    BEGIN
	result.num := x.num*INTEGER(y.denom) - y.num*INTEGER(x.denom);
	result.denom := x.denom * y.denom;
	Reduce (result);
	RETURN result;
    END Subtract;

(****************************************************************)

PROCEDURE Compare (x, y: Rational): INTEGER;

    (* Returns 0 if x=y, <0 if x<y, and >0 if x>y. *)

    VAR test: INTEGER;

    BEGIN
	test := x.num*INTEGER(y.denom) - y.num*INTEGER(x.denom);
	IF test > 0 THEN RETURN +1
	ELSIF test = 0 THEN RETURN 0
	ELSE RETURN -1
	END (*IF*);
    END Compare;

(****************************************************************)

PROCEDURE Multiply (x, y: Rational): Rational;

    (* Returns x*y. *)

    VAR result: Rational;  temp: CARDINAL;

    BEGIN
	(* To reduce the chance of overflow, do the reductions	*)
	(* before the multiplication.				*)

	temp := x.denom;  x.denom := y.denom;  y.denom := temp;
	Reduce (x);  Reduce(y);
	result.num := x.num * y.num;
	result.denom := x.denom * y.denom;
	RETURN result;
    END Multiply;

(****************************************************************)

PROCEDURE Divide (x, y: Rational): Rational;

    (* Returns x/y. *)

    BEGIN
	RETURN Multiply (x, Reciprocal(y));
    END Divide;

(****************************************************************)

PROCEDURE Reciprocal (x: Rational): Rational;

    (* Returns 1/x. *)

    VAR result: Rational;

    BEGIN
	IF x.num < 0 THEN
	    result.num := -INTEGER(x.denom);  result.denom := -x.num;
	ELSE
	    result.num := x.denom;  result.denom := x.num;
	END (*IF*);
	RETURN result;
    END Reciprocal;

(****************************************************************)

END Rationals.
