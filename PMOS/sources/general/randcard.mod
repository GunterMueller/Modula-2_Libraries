IMPLEMENTATION MODULE RandCard;

	(********************************************************)
	(*							*)
	(*		Random number generator			*)
	(*							*)
	(*	This version is a more portable (but slightly	*)
	(*	slower) version of the assembly language	*)
	(*	module of the same name.			*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	18 March 1995			*)
	(*  Status:		Working				*)
	(*							*)
	(********************************************************)

(************************************************************************)
(*									*)
(* The algorithm used is Schrage's method, as described in		*)
(*	Stephen K. Park and Keith W. Miller, "Random Number Generators:	*)
(*	Good ones are hard to find", CACM 31(10), Oct 1988, 1192-1201.	*)
(* A basic property of this particular implementation is that all	*)
(* intermediate results fit into 32 bits (including sign).		*)
(*									*)
(************************************************************************)

FROM LowLevel IMPORT
    (* proc *)	HighWord, MakeLongword, Mul;

(************************************************************************)

CONST
    a = 16807;			(* 7^5 *)
    divisor = modulus DIV a;	(* = 127773 *)
    divisorL = divisor - 65536;	(* low order 16 bits of divisor *)

(************************************************************************)

PROCEDURE Divide (number: LONGCARD;
			VAR (*OUT*) quotient: CARDINAL;
			VAR (*OUT*) remainder: LONGCARD);

    (* Special-purpose division procedure - be careful about using	*)
    (* this in other applications, because it takes advantage of some	*)
    (* foreknowledge of the numbers which arise in this application.	*)
    (* Divides number by divisor=127773.				*)

    VAR step: CARDINAL;

    BEGIN
	remainder := number;
	quotient := 0;
	WHILE remainder >= divisor DO
	    step := ORD(HighWord(remainder)) DIV 2;
	    IF step = 0 THEN step := 1 END(*IF*);
	    INC (quotient, step);

	    (* Here, we take advantage of the fact that	*)
	    (*	divisor := 2^16 + divisorL		*)

	    remainder := remainder - MakeLongword(step, 0)
					- Mul(divisorL,step);
	END (*WHILE*);
    END Divide;

(************************************************************************)

PROCEDURE RandCardinal (): LONGCARD;

    (* Let a = 7^5 = 16807, q = modulus DIV a = 127773, and		*)
    (* r = modulus MOD a = 2836.					*)
    (* We perform the sequence of calculations:				*)
    (*		high := SHORT(seed DIV q);  low := seed MOD q;		*)
    (* 		test := a*low - r*LONG(high);				*)
    (*		IF test > 0 THEN seed := test				*)
    (*		ELSE seed := test + modulus;				*)
    (*		ENDIF;							*)
    (*		RETURN seed;						*)
    (* The division in the first step is complicated by the fact that	*)
    (* our processor does not provide a 32-bit division operation.	*)

    CONST r = modulus MOD a;	(* 2836 *)

    VAR high: CARDINAL;  low: LONGCARD;  test: LONGINT;

    BEGIN
	Divide (seed, high, low);
	test := VAL(LONGINT,a*low) - VAL(LONGINT,Mul(r, high));
	IF test > 0 THEN seed := test
	ELSE seed := test + modulus;
	END (*IF*);
	RETURN seed;
    END RandCardinal;

(****************************************************************)
(*			MODULE INITIALISATION			*)
(****************************************************************)

BEGIN
    seed := 1;
END RandCard.
