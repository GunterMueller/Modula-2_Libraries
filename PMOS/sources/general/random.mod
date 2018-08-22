IMPLEMENTATION MODULE Random;

	(********************************************************)
	(*							*)
	(*		Random number generator			*)
	(*							*)
	(*  Programmer:		P. Moylan			*)
	(*  Last edited:	12 February 1993		*)
	(*  Status:		OK				*)
	(*							*)
	(********************************************************)

FROM RandCard IMPORT
    (* const*)	modulus,
    (* var  *)	seed,
    (* proc *)	RandCardinal;

VAR scale: REAL;

(************************************************************************)

PROCEDURE RANDOM(): REAL;

    (* Returns a random number from a uniform (0.0, 1.0) distribution.	*)
    (* This version relies on procedure RandCardinal to do the random	*)
    (* number generation; all that we do is the scaling.		*)

    BEGIN
	RETURN scale*FLOAT(RandCardinal());
    END RANDOM;

(************************************************************************)

PROCEDURE Randomize (newseed: LONGCARD);

    (* Resets the seed of the random number generator.	*)

    BEGIN
	IF newseed = 0 THEN newseed := 1
	ELSIF newseed >= modulus THEN newseed := modulus-1
	END (*IF*);
	seed := newseed;
    END Randomize;

(************************************************************************)

BEGIN

    (* The construct FLOAT(LONGCARD(modulus)) below looks redundant,	*)
    (* but it's there because a plain FLOAT(modulus) causes strange	*)
    (* things to happen.  Compiler bug?					*)

    scale := 1.0/FLOAT(LONGCARD(modulus));

END Random.
