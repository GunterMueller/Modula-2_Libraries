IMPLEMENTATION MODULE CardUtils;
(*==========================================================
    Version  : 1.0  Sat, Mar 4, 1989  C. Lins
    Compiler : JPI TopSpeed Modula-2
    Component: Primitive Utility - Cardinal Utilities
    Code Size: R-  bytes

    Revision History
    v1.0  Sat, Mar 4, 1989  C. Lins
      Initial JPI implementation.

    Introduction
    This module provides a collection of utilities for the
    Modula-2 numeric types CARDINAL and LONGCARD.

    Implementation Notes
    * The standard procedure HIGH, as defined in [1], should return a result
      of type CARDINAL. In [3], INTEGER is used instead. Since these two types
      are incompatible within the context of a FOR loop control index, the
      index has been defined as INTEGER, even though the minimum value is 0.

    Proprietary Notices
    CardUtils.MOD, Copyright (C) 1989 Charles A. Lins.
==========================================================*)

PROCEDURE Min	(    left  : LONGCARD  (*--in	*);
		     right : LONGCARD  (*--in	*))
			   : LONGCARD  (*--out	*);
BEGIN
  IF left < right THEN
    RETURN left;
  END(*--if*);
  RETURN right;
END Min;
(*-------------------------*)

PROCEDURE Max	(    left  : LONGCARD  (*--in	*);
		     right : LONGCARD  (*--in	*))
			   : LONGCARD  (*--out	*);
BEGIN
  IF left > right THEN
    RETURN left;
  END(*--if*);
  RETURN right;
END Max;
(*-------------------------*)

PROCEDURE MinOf (    theNumbers : ARRAY OF CARDINAL (*--in   *))
				: CARDINAL	    (*--out  *);

VAR   smallest: CARDINAL; (*-- current smallest value in theNumbers *)
      index   : INTEGER;  (*-- loop index over theNumbers *)

BEGIN
  smallest := theNumbers[0];
  FOR index := 1 TO HIGH(theNumbers) DO
    IF theNumbers[index] < smallest THEN
      smallest := theNumbers[index];
    END (*--if*);
  END (*--for*);
  RETURN smallest;
END MinOf;
(*-------------------------*)

PROCEDURE MaxOf (    theNumbers : ARRAY OF CARDINAL (*--in   *))
				: CARDINAL	    (*--out  *);

VAR   largest: CARDINAL; (*-- current largest value in theNumbers *)
      index  : INTEGER;  (*-- loop index over theNumbers *)

BEGIN
  largest := theNumbers[0];
  FOR index := 1 TO HIGH(theNumbers) DO
    IF theNumbers[index] > largest THEN
      largest := theNumbers[index];
    END (*--if*);
  END (*--for*);
  RETURN largest;
END MaxOf;
(*-------------------------*)

PROCEDURE MinOfLong (	 theNumbers : ARRAY OF LONGCARD (*--in	 *))
				    : LONGCARD		(*--out  *);

VAR   smallest: LONGCARD; (*-- current smallest value in theNumbers *)
      index   : INTEGER;  (*-- loop index over theNumbers *)

BEGIN
  smallest := theNumbers[0];
  FOR index := 1 TO HIGH(theNumbers) DO
    IF theNumbers[index] < smallest THEN
      smallest := theNumbers[index];
    END (*--if*);
  END (*--for*);
  RETURN smallest;
END MinOfLong;
(*-------------------------*)

PROCEDURE MaxOfLong (	 theNumbers : ARRAY OF LONGCARD (*--in	 *))
				    : LONGCARD		(*--out  *);

VAR   largest: LONGCARD; (*-- current largest value in theNumbers *)
      index  : INTEGER;  (*-- loop index over theNumbers *)

BEGIN
  largest := theNumbers[0];
  FOR index := 1 TO HIGH(theNumbers) DO
    IF theNumbers[index] > largest THEN
      largest := theNumbers[index];
    END (*--if*);
  END (*--for*);
  RETURN largest;
END MaxOfLong;
(*-------------------------*)

END CardUtils.
