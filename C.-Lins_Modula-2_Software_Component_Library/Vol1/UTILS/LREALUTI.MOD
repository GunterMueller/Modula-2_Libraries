IMPLEMENTATION MODULE LRealUtils;
(*==========================================================
    Version  : 1.0  Sat, Mar 4, 1989  C. Lins
    Compiler : JPI TopSpeed Modula-2
    Component: Primitive Utility - LongReal Utilities
    Code Size: R-  bytes

    Revision History
    v1.0  Sat, Mar 4, 1989  C. Lins
      Initial JPI implementation.

    Introduction
    This module provides a collection of utilities for the
    Modula-2 numeric type LONGREAL.

    Implementation Notes
    * The standard procedure HIGH, as defined in [1], should return a result
      of type CARDINAL. In [3], INTEGER is used instead. Since these two types
      are incompatible within the context of a FOR loop control index, the
      index has been defined as INTEGER, even though the minimum value is 0.

    Proprietary Notices
    LongRealUtils.MOD, Copyright (C) 1989 Charles A. Lins.
==========================================================*)

PROCEDURE Min	(    left  : LONGREAL  (*--in	*);
		     right : LONGREAL  (*--in	*))
			   : LONGREAL  (*--out	*);
BEGIN
  IF left < right THEN
    RETURN left;
  END(*--if*);
  RETURN right;
END Min;
(*-------------------------*)

PROCEDURE Max	(    left  : LONGREAL  (*--in	*);
		     right : LONGREAL  (*--in	*))
			   : LONGREAL  (*--out	*);
BEGIN
  IF left > right THEN
    RETURN left;
  END(*--if*);
  RETURN right;
END Max;
(*-------------------------*)

PROCEDURE MinOf (    theNumbers : ARRAY OF LONGREAL (*--in   *))
				: LONGREAL	    (*--out  *);

VAR   smallest: LONGREAL; (*-- current smallest value in theNumbers *)
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

PROCEDURE MaxOf (    theNumbers : ARRAY OF LONGREAL (*--in   *))
				: LONGREAL	    (*--out  *);

VAR   largest: LONGREAL; (*-- current largest value in theNumbers *)
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

END LRealUtils.
