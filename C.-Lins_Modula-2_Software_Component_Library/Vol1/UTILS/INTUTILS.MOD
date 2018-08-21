IMPLEMENTATION MODULE IntUtils; 
(*==========================================================
    Version  : 1.0  Sat, Mar 4, 1989  C. Lins
    Compiler : JPI TopSpeed Modula-2
    Component: Primitive Utility - Integer Utilities
    Code Size: R-  bytes

    Revision History
    v1.0  Sat, Mar 4, 1989  C. Lins
      Initial JPI implementation.

    Introduction
    This module provides a collection of utilities for the Modula-2 numeric
    types INTEGER and LONGINT.

    Implementation Notes
    * The standard procedure HIGH, as defined in [1], should return a result
      of type CARDINAL. In [3], INTEGER is used instead. Since these two types
      are incompatible within the context of a FOR loop control index, the
      index has been defined as INTEGER, even though the minimum value is 0.

    Proprietary Notices
      Copyright (C) 1989 Charles A. Lins. All rights reserved. 
==========================================================*)

PROCEDURE Min	(    left  : LONGINT  (*--in   *);
		     right : LONGINT  (*--in   *))
			   : LONGINT  (*--out  *);
BEGIN
  IF left < right THEN
    RETURN left;
  END(*--if*);
  RETURN right;
END Min;
(*-------------------------*)

PROCEDURE Max	(    left  : LONGINT  (*--in   *);
		     right : LONGINT  (*--in   *))
			   : LONGINT  (*--out  *);
BEGIN
  IF left > right THEN
    RETURN left;
  END(*--if*);
  RETURN right;
END Max;
(*-------------------------*)

PROCEDURE RoundUp   (	 theNumber  : LONGINT (*--in   *);
			 theModulus : INTEGER (*--in   *))
				    : LONGINT (*--out  *);
BEGIN
  RETURN ((theNumber + VAL(LONGINT, theModulus) - 1)
         DIV VAL(LONGINT, theModulus)) * VAL(LONGINT, theModulus);
END RoundUp;
(*-------------------------*)


PROCEDURE MinOf (    theNumbers : ARRAY OF INTEGER (*--in   *))
				: INTEGER	   (*--out  *);

VAR   smallest: INTEGER;
      index   : INTEGER;

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

PROCEDURE MaxOf (    theNumbers : ARRAY OF INTEGER (*--in   *))
				: INTEGER	   (*--out  *);

VAR   largest: INTEGER;
      index  : INTEGER;

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


PROCEDURE MinOfLong (	 theNumbers : ARRAY OF LONGINT (*--in	*))
				    : LONGINT	       (*--out	*);

VAR   smallest: LONGINT;
      index   : INTEGER;

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

PROCEDURE MaxOfLong (	 theNumbers : ARRAY OF LONGINT (*--in	*))
				    : LONGINT	       (*--out	*);

VAR   largest: LONGINT;
      index  : INTEGER;

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

END IntUtils.
