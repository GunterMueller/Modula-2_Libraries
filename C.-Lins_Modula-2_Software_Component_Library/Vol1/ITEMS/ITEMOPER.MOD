IMPLEMENTATION MODULE ItemOperations;
(*====================================================================
    Version  : 1.0  Sat, Mar 4, 1989  C. Lins
    Compiler : JPI TopSpeed Modula-2
    Code Size: R-  bytes
    Component: Structure Utility - Generic Item Operations

    INTRODUCTION
    This module provides predefined routines for the standard data
    types, (CHAR, INTEGER, etc.), compatible with the procedure types
    exported by the “Items” module.

    REVISION HISTORY
    v1.0  Sat, Mar 4, 1989  C. Lins
      Initial JPI implementation.

    Proprietary Notices
      Copyright (C) 1989 Charles A. Lins. All rights reserved.
====================================================================*)

FROM Items IMPORT
    (*--Type*) Item;

FROM Relations IMPORT
    (*--Type*) Relation;

    (*-----------------------*)

PROCEDURE AssignItem   (    source : Item     (*-- in	 *))
				   : Item     (*-- out	 *);
BEGIN
  RETURN source;
END AssignItem;
(*----------------------------*)

PROCEDURE EqualItems   (    left   : Item     (*-- in	 *);
			    right  : Item     (*-- in	 *))
				   : BOOLEAN  (*-- out	 *);
BEGIN
  RETURN left = right;
END EqualItems;
(*----------------------------*)

PROCEDURE CharCompare	   (	left : Item	(*-- in    *);
				right: Item	(*-- in    *))
				     : Relation (*-- out   *);

VAR   leftChar	: CHAR;
      rightChar : CHAR;

BEGIN
  leftChar  := CHAR(left);
  rightChar := CHAR(right);

  IF (leftChar = rightChar) THEN
     RETURN equal;

  ELSIF (leftChar < rightChar) THEN
     RETURN less;

  ELSIF (leftChar > rightChar) THEN
     RETURN greater;
  END (*--if*);

  RETURN incomparable; (*-- We should NEVER get here *)
END CharCompare;
(*----------------------------*)

PROCEDURE IntegerCompare   (	left : Item	(*-- in    *);
				right: Item	(*-- in    *))
				     : Relation (*-- out   *);

VAR   leftInt  : INTEGER;
      rightInt : INTEGER;

BEGIN
  leftInt  := INTEGER(left);
  rightInt := INTEGER(right);

  IF (leftInt = rightInt) THEN
     RETURN equal;

  ELSIF (leftInt < rightInt) THEN
     RETURN less;

  ELSIF (leftInt > rightInt) THEN
     RETURN greater;
  END (*--if*);

  RETURN incomparable; (*-- We should NEVER get here *)
END IntegerCompare;
(*----------------------------*)

PROCEDURE CardinalCompare  (	left : Item	(*-- in    *);
				right: Item	(*-- in    *))
				     : Relation (*-- out   *);

VAR   leftCard	: CARDINAL;
      rightCard : CARDINAL;

BEGIN
  leftCard  := CARDINAL(left);
  rightCard := CARDINAL(right);

  IF (leftCard = rightCard) THEN
     RETURN equal;

  ELSIF (leftCard < rightCard) THEN
     RETURN less;

  ELSIF (leftCard > rightCard) THEN
     RETURN greater;
  END (*--if*);

  RETURN incomparable; (*-- We should NEVER get here *)
END CardinalCompare;
(*----------------------------*)

PROCEDURE LongIntCompare   (	left : Item	(*-- in    *);
				right: Item	(*-- in    *))
				     : Relation (*-- out   *);

VAR   leftLongInt  : LONGINT;
      rightLongInt : LONGINT;

BEGIN
  leftLongInt  := LONGINT(left);
  rightLongInt := LONGINT(right);

  IF (leftLongInt = rightLongInt) THEN
     RETURN equal;

  ELSIF (leftLongInt < rightLongInt) THEN
     RETURN less;

  ELSIF (leftLongInt > rightLongInt) THEN
     RETURN greater;
  END (*--if*);

  RETURN incomparable; (*-- We should NEVER get here *)
END LongIntCompare;
(*----------------------------*)

PROCEDURE LongCardCompare  (	left : Item	(*-- in    *);
				right: Item	(*-- in    *))
				     : Relation (*-- out   *);

VAR   leftLongCard  : LONGCARD;
      rightLongCard : LONGCARD;

BEGIN
  leftLongCard	:= LONGCARD(left);
  rightLongCard := LONGCARD(right);

  IF (leftLongCard = rightLongCard) THEN
     RETURN equal;

  ELSIF (leftLongCard < rightLongCard) THEN
     RETURN less;

  ELSIF (leftLongCard > rightLongCard) THEN
     RETURN greater;
  END (*--if*);

  RETURN incomparable; (*-- We should NEVER get here *)
END LongCardCompare;
(*----------------------------*)

PROCEDURE RealCompare	   (	left : Item	(*-- in    *);
				right: Item	(*-- in    *))
				     : Relation (*-- out   *);

VAR   leftReal	: REAL;
      rightReal : REAL;

BEGIN
  leftReal  := REAL(left);
  rightReal := REAL(right);

  IF (leftReal = rightReal) THEN
     RETURN equal;

  ELSIF (leftReal < rightReal) THEN
     RETURN less;

  ELSIF (leftReal > rightReal) THEN
     RETURN greater;
  END (*--if*);

  RETURN incomparable; (*-- We should NEVER get here *)
END RealCompare;
(*----------------------------*)

END ItemOperations.
