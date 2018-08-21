IMPLEMENTATION MODULE ItemConversions;
(*====================================================================
    Version  : 1.0  Sat, Mar 4, 1989  C. Lins
    Compiler : JPI TopSpeed Modula-2
    Code Size: R-  bytes
    Component: Item Conversions Utility

    INTRODUCTION
    This module provides conversion operations between generic items
    and the basic data types.

    REVISION HISTORY
    v1.0  Sat, Mar 4, 1989  C. Lins
      Initial JPI implementation.

    PROPRIETARY NOTICES
    ItemConversions.MOD, by C. Lins, Copyright (C) 1989 Charles A. Lins.
=====================================================================*)

FROM SYSTEM IMPORT
    (*--Type*) ADDRESS;

FROM Items IMPORT
    (*--Type*) Item;

    (*--------------------*)

PROCEDURE AddrToItem (	  x : ADDRESS  (*-- in	  *))
			    : Item     (*-- out   *);
BEGIN
  RETURN Item(x);
END AddrToItem;
(*--------------------*)

PROCEDURE CardToItem (	  x : CARDINAL (*-- in	  *))
			    : Item     (*-- out   *);
BEGIN
  RETURN Item(x);
END CardToItem;
(*--------------------*)

PROCEDURE CharToItem (	  x : CHAR     (*-- in	  *))
			    : Item     (*-- out   *);
BEGIN
  RETURN Item(x);
END CharToItem;
(*--------------------*)

PROCEDURE IntToItem  (	  x : INTEGER (*-- in	 *))
			    : Item    (*-- out	 *);
BEGIN
  RETURN Item(x);
END IntToItem;
(*--------------------*)

PROCEDURE RealToItem (	  x : REAL     (*-- in	  *))
			    : Item     (*-- out   *);
BEGIN
  RETURN Item(x);
END RealToItem;
(*--------------------*)


(*--
    Item -> Basic Type Conversions
--*)

PROCEDURE ItemToAddr (	  x : Item	(*-- in    *))
			    : ADDRESS	(*-- out   *);
BEGIN
  RETURN ADDRESS(x);
END ItemToAddr;
(*--------------------*)

PROCEDURE ItemToCard (	  x : Item	(*-- in    *))
			    : CARDINAL	(*-- out   *);
BEGIN
  RETURN CARDINAL(x);
END ItemToCard;
(*--------------------*)

PROCEDURE ItemToChar (	  x : Item	(*-- in    *))
			    : CHAR	(*-- out   *);
BEGIN
  RETURN CHAR(x);
END ItemToChar;
(*--------------------*)

PROCEDURE ItemToInt  (	  x : Item	(*-- in    *))
			    : INTEGER	(*-- out   *);
BEGIN
  RETURN INTEGER(x);
END ItemToInt;
(*--------------------*)

PROCEDURE ItemToReal (	  x : Item	(*-- in    *))
			    : REAL	(*-- out   *);
BEGIN
  RETURN REAL(x);
END ItemToReal;
(*--------------------*)

END ItemConversions.
