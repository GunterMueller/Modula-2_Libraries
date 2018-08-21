IMPLEMENTATION MODULE TypeManager;
(*====================================================================
    Version  : 1.0  Sat, Mar 4, 1989  C. Lins
    Compiler : JPI TopSpeed Modula-2
    Code Size: R-  bytes
    Component: Tool - Dynamic Type Management Utility

    REVISION HISTORY
    v1.0  Sat, Mar 4, 1989  C. Lins
      Initial JPI implementation.

    Proprietary Notices
      Copyright (C) 1989 Charles A. Lins. All rights reserved.
====================================================================*)

FROM SYSTEM IMPORT
    (*--Proc*) ADR;

FROM JPIStorage IMPORT
    (*--Proc*) Allocate;

FROM Items IMPORT
    (*--Type*) Item, AssignProc, CompareProc, DisposeProc;

FROM ItemOperations IMPORT
    (*--Proc*) AssignItem, CharCompare, IntegerCompare, CardinalCompare,
	       LongIntCompare, LongCardCompare, RealCompare;

    (*-----------------------*)

TYPE  TypeID   = POINTER TO TypeNode;
TYPE  TypeNode = RECORD
	name	: TypeName;    (*-- Name for this type *)
	assign	: AssignProc;  (*-- Assignment routine *)
	compare : CompareProc; (*-- Comparison routine *)
	dispose : DisposeProc; (*-- Item Disposal routine *)
      END (*-- TypeNode *);


    (*-----------------------*)

TYPE  SimpleTypes = (chars, ints, cards, longints, longcards, reals);
VAR   basicTypes  : ARRAY SimpleTypes OF TypeNode;


    (*-----------------------*)

PROCEDURE NullDispose (VAR theItem : Item (*-- inout *));
BEGIN
END NullDispose;
(*----------------------------*)


    (*-----------------------*)
    (*	   CONSTRUCTORS      *)

PROCEDURE Build     (	 theName    : TypeName	  (*-- in    *);
			 assignment : AssignProc  (*-- in    *);
			 comparison : CompareProc (*-- in    *);
			 deallocate : DisposeProc (*-- in    *);
		     VAR theTypeNode: TypeNode	  (*-- out   *));
BEGIN
  WITH theTypeNode DO
    name    := theName;
    assign  := assignment;
    compare := comparison;
    dispose := deallocate;
  END (*--with*);
END Build;
(*----------------------------*)

PROCEDURE Create    (	 theName    : TypeName	  (*-- in    *);
			 assignment : AssignProc  (*-- in    *);
			 comparison : CompareProc (*-- in    *);
			 deallocate : DisposeProc (*-- in    *))
				    : TypeID	  (*-- out   *);

VAR   newType : TypeID;

BEGIN
  Allocate(newType, SIZE(TypeNode));
  IF (newType # NullType) THEN
    Build(theName, assignment, comparison, deallocate, newType^);
  END (*--if*);
  RETURN newType;
END Create;
(*----------------------------*)

PROCEDURE CharTypeID	 () : TypeID   (*-- out   *);
BEGIN
  RETURN ADR(basicTypes[chars]);
END CharTypeID;
(*----------------------------*)

PROCEDURE IntegerTypeID  () : TypeID   (*-- out   *);
BEGIN
  RETURN ADR(basicTypes[ints]);
END IntegerTypeID;
(*----------------------------*)

PROCEDURE CardinalTypeID () : TypeID   (*-- out   *);
BEGIN
  RETURN ADR(basicTypes[cards]);
END CardinalTypeID;
(*----------------------------*)

PROCEDURE LongIntTypeID  () : TypeID   (*-- out   *);
BEGIN
  RETURN ADR(basicTypes[longints]);
END LongIntTypeID;
(*----------------------------*)

PROCEDURE LongCardTypeID () : TypeID   (*-- out   *);
BEGIN
  RETURN ADR(basicTypes[longcards]);
END LongCardTypeID;
(*----------------------------*)

PROCEDURE RealTypeID	 () : TypeID   (*-- out   *);
BEGIN
  RETURN ADR(basicTypes[reals]);
END RealTypeID;
(*----------------------------*)


    (*-----------------------*)
    (*	    SELECTORS	     *)

PROCEDURE NameOf    (	 theType : TypeID      (*-- in	  *);
		     VAR theName : TypeName    (*-- out   *));
BEGIN
  IF (theType = NullType) THEN
    theName := "";
  ELSE
    theName := theType^.name;
  END (*--if*);
END NameOf;
(*----------------------------*)

PROCEDURE AssignOf  (	 theType : TypeID      (*-- in	  *))
				 : AssignProc  (*-- out   *);
BEGIN
  IF (theType = NullType) THEN
    RETURN AssignItem;
  END (*--if*);
  RETURN theType^.assign;
END AssignOf;
(*----------------------------*)

PROCEDURE CompareOf (	 theType : TypeID      (*-- in	  *))
				 : CompareProc (*-- out   *);
BEGIN
  IF (theType = NullType) THEN
    RETURN NoCompareProc;
  END (*--if*);
  RETURN theType^.compare;
END CompareOf;
(*----------------------------*)

PROCEDURE DisposeOf (	 theType : TypeID      (*-- in	  *))
				 : DisposeProc (*-- out   *);
BEGIN
  IF (theType = NullType) THEN
    RETURN NullDispose;
  END (*--if*);
  RETURN theType^.dispose;
END DisposeOf;
(*----------------------------*)

PROCEDURE AttributesOf (    theType    : TypeID      (*-- in	*);
			VAR theName    : TypeName    (*-- out	*);
			VAR assignment : AssignProc  (*-- out	*);
			VAR comparison : CompareProc (*-- out	*);
			VAR deallocate : DisposeProc (*-- out	*));
BEGIN
  IF (theType = NullType) THEN
    theName    := "";
    assignment := AssignItem;
    comparison := NoCompareProc;
    deallocate := NullDispose;
  ELSE
    WITH theType^ DO
      theName	 := name;
      assignment := assign;
      comparison := compare;
      deallocate := dispose;
    END (*--with*);
  END (*--if*);
END AttributesOf;
(*----------------------------*)


(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
(*~~ MODULE INITIALIZATION  ~~*)

BEGIN
  Build("CHAR",     AssignItem,  CharCompare,
		    NullDispose, basicTypes[chars]);
  Build("INTEGER",  AssignItem,  IntegerCompare,
		    NullDispose, basicTypes[ints]);
  Build("CARDINAL", AssignItem,  CardinalCompare,
		    NullDispose, basicTypes[cards]);
  Build("LONGINT",  AssignItem,  LongIntCompare,
		    NullDispose, basicTypes[longints]);
  Build("LONGCARD", AssignItem,  LongCardCompare,
		    NullDispose, basicTypes[longcards]);
  Build("REAL",     AssignItem,  RealCompare,
		    NullDispose, basicTypes[reals]);
END TypeManager.
