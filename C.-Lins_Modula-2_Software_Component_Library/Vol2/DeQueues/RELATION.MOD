IMPLEMENTATION MODULE Relations;
(*====================================================================
    Version  : 1.0  Sat, Mar 4, 1989  C. Lins
    Compiler : JPI TopSpeed Modula-2
    Code Size: R-  bytes
    Component: Tool - Relational Operators Utility

    REVISION HISTORY
    v1.0  Sat, Mar 4, 1989  C. Lins
      Initial JPI implementation.

    INTRODUCTION
    This module provides definitions for the standard Modula-2 relations
    and relational operators, along with routines for mapping one type to
    the other, deriving the NOT of a relational operator, and testing for
    a given relational operator implying that another relational operator
    is also true.

    LOCAL DATA TYPES
    * RelSet	 Defines a set of relational operators.

    LOCAL VARIABLES
    * relMap
	Mapping array of relational operators to a RelSet. It is used by the
	relational expression routines to easily determine whether the result
	from a comparison routine meeting the expression relation.

		   Set of
		  Relations
	  Index    Mapping
	  ÑÑÑÑÑ   ÑÑÑÑÑÑÑÑÑ
	    <=	  {<=, <, =}
	    <	  {<=, <, #}
	    =	  {<=, =, >=}
	    #	  {<, #, >}
	    >	  {#, >, >=}
	    >=	  {=, >, >=}
	    ?	  {}

    * NOTRelation
	Mapping of relational operators to the opposite operator, i.e., = ç #.

    INITIALIZATION ROUTINES
    * InitRelMapping:  Initialize the mapping array relMap.
    * InitNOTMapping:  Initialize the mapping array NOTRelation.

    PROPRIETARY NOTICES
    Relations.MOD, by C. Lins, Copyright (C) 1989 Charles A. Lins.
====================================================================*)


VAR   mapToRelOp : ARRAY Relation OF RelOp;

PROCEDURE RelToRelOp (	  theRelation: Relation (*-- in    *))
				     : RelOp	(*-- out   *);
BEGIN
  RETURN mapToRelOp [ theRelation ];
END RelToRelOp;
(*----------------------------*)


VAR   mapRelation: ARRAY RelOp OF Relation;

PROCEDURE RelOpToRel (	  theRelOp   : RelOp	(*-- in    *))
				     : Relation (*-- out   *);
BEGIN
  RETURN mapRelation [ theRelOp ];
END RelOpToRel;
(*----------------------------*)


VAR   notRelOp : ARRAY RelOp OF RelOp;

PROCEDURE NotRelOpOf (	  theRelOp   : RelOp	(*-- in    *))
				     : RelOp	(*-- out   *);
BEGIN
  RETURN notRelOp [ theRelOp ];
END NotRelOpOf;
(*----------------------------*)


TYPE  RelSet = SET OF RelOp;
VAR   relMap : ARRAY RelOp OF RelSet;

PROCEDURE AImpliesB  (	  left	     : RelOp	(*-- in    *);
			  right      : RelOp	(*-- in    *))
				     : BOOLEAN	(*-- out   *);
BEGIN
  RETURN left IN relMap [ right ];
END AImpliesB;
(*----------------------------*)


(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
(*   MODULE INITIALIZATION    *)

BEGIN
  relMap [ lt ] := RelSet {le, lt, ne};
  relMap [ le ] := RelSet {le, lt, eq};
  relMap [ eq ] := RelSet {le, eq, ge};
  relMap [ ne ] := RelSet {lt, ne, gt};
  relMap [ gt ] := RelSet {ge, gt, ne};
  relMap [ ge ] := RelSet {eq, ge, gt};
  relMap [ unordered ] := RelSet {};

  notRelOp [ lt ] := ge;
  notRelOp [ le ] := gt;
  notRelOp [ eq ] := ne;
  notRelOp [ ne ] := eq;
  notRelOp [ gt ] := le;
  notRelOp [ ge ] := lt;
  notRelOp [ unordered ] := unordered;

  mapToRelOp  [ less	] := lt;
  mapToRelOp  [ equal	] := eq;
  mapToRelOp  [ greater ] := gt;
  mapToRelOp  [incomparable] := unordered;

  mapRelation [ lt ] := less;
  mapRelation [ le ] := incomparable;
  mapRelation [ eq ] := equal;
  mapRelation [ ne ] := incomparable;
  mapRelation [ gt ] := greater;
  mapRelation [ ge ] := incomparable;
  mapRelation [ unordered ] := incomparable;
END Relations.
