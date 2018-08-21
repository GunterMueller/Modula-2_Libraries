(*
Doubly-linked List Sequential Search Utilities - Implementation

Presented below are the implementation details of our sequential search
algorithms for the doubly-linked bounded list.
*)

IMPLEMENTATION MODULE ListDBMSearch; 
(*==========================================================
    Version  : 1.00  18 May 1989  C. Lins
    Compiler : TopSpeed Modula-2
    Component: Tools - Structure Utility - Doubly-linked List Search

    Revision History
    v1.00  18 May 1989  C. Lins
      Initial implementation.
	(C) Copyright 1989 Charles A. Lins
==========================================================*)

FROM Relations IMPORT
    (*--Type*) Relation;

FROM Items IMPORT
    (*--Type*) Item, CompareProc;

FROM ListDBM IMPORT
    (*--Type*) List, Pool, NullList,
    (*--Proc*) GetNext, GetItem, IsEmpty, SetNext, SetItem,
			   GetPrev, SetPrev;

    (*--------------------*)

(*
PrimarySearch

Our primary key search routine has been optimized using the LOOP construct
instead of a WHILE statement to avoid multiple calls to IsEmpty,
GetItem, and keysEqual at the end of the routine when determining
success or failure. (See reference [2]).
*)

PROCEDURE PrimarySearch (    theItem   : Item	(*-- in    *);
	theList   : List	(*-- in    *);
	thePool   : Pool	(*-- in    *);
	keysEqual : CompareProc (*-- in    *);
	found     : SuccessProc (*-- in    *);
	notFound  : FailureProc (*-- in    *));
BEGIN
  LOOP 
    IF IsEmpty(thePool, theList) THEN
      notFound(theItem);
      EXIT (*--loop*);
    ELSIF (keysEqual(GetItem(thePool, theList), theItem) = equal) THEN
      found(theItem);
      EXIT (*--loop*);
    END (*--if*);
    theList := GetNext(thePool, theList);
  END (*--loop*);
END PrimarySearch;
(*-------------------------*)

(*
SecondarySearch

Where the primary key search stops after the finding the first occurance
of theItem, the secondary key search continues to the end of the list
in order to find all occurances of theItem.  Therefore, the complexity
is O(n).
*)

PROCEDURE SecondarySearch (    theItem   : Item	(*-- in    *);
	theList   : List	(*-- in    *);
	thePool   : Pool	(*-- in    *);
	keysEqual : CompareProc (*-- in    *);
	found     : SuccessProc (*-- in    *));
BEGIN
  WHILE ~IsEmpty(thePool, theList) DO
    IF keysEqual(GetItem(thePool, theList), theItem) = equal THEN
      found(theItem);
    END (*--if*);
    theList := GetNext(thePool, theList);
  END (*--while*);
END SecondarySearch;
(*-------------------------*)

(*
Self-organizing Sequential Search - MoveToFront

The algorithm first takes care of the special cases where the list is
empty or when the item is already at the front of the list. Then if it
is necessary to scan the list, the routine does so remembering the last
node examined in case theItem is found it can be easily moved to the
front without searching for the node's predecessor.
*)

PROCEDURE MoveToFront (    theItem   : Item	(*-- in    *);
	VAR theList   : List	(*-- inout *);
	thePool   : Pool	(*-- in    *);
	keysEqual : CompareProc (*-- in    *);
	found     : SuccessProc (*-- in    *);
	notFound  : FailureProc (*-- in    *));

VAR   index : List; (*-- loop index over the nodes of the list *)
      next  : List; (*-- link to next node from the last node *)
	  succ	: List; (*-- successor to indexes' successor *)

BEGIN
  IF IsEmpty(thePool, theList) THEN
    notFound(theItem);
  ELSIF keysEqual(theItem, GetItem(thePool, theList)) = equal THEN
    found(theItem);
  ELSE
    index := theList;
	LOOP
	  next := GetNext(thePool, index);
	  IF IsEmpty(thePool, next) THEN
	notFound(theItem);
	    EXIT (*--loop*);
      ELSIF keysEqual(theItem, GetItem(thePool, next)) = equal THEN
		
		(*-- unlink found node from the list *)
		succ := GetNext(thePool, next);
		SetNext(thePool, index, succ);
		IF ~IsEmpty(thePool, succ) THEN
		  SetPrev(thePool, succ, index);
		END (*--if*);
		
		(*-- relink found node at front of the list *)
		SetPrev(thePool, next, NullList);
	SetNext(thePool, next, theList);
		SetPrev(thePool, theList, next);
		
		(*-- return found node as the new front *)
	theList := next;
	found(theItem);
	EXIT (*--loop*);
      END (*--if*);
	  index := next;
    END (*--loop*);
  END (*--if*);
END MoveToFront;
(*-------------------------*)

(*
Self-organizing Sequential Search - Transpose

The transpose hueristic is similar to the Move-To-Front method, above,
except when theItem is found it is moved only one position forward in
the list.  Here, theItem is used as a temporary when moving the value
forward in the list.  For a Pascal version of this algorithm using arrays
see Gonnet [2].
*)

PROCEDURE Transpose (    theItem   : Item	(*-- in    *);
	VAR theList   : List	(*-- inout *);
	thePool   : Pool	(*-- in    *);
	keysEqual : CompareProc (*-- in    *);
	found     : SuccessProc (*-- in    *);
	notFound  : FailureProc (*-- in    *));

VAR   index : List; (*-- node being examined *)
	  prev  : List; (*-- previous node *)
	  data  : Item;

BEGIN
  IF IsEmpty(thePool, theList) THEN
    notFound(theItem);
  ELSIF keysEqual(theItem, GetItem(thePool, theList)) = equal THEN
    found(theItem);
  ELSE
    index := theList;
	LOOP
	  index := GetNext(thePool, index);
	  IF IsEmpty(thePool, index) THEN
	notFound(theItem);
	    EXIT (*--loop*);
      ELSIF keysEqual(theItem, GetItem(thePool, index)) = equal THEN
		data := GetItem(thePool, index);
	    prev := GetPrev(thePool, index);
	SetItem(thePool, index, GetItem(thePool, prev));
		SetItem(thePool, prev, data);
	found(theItem);
	EXIT (*--loop*);
      END (*--if*);
    END (*--loop*);
  END (*--if*);
END Transpose;
(*-------------------------*)

END ListDBMSearch.

(*

References

[1] J.L. Bentley and C.C. McGeoch, Worst-Case Analyses of
    Self-Organizing Sequential Search Heuristics,
	Carnegie-Mellon University, 1983.
[2] G.H. Gonnet, Handbook of Algorithms and Data Structures,
    Searching Algorithms, Addison-Wesley, London 1984,
	pp. 23-30.
*)
