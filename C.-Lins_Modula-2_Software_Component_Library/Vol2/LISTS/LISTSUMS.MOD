(*
8.2 Sequential Search Utilities - Implementation

Presented below are the implementation details of our sequential search
algorithms for the singly-linked unbounded list.
*)

IMPLEMENTATION MODULE ListSUMSearch; 
(*==========================================================
    Version  : 1.00  18 May 1989  C. Lins
    Compiler : TopSpeed Modula-2 Compiler
    Component: Tools - Structure Utility - Single List Search

    Revision History
    v1.00  18 May 1989  C. Lins
      Initial implementation.
	(C) Copyright 1989 Charles A. Lins
==========================================================*)

FROM Relations IMPORT
    (*--Type*) Relation;

FROM Items IMPORT
    (*--Type*) Item, CompareProc;

FROM ListSUM IMPORT
    (*--Type*) List,
    (*--Proc*) GetNext, GetItem, IsEmpty, SetNext, SetItem;

    (*--------------------*)

(*
8.2.1 PrimarySearch

Our primary key search routine has been optimized using the LOOP construct
instead of a WHILE statement to avoid multiple calls to IsEmpty,
GetItem, and keysEqual at the end of the routine when determining
success or failure. (See reference [2]).
*)

PROCEDURE PrimarySearch (    theItem   : Item	(*-- in    *);
	theList   : List	(*-- in    *);
	keysEqual : CompareProc (*-- in    *);
	found     : SuccessProc (*-- in    *);
	notFound  : FailureProc (*-- in    *));
BEGIN
  LOOP 
    IF IsEmpty(theList) THEN
      notFound(theItem);
      EXIT (*--loop*);
    ELSIF (keysEqual(GetItem(theList), theItem) = equal) THEN
      found(theItem);
      EXIT (*--loop*);
    END (*--if*);
    theList := GetNext(theList);
  END (*--loop*);
END PrimarySearch;
(*-------------------------*)

(*
8.2.2 SecondarySearch

Where the primary key search stops after the finding the first occurance
of theItem, the secondary key search continues to the end of the list
in order to find all occurances of theItem.  Therefore, the complexity
is O(n).
*)

PROCEDURE SecondarySearch (    theItem   : Item	(*-- in    *);
	theList   : List	(*-- in    *);
	keysEqual : CompareProc (*-- in    *);
	found     : SuccessProc (*-- in    *));
BEGIN
  WHILE ~IsEmpty(theList) DO
    IF keysEqual(GetItem(theList), theItem) = equal THEN
      found(theItem);
    END (*--if*);
    theList := GetNext(theList);
  END (*--while*);
END SecondarySearch;
(*-------------------------*)

(*
8.2.3 Self-organizing Sequential Search - MoveToFront

The algorithm first takes care of the special cases where the list is
empty or when the item is already at the front of the list. Then if it
is necessary to scan the list, the routine does so remembering the last
node examined in case theItem is found it can be easily moved to the
front without searching for the node's predecessor.
*)

PROCEDURE MoveToFront (    theItem   : Item	(*-- in    *);
	VAR theList   : List	(*-- inout *);
	keysEqual : CompareProc (*-- in    *);
	found     : SuccessProc (*-- in    *);
	notFound  : FailureProc (*-- in    *));

VAR   last : List; (*-- last node examined *)
      next : List; (*-- link to next node from the last node *)
      temp : List; (*-- temporary for swapping three links *)

BEGIN
  IF IsEmpty(theList) THEN
    notFound(theItem);
  ELSIF keysEqual(theItem, GetItem(theList)) = equal THEN
    found(theItem);
  ELSE
    last := theList;
	LOOP
	  next := GetNext(last);
	  IF IsEmpty(next) THEN
	notFound(theItem);
	    EXIT (*--loop*);
      ELSIF keysEqual(theItem, GetItem(next)) = equal THEN
	temp    := theList;
	theList := next;
	SetNext(last, GetNext(next));
	SetNext(theList, temp);
	found(theItem);
	EXIT (*--loop*);
      END (*--if*);
	  last := next;
    END (*--loop*);
  END (*--if*);
END MoveToFront;
(*-------------------------*)

(*
8.2.4 Self-organizing Sequential Search - Transpose

The transpose hueristic is similar to the Move-To-Front method, above,
except when theItem is found it is moved only one position forward in
the list.  Here, theItem is used as a temporary when moving the value
forward in the list.  For a Pascal version of this algorithm using arrays
see Gonnet [2].
*)

PROCEDURE Transpose (    theItem   : Item	(*-- in    *);
	VAR theList   : List	(*-- inout *);
	keysEqual : CompareProc (*-- in    *);
	found     : SuccessProc (*-- in    *);
	notFound  : FailureProc (*-- in    *));

VAR   last : List; (*-- last node examined *)
      next : List; (*-- link to next node from the last node *)

BEGIN
  IF IsEmpty(theList) THEN
    notFound(theItem);
  ELSIF keysEqual(theItem, GetItem(theList)) = equal THEN
    found(theItem);
  ELSE
    last := theList;
	LOOP
	  next := GetNext(last);
	  IF IsEmpty(next) THEN
	notFound(theItem);
	    EXIT (*--loop*);
      ELSIF keysEqual(theItem, GetItem(next)) = equal THEN
	SetItem(next, GetItem(last));
		SetItem(last, theItem);
	found(theItem);
	EXIT (*--loop*);
      END (*--if*);
	  last := next;
    END (*--loop*);
  END (*--if*);
END Transpose;
(*-------------------------*)

END ListSUMSearch.

(*

References

[1] J.L. Bentley and C.C. McGeoch, Worst-Case Analyses of
    Self-Organizing Sequential Search Heuristics,
	Carnegie-Mellon University, 1983.
[2] G.H. Gonnet, Handbook of Algorithms and Data Structures,
    Searching Algorithms, Addison-Wesley, London 1984,
	pp. 23-30.
*)
