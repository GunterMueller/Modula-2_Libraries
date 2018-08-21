(*
Doubly-linked List Utilities - Implementation

*)

IMPLEMENTATION MODULE ListDBMTools; 
(*==========================================================
    Version  : 1.00  18 May 1989  C. Lins
    Compiler : TopSpeed Modula-2
    Component: Tools - Structure Utility - Doubly-Linked List

    Revision History
    v1.00  18 May 1989  C. Lins
      Initial implementation.
	(C) Copyright 1989 Charles A. Lins
=============================================================*)

FROM Items IMPORT
    (*--Type*) Item;

FROM ListDBM IMPORT
    (*--Type*) List, Pool, NullList,
	(*--Proc*) IsEmpty, GetNext, GetItem, SetNext, SetList,
		Clear, Assign, Insert, GetPrev, SetPrev;

    (*--------------------*)

(*
Constructors

Construct first clears theList, if necessary, and then loops over theItems
in reverse order adding each item to the front of theList.
*)

PROCEDURE Construct (    theItems : ARRAY OF Item (*-- in    *);
	VAR theList  : List	(*-- inout *);
					     thePool  : Pool		  (*-- in    *));

VAR   index : INTEGER; (*-- loop index over items array *)

BEGIN
  IF ~IsEmpty(thePool, theList) THEN
    Clear(thePool, theList);
  END (*--if*);
  FOR index := HIGH(theItems) TO 0 BY -1 DO
  	Insert(thePool, theItems[index], theList);
  END (*--for*);
END Construct;
(*-------------------------*)

(*
Build is quite similar to Construct, above, in traversing theItems in
reverse order appending items to the front of theList. The difference
is that only the first numItems are used to form the list.  If numItems
is greater than the actual number of items in theItems array, only those
items truly present are inserted.
Complexity: O(n) where n is the number of items inserted.
*)

PROCEDURE Build     (    theItems : ARRAY OF Item (*-- in    *);
	numItems : CARDINAL      (*-- in    *);
	VAR theList  : List	(*-- inout *);
					     thePool  : Pool		  (*-- in    *));

VAR   index : INTEGER; (*-- loop index over items array *)

BEGIN
  IF ~IsEmpty(thePool, theList) THEN
    Clear(thePool, theList);
  END (*--if*);
  IF (numItems > VAL(CARDINAL, HIGH(theItems) + 1)) THEN
    numItems := HIGH(theItems) + 1;
  END (*--if*);
  FOR index := VAL(INTEGER, numItems) - 1 TO 0 BY -1 DO
  	Insert(thePool, theItems[index], theList);
  END (*--for*);
END Build;
(*-------------------------*)

(*
AddToTail inserts theItem at the end of theList.  The routine first
takes care of the trivial case where theList is empty.  Then it proceeds
to scan theList for the last item.  Once this has been found, a new list
(of length one) is created for the item, and the last node is linked to
the new list.  Note: once the last node has been found, next is no longer
needed and is therefore used as a temporary to create the new list for
theItem.
Complexity: O(n).
*)

PROCEDURE AddToTail (    theItem  : Item	(*-- in    *);
	VAR theList  : List	(*-- inout *);
					     thePool  : Pool		  (*-- in    *));

VAR   last : List; (*-- last list node examined *)
      next : List; (*-- link from last node to next node *) 

BEGIN
  IF IsEmpty(thePool, theList) THEN
    theList := SetList(thePool, theItem);
  ELSE
    last := theList;
    LOOP
      next := GetNext(thePool, last);
	  IF IsEmpty(thePool, next) THEN
	    EXIT (*--loop*);
	  END (*--if*);
	  last := next;
    END (*--loop*);
    next := SetList(thePool, theItem);
    SetNext(thePool, last, next);
    SetPrev(thePool, next, last);
  END (*--if*);
END AddToTail;
(*-------------------------*)

(*
Split searches theList maintaining a link to the previous node examined.
When theItem is found at the front of theList, the toList becomes theList
and theList becomes the empty list.  Otherwise, theList is truncated at
the previous node examined, and the toList becomes a list beginning with
theItem.
*)

PROCEDURE Split     (VAR theList  : List	(*-- inout *);
	theItem  : Item	(*-- in    *);
	VAR toList   : List	(*-- inout *);
					     thePool  : Pool		  (*-- in    *));

VAR   last  : List; (*-- last list node examined *)
	  prev  : List;

BEGIN
  last  := theList;
  LOOP
	IF IsEmpty(thePool, last) THEN
	  EXIT (*--loop*);
    ELSIF (theItem = GetItem(thePool, last)) THEN
	  Clear(thePool, toList);
	  IF (GetPrev(thePool, last) = NullList) THEN
	    theList := NullList;
	  ELSE
	  	prev := GetPrev(thePool, last);
	    SetNext(thePool, prev, NullList);
	  END (*--if*);
	  toList := last;
	  SetPrev(thePool, toList, NullList);
	  EXIT (*--loop*);
	ELSE
      last  := GetNext(thePool, last);
	END (*--if*);
  END (*--loop*);
END Split;
(*-------------------------*)


(*
Selectors

LastOf simply scans theList looking for the last list node and
returns the node containing the last item. If theList is empty,
the NullList if returned.
*)

PROCEDURE LastOf     (    thePool : Pool     (*-- in    *);
					      theList : List	 (*-- in    *))
	: List     (*-- out   *);

VAR   last : List; (*-- last list node examined *)
      next : List; (*-- link from last node to next node *) 

BEGIN
  IF IsEmpty(thePool, theList) THEN
    RETURN NullList;
  END (*--if*);
  last := theList;
  LOOP
    next := GetNext(thePool, last);
	IF IsEmpty(thePool, next) THEN
	  EXIT (*--loop*);
	END (*--if*);
	last := next;
  END (*--loop*);
  RETURN last;
END LastOf;
(*-------------------------*)

(*
PositionOf does a linear search of theList looking for theItem keeping a
running count of the index position.  If theItem is not found in theList then
the NullList is returned otherwise the index position at which theItem is
found is returned.
*)

PROCEDURE PositionOf (    theItem : Item     (*-- in    *);
	theList : List     (*-- in    *);
					      thePool : Pool	 (*-- in    *))
							: Position (*-- out   *);

VAR   index : Position; (*-- running index into theList *)

BEGIN
  index := 1;
  WHILE ~IsEmpty(thePool, theList) DO
    IF (theItem = GetItem(thePool, theList)) THEN
	  RETURN index;
	END (*--if*);
    INC(index);
	theList := GetNext(thePool, theList);
  END (*--while*);
  RETURN NullPosition;
END PositionOf;
(*-------------------------*)

(*
LocationOf is the inverse of PositionOf given above.  The routine first takes
care of the trivial case in which it is given an empty list by returning an
empty list.  theList parameter is used as a local variable to access each of
the list nodes and if theIndex position exists within theList the link to
that list is returned.  If theList ever becomes empty during the loop the
routine can be certain that theIndex position does not exist.
*)

PROCEDURE LocationOf (    theIndex: Position (*-- in    *);
	theList : List     (*-- in    *);
					      thePool : Pool	 (*-- in    *))
							: List     (*-- out   *);

VAR   index : Position; (*-- running index into theList *)

BEGIN
  IF IsEmpty(thePool, theList) THEN
    RETURN NullList;
  ELSE
    FOR index := 2 TO theIndex DO
	  theList := GetNext(thePool, theList);
	  IF IsEmpty(thePool, theList) THEN
	    RETURN NullList;
      END (*--if*);
	END (*--for*);
	RETURN theList;
  END (*--if*);
END LocationOf;
(*-------------------------*)

END ListDBMTools.
