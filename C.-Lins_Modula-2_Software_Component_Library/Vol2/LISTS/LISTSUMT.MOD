(*
8.4 List Utilities - Implementation

*)

IMPLEMENTATION MODULE ListSUMTools; 
(*==========================================================
    Version  : 1.00  18 May 1989  C. Lins
    Compiler : TopSpeed Modula-2
    Component: Tools - Structure Utility - Singly-Linked List

    Revision History
    v1.00  18 May 1989  C. Lins
      Initial TopSpeed Modula-2 implementation.
	(C) Copyright 1989 Charles A. Lins
=============================================================*)

FROM Items IMPORT
    (*--Type*) Item;

FROM ListSUM IMPORT
    (*--Type*) List, NullList,
	(*--Proc*) IsEmpty, GetNext, GetItem, SetNext, SetList,
		Clear, Assign, Insert;

    (*--------------------*)

(*
8.4.1 Constructors

Construct first clears theList, if necessary, and then loops over theItems
in reverse order adding each item to the front of theList.
*)

PROCEDURE Construct (    theItems : ARRAY OF Item (*-- in    *);
	VAR theList  : List	(*-- inout *));

VAR   index : INTEGER; (*-- loop index over items array *)

BEGIN
  IF ~IsEmpty(theList) THEN
    Clear(theList);
  END (*--if*);
  FOR index := HIGH(theItems) TO 0 BY -1 DO
  	Insert(theItems[index], theList);
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
	VAR theList  : List	(*-- inout *));

VAR   index : INTEGER; (*-- loop index over items array *)

BEGIN
  IF ~IsEmpty(theList) THEN
    Clear(theList);
  END (*--if*);
  IF (numItems > VAL(CARDINAL, HIGH(theItems) + 1)) THEN
    numItems := HIGH(theItems) + 1;
  END (*--if*);
  FOR index := VAL(INTEGER, numItems) - 1 TO 0 BY -1 DO
  	Insert(theItems[index], theList);
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
	VAR theList  : List	(*-- inout *));

VAR   last : List; (*-- last list node examined *)
      next : List; (*-- link from last node to next node *) 

BEGIN
  IF IsEmpty(theList) THEN
    theList := SetList(theItem);
  ELSE
    last := theList;
    LOOP
      next := GetNext(last);
	  IF IsEmpty(next) THEN
	    EXIT (*--loop*);
	  END (*--if*);
	  last := next;
    END (*--loop*);
    next := SetList(theItem);
    SetNext(last, next);
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
	VAR toList   : List	(*-- inout *));

VAR   last  : List; (*-- last list node examined *)
      prior : List; (*-- link from last node to prior node examined *) 

BEGIN
  last  := theList;
  prior := NullList;
  LOOP
	IF IsEmpty(last) THEN
	  EXIT (*--loop*);
    ELSIF (theItem = GetItem(last)) THEN
	  Clear(toList);
	  IF (prior = NullList) THEN
	    theList := NullList;
	  ELSE
	    SetNext(prior, NullList);
	  END (*--if*);
	  toList := last;
	  EXIT (*--loop*);
	ELSE
	  prior := last;
      last  := GetNext(last);
	END (*--if*);
  END (*--loop*);
END Split;
(*-------------------------*)


(*
8.4.2 Selectors

LastOf simply scans theList looking for the last list node and
returns the node containing the last item. If theList is empty,
the NullList if returned.
*)

PROCEDURE LastOf    (    theList : List  (*-- in    *))
	: List  (*-- out   *);

VAR   last : List; (*-- last list node examined *)
      next : List; (*-- link from last node to next node *) 

BEGIN
  IF IsEmpty(theList) THEN
    RETURN NullList;
  END (*--if*);
  last := theList;
  LOOP
    next := GetNext(last);
	IF IsEmpty(next) THEN
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
	theList : List     (*-- in    *))
							: Position (*-- out   *);

VAR   index : Position; (*-- running index into theList *)

BEGIN
  index := 1;
  WHILE ~IsEmpty(theList) DO
    IF (theItem = GetItem(theList)) THEN
	  RETURN index;
	END (*--if*);
    INC(index);
	theList := GetNext(theList);
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
	theList : List     (*-- in    *))
							: List     (*-- out   *);

VAR   index : Position; (*-- running index into theList *)

BEGIN
  IF IsEmpty(theList) THEN
    RETURN NullList;
  ELSE
    FOR index := 2 TO theIndex DO
	  theList := GetNext(theList);
	  IF IsEmpty(theList) THEN
	    RETURN NullList;
      END (*--if*);
	END (*--for*);
	RETURN theList;
  END (*--if*);
END LocationOf;
(*-------------------------*)

END ListSUMTools.

(*

References

[1]  A. Aho, J. Hopcroft, and J. Ullman, Data Structures and Algorithms,
    Addison-Wesley, Reading, MA 1983
[2] G. Booch, Software Components with Ada Structures, Tools, and Subsystems,
	Benjamin/Cummings, Menlo Park, CA 1987
[3] G.H. Gonnet, Handbook of Algorithms and Data Structures,
    Addison-Wesley, London England 1984
[4]	D. Knuth, The Art of Computer Programming, Volume 1, Fundamental Algorithms,
    Addison-Wesley, Reading, MA 1973
[5]	N. Wirth, Algorithms and Data Structures, Prentice-Hall, Englewood
    Cliffs, NJ 1986
*)
