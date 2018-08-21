(*
13.2	SetSUMI Implementation
*)

IMPLEMENTATION MODULE SetSUMI;
(*==========================================================
    Version  : 1.00  30 Apr 1989 1988  C. Lins
    Compiler : TopSpeed Modula-2
    Code Size: R-  bytes
    Component: Monolithic Structures - Set
               Sequential Unbounded Managed Iterator

    INTRODUCTION
    This module implements the unbounded Set abstraction for
    generic Items using a linearly ordered list.

    REVISION HISTORY
    v1.00  30 Apr 1988  C. Lins
        Initial implementation for TopSpeed Modula-2.

    (C) Copyright 1989 Charles A. Lins
=============================================================*)

FROM JPIStorage IMPORT
    (*--Proc*) Allocate, Deallocate;

FROM Items IMPORT
    (*--Type*) Item, AssignProc, CompareProc, DisposeProc,
	           AccessProc, LoopAccessProc;

FROM ErrorHandling IMPORT
    (*--Type*) HandlerProc,
    (*--Proc*) Raise, NullHandler, ExitOnError;

FROM Relations IMPORT
    (*--Type*) Relation;

FROM SetEnum IMPORT
    (*--Type*) Exceptions, Operations, ComponentID;

FROM TypeManager IMPORT
    (*--Cons*) NullType,
    (*--Type*) TypeID,
    (*--Proc*) AssignOf, CompareOf, DisposeOf;

    (*--------------------*)


(*
13.2.1	Internal Unbounded Set Representation

ÇIllustration HereÈ

Figure 13.1

For the internal representation of an unbounded set, a linear linked list is used as shown
above in Figure 13.1.  Each item of the set is stored in a node  which is linked to its
immediate successor.  The unbounded set header maintains the data type ID of the set, its
current length, and the link to the first node of the list of set member items.

Representation Invariants
1. when length = 0, first = NIL
2. when length > 0, first ­ NIL
3. the each item node is linked to its successor and the last item node has a next of NIL
4. first^.item < first^.next^.item and item < item^.next^.item
5. length contains the number of nodes in the list
*)


TYPE  Link = POINTER TO Node;
TYPE  Node = RECORD  (*-- a set item node *)
        item : Item; (*-- the item nodes' data *)
        next : Link; (*-- link to next node in list *)
	  END (*-- Node *);

TYPE  UnboundedSet = RECORD (*-- set header *)
        dataID : TypeID;    (*-- defined data type *)
        length : CARDINAL;  (*-- current set length, := 0 *)
        first  : Link;      (*-- link to first Item node *)
      END (*-- UnboundedSet *);

TYPE  Set = POINTER TO UnboundedSet;



(*
13.2.2		Exceptions

To support the exception handling mechanism two variables are needed.  The first,
setError, is used to record the exception code from each operation; while handlers  is an
array of exception handling procedures indexed by the exception code.

The routines SetError, GetHandler, and SetHandler have been previously described in the
definition module, and their operation should be readily apparent.  RaiseErrIn  is a local
routine used to set the setError variable and invoke the Raise  routine of the
ErrorHandling module.
*)

VAR   setError  : Exceptions;
VAR   handlers  : ARRAY Exceptions OF HandlerProc;


    (*-----------------------*)

PROCEDURE SetError () : Exceptions (*-- out   *);
BEGIN
  RETURN setError;
END SetError;
(*--------------------*)

PROCEDURE GetHandler  (    ofError   : Exceptions  (*-- in    *))
                                     : HandlerProc (*-- out   *);
BEGIN
  RETURN handlers[ofError];
END GetHandler;
(*--------------------*)

PROCEDURE SetHandler  (    ofError   : Exceptions  (*-- in    *);
                           toHandler : HandlerProc (*-- in    *));
BEGIN
  handlers[ofError] := toHandler;
END SetHandler;
(*--------------------*)

PROCEDURE RaiseErrIn (    theRoutine : Operations    (*-- in    *);
                          theError   : Exceptions    (*-- in    *));
BEGIN
  setError := theError;
  Raise(ComponentID + ModuleID, theRoutine, theError, handlers[theError]);
END RaiseErrIn;
(*--------------------*)


(*
13.2.3	Local Routines

Many set routines need to create new item nodes and NewNode  performs this; setting the
item and link fields to the new node which is returned.  There are two ÒstateÓ variables
used by NewNode: (1) setRoutine, and (2) assignItem, which are used when raising the
overflow exception, and when copying items values, respectively.  A routine, such as
Union, may call NewNode many times throughout its execution and it would be quite
inefficient to repeatedly pass these as parameters.

Several set algorithms require the ability to copy the all remaining items from a given set
to a partially created target set.  CopySubset  performs this operation by looping through
the fromSet starting from the given index to the end of the set copying each item to the
target set along the way.

All routines that accept a target set as an inout parameter need to either (1) clear the set of
its present contents if the set already exists, or (2) create a new, empty set to be target of
the operation.  The Recreate  routine provides such a capability, returning true if
successful.  It should be noted that failure only occurs as a result when the set must be
created.
*)

VAR   setRoutine : Operations;  (*-- Routine calling NewNode *)
VAR   assignItem : AssignProc;  (*-- Item assignment routine *)

PROCEDURE NewNode       (    theItem : Item        (*-- in    *);
       	       	             theNext : Link        (*-- in    *))
       	       	                     : Link        (*-- out   *);

VAR   newLink : Link;

BEGIN
  Allocate(newLink, SIZE(Node));
  IF (newLink = NIL) THEN
    RaiseErrIn(setRoutine, overflow);
  ELSE
    WITH newLink^ DO
      item := assignItem(theItem);
      next := theNext;
    END (*--with*);
  END (*--if*);
  RETURN newLink;
END NewNode;
(*--------------------*)

PROCEDURE CopySubset (    fromIndex  : Link       (*-- in    *);
                          toIndex    : Link       (*-- in    *);
                      VAR toSet      : Set        (*-- inout *));

VAR   tempNode : Link;

BEGIN
  LOOP
    IF (fromIndex = NIL) THEN
      EXIT (*--loop*);
    ELSE
      tempNode := NewNode(fromIndex^.item, NIL);
      IF (tempNode = NIL) THEN
        EXIT (*--loop*);
      END (*--if*);
      INC(toSet^.length);
      IF (toSet^.first = NIL) THEN
        toSet^.first := tempNode;
      ELSE
        toIndex^.next := tempNode;
      END (*--if*);
      toIndex   := tempNode;
      fromIndex := fromIndex^.next;
    END (*--if*);
  END (*--loop*);
END CopySubset;
(*--------------------*)

PROCEDURE Recreate (    theType : TypeID    (*-- in    *);
                    VAR theSet  : Set       (*-- inout *))
                                : BOOLEAN   (*-- out   *);
BEGIN
  IF (theSet # NIL) THEN
    Clear(theSet);
    theSet^.dataID := theType;
  ELSE
    theSet := Create(theType);
  END (*--if*);
  RETURN (setError = noerr);
END Recreate;
(*--------------------*)



(*
13.2.4		Constructors

Create attempts to allocate a new, empty unbounded set header, which if successful allows
the set to be initialized to an empty state with the given data type ID, a length of zero and
a pointer to the first item node of NIL.  If unable to allocate the header the overflow
exception is raised and the NullSet is returned.
*)

PROCEDURE Create        (    theType  : TypeID    (*-- in    *))
                                      : Set       (*-- out   *);

VAR   newSet : Set; (*-- new set variable being created *)

BEGIN
  setError := noerr;
  Allocate(newSet, SIZE(UnboundedSet));
  IF (newSet # NIL) THEN
    WITH newSet^ DO
      dataID := theType;
      length := 0;
      first  := NIL;
    END (*--with*);
    RETURN newSet;
  END (*--if*);
  RaiseErrIn(create, overflow);
  RETURN NullSet;
END Create;
(*--------------------*)


(*
Destroy takes advantage that Clear  sets setError  to noerr and raises the undefined  set
exception.  So if Clear succeeds, Destroy releases the allocated set header.
*)

PROCEDURE Destroy       (VAR theSet   : Set       (*-- inout *));
BEGIN
  Clear(theSet);
  IF (setError = noerr) THEN
    Deallocate(theSet, SIZE(theSet^));
  END (*--if*);
END Destroy;
(*--------------------*)


(*
Clear  sets setError  to noerr then ensures a valid set object, raising the undefined 
exception, if necessary.  Then the set nodes are traversed, in order, deallocating each item
and then its node.  The representation invariants state that when length is zero, first must
be NIL, and so we repeatedly adjust first since our last step will be to set length to zero. 
The loop is guaranteed to terminate as the last item node must have a next of NIL.
*)

PROCEDURE Clear         (VAR theSet   : Set       (*-- inout *));

VAR   freeItem : DisposeProc; (*-- Item disposal routine, if any *)
      theNode  : Link;        (*-- Set node to be deallocated *)

BEGIN
  setError := noerr;
  IF (theSet # NIL) THEN
    WITH theSet^ DO
      freeItem := DisposeOf(dataID);
      WHILE (first # NIL) DO
        theNode := first;
        first   := first^.next;
        freeItem(theNode^.item);
        Deallocate(theNode, SIZE(theNode^));
      END (*--while*);
      length := 0;
    END (*--with*);
  ELSE
    RaiseErrIn(clear, undefined);
  END (*--if*);
END Clear;
(*--------------------*)


(*
Assign attempts to duplicate the items of the source set in the target set, avoiding the
useless operation of assigning a set to itself and recreating the target set if necessary.  To
simplify the assignment, the routine initially copies the first node from the source to the
target and then proceeds to loop through any remaining item nodes.  This is done to
maintain the elements of the target in the same order as they appeared in the source.
*)

PROCEDURE Assign        (    theSet   : Set       (*-- in    *);
                         VAR toSet    : Set       (*-- inout *));

VAR   fromIndex : Link; (*-- Loop index over source set items *)
      toIndex   : Link; (*-- Loop index over target set items *)

BEGIN
  setError := noerr;
  IF (theSet # NIL) THEN

    IF (toSet = theSet) OR ~Recreate(theSet^.dataID, toSet) THEN
      RETURN;
    END (*--if*);

    WITH theSet^ DO
      IF (first = NIL) THEN
        RETURN;
      END (*--if*);
      assignItem := AssignOf(dataID);
    END (*--with*);

    setRoutine := assign;
    WITH toSet^ DO
      first := NewNode(theSet^.first^.item, NIL);
      IF (setError = overflow) THEN
        RETURN;
      END (*--if*);
      toIndex     := first;
      fromIndex   := theSet^.first;
    END (*--with*);

    WHILE (fromIndex^.next # NIL) DO
      fromIndex     := fromIndex^.next;
      toIndex^.next := NewNode(fromIndex^.item, NIL);
      IF (setError = overflow) THEN
        RETURN;
      END (*--if*);
      toIndex := toIndex^.next;
    END (*--while*);
    toSet^.length := theSet^.length;
  ELSE
    RaiseErrIn(assign, undefined);
  END (*--if*);
END Assign;
(*--------------------*)


(*
Include  must add the given item to the set if it is not already a member or to simply exit
if the item is already a member (these semantics are compatible with Modula-2's INCL
operation).  We could use the IsAMember selector except the index where the item is not
found is necessary to insert the item in its appropriate position within the ordered list. 
Once we have this position, we make add the new item using the standard ordered linked
list insertion algorithm.
*)

PROCEDURE Include       (    theItem  : Item      (*-- in    *);
                         VAR inSet    : Set       (*-- inout *));

VAR   current     : Link;        (*-- Loop index in search of theItem *)
      previous    : Link;        (*-- Previous node examined *)
      newNode     : Link;        (*-- For new node to add *)
      compareItem : CompareProc; (*-- Item comparison routine *)
      itemOrder   : Relation;    (*-- Ordering relation between items *)

BEGIN
  setError := noerr;
  IF (inSet # NIL) THEN
    compareItem := CompareOf(inSet^.dataID);
    current     := inSet^.first;
    previous    := NIL;
    LOOP
      IF (current = NIL) THEN
        EXIT (*--loop*);
      END (*--if*);

      itemOrder := compareItem(current^.item, theItem);

      IF (itemOrder = equal) THEN
        RETURN;

      ELSIF (itemOrder = greater) THEN
        EXIT (*--loop*);
      END (*--if*);

      previous := current;
      current  := current^.next;
    END (*--loop*);

    (*-- Insert the new item *)

    Allocate(newNode, SIZE(Node));
    IF (newNode = NIL) THEN
      RaiseErrIn(include, overflow);
    ELSE
      newNode^.item := theItem;
      IF (previous = NIL) THEN
        newNode^.next := inSet^.first;
        inSet^.first  := newNode;
      ELSE
        newNode^.next  := current;
        previous^.next := newNode;
      END (*--if*);
      INC(inSet^.length);
    END (*--if*);
  ELSE
    RaiseErrIn(include, undefined);
  END (*--if*);
END Include;
(*--------------------*)


(*
Exclude undoes what Include did to add an item to the set. The first step is to determine if
the given item is present in the set.  If the item is not found the routine simply exits to
be compatible with Modula-2's EXCL operation.  If the item is found the list is updated
by relinking nodes using the standard ordered linked list node deletion algorithm, the
removed node and its item are both deallocated, and the set length is updated to reflect the
removal of the item. 
*)

PROCEDURE Exclude       (    theItem  : Item      (*-- in    *);
                         VAR fromSet  : Set       (*-- inout *));

VAR   current     : Link;        (*-- Loop index over items *)
      previous    : Link;        (*-- Previous node examined *)
      compareItem : CompareProc; (*-- Item comparison routine *)
      itemOrder   : Relation;    (*-- Relation between items *)
      freeItem    : DisposeProc; (*-- Item disposal routine *)

BEGIN
  setError := noerr;
  IF (fromSet # NIL) THEN
    compareItem := CompareOf(fromSet^.dataID);
    current     := fromSet^.first;
    previous    := NIL;
    LOOP
      IF (current = NIL) THEN
        RETURN;
      END (*--if*);

      itemOrder := compareItem(current^.item, theItem);

      IF (itemOrder = equal) THEN
        EXIT (*--loop*);
      ELSIF (itemOrder = greater) THEN
        RETURN;
      END (*--if*);

      previous := current;
      current  := current^.next;
    END (*--loop*);

    (*-- ÒcurrentÓ points to the node to be deleted. *)

    IF (previous = NIL) THEN
      fromSet^.first := current^.next;
    ELSE
      previous^.next := current^.next;
    END (*--if*);
    freeItem := DisposeOf(fromSet^.dataID);
    freeItem(current^.item);
    Deallocate(current, SIZE(current^));
    DEC(fromSet^.length);
  ELSE
    RaiseErrIn(exclude, undefined);
  END (*--if*);
END Exclude;
(*--------------------*)


(*
Union computes the set containing all members of left and right, e.g.,

x IN toSet iff (x IN left) OR (x IN right).

The algorithm used is a variation on the array merge from reference [2], pg. 414 and the
ordered list set intersection algorithm presented in Aho, Hopcroft, and Ullman [1, pg.
117].  It is essentially the same algorithm used in the previous chapter on the bounded set
converted to work with ordered linear linked lists.

The algorithm loops over the items of the left and right sets until the end of either is
reached.  On each iteration, the items are compared for the ordering relation between them. 
This is used to determine from which set an item is copied to the target set, and which
indexes to advance.  In this manner, all items are processed only once and duplicate items
in the target set are avoided.

The last step is to copy the remaining items, if any, from either the left set or the right
set to the destination set.
*)

PROCEDURE Union         (    left     : Set       (*-- in    *);
                             right    : Set       (*-- in    *);
                         VAR toSet    : Set       (*-- inout *));

VAR   leftIndex   : Link;        (*-- Loop index over left set *)
      rightIndex  : Link;        (*-- Loop index over right set *)
      toIndex     : Link;        (*-- List of target set nodes *)
      tempNode    : Link;        (*-- Temporary node *)
      compareItem : CompareProc; (*-- Item comparison routine *)
      order       : Relation;    (*-- Ordering relation between items *)

BEGIN
  setError := noerr;
  IF (left = NIL) OR (right = NIL) THEN
    RaiseErrIn(union, undefined);
    RETURN;
  ELSIF (left^.dataID # right^.dataID) THEN
    RaiseErrIn(union, typeerror);
    RETURN;
  ELSIF ~Recreate(left^.dataID, toSet) THEN
    RETURN;
  END (*--if*);

  WITH toSet^ DO
    compareItem := CompareOf(dataID);
    assignItem  := AssignOf(dataID);
  END (*--with*);

  setRoutine := union;
  leftIndex  := left^.first;
  rightIndex := right^.first;
  WHILE (leftIndex # NIL) & (rightIndex # NIL) DO
    order := compareItem(leftIndex^.item, rightIndex^.item);
    Allocate(tempNode, SIZE(Node));
    IF (tempNode = NIL) THEN
      RaiseErrIn(union, overflow);
      RETURN;
    END (*--if*);
    tempNode^.next := NIL;
    INC(toSet^.length);

    IF (order = less) THEN
      tempNode^.item := assignItem(leftIndex^.item);
      leftIndex := leftIndex^.next;

    ELSIF (order = equal) THEN
      tempNode^.item := assignItem(leftIndex^.item);
      leftIndex := leftIndex^.next;
      rightIndex := rightIndex^.next;

    ELSE
      tempNode^.item := assignItem(rightIndex^.item);
      rightIndex := rightIndex^.next;
    END (*--if*);

    (*-- Update the linked list *)

    IF (toSet^.first = NIL) THEN
      toSet^.first := tempNode;
    ELSE
      toIndex^.next := tempNode;
    END (*--if*);
    toIndex := tempNode;
  END (*--while*);

  IF (leftIndex = NIL) THEN
    CopySubset(rightIndex, toIndex, toSet);
  ELSIF (rightIndex = NIL) THEN
    CopySubset(leftIndex, toIndex, toSet);
  END (*--if*);
END Union;
(*--------------------*)


(*
Intersection computes the set containing all members in both left and right sets, e.g.,

x IN toSet iff (x IN left) AND (x IN right).

The algorithm, from that given by Aho, Hopcroft, and Ullman, in [1, pg. 117], loops
over the items of the left and right sets until the end of either is reached.  On each
iteration, the items are compared for the ordering relation between them.  Equal items are
copied to the target set and both indexes are advanced, otherwise the index to the smaller
item is advanced.  In this manner, all items are processed only once and duplicate items in
the target set are avoided.
*)

PROCEDURE Intersection  (    left    : Set         (*-- in    *);
                             right   : Set         (*-- in    *);
                         VAR toSet   : Set         (*-- inout *));

VAR   leftIndex   : Link;        (*-- Loop link over left set *)
      rightIndex  : Link;        (*-- Loop link over right set *)
      toIndex     : Link;        (*-- List of target set nodes *)
      compareItem : CompareProc; (*-- Item comparison routine *)
      order       : Relation;    (*-- Ordering relation between items *)

BEGIN
  setError := noerr;
  IF (left = NIL) OR (right = NIL) THEN
    RaiseErrIn(intersection, undefined);
    RETURN;
  ELSIF (left^.dataID # right^.dataID) THEN
    RaiseErrIn(intersection, typeerror);
    RETURN;
  ELSIF ~Recreate(left^.dataID, toSet) THEN
    RETURN;
  END (*--if*);

 
  WITH toSet^ DO
    compareItem := CompareOf(dataID);
    assignItem  := AssignOf(dataID);
  END (*--with*);

  setRoutine := intersection;
  leftIndex  := left^.first;
  rightIndex := right^.first;
  WHILE (leftIndex # NIL) & (rightIndex # NIL) DO
    order := compareItem(leftIndex^.item, rightIndex^.item);

    IF (order = equal) THEN
      IF (toSet^.first = NIL) THEN
        toSet^.first := NewNode(leftIndex^.item, NIL);
        toIndex := toSet^.first;
      ELSE
        toIndex^.next := NewNode(leftIndex^.item, NIL);
        toIndex := toIndex^.next;
      END (*--if*);
      IF (setError = overflow) THEN
        RETURN;
      END (*--if*);
      INC(toSet^.length);
      leftIndex  := leftIndex^.next;
      rightIndex := rightIndex^.next;

    ELSIF (order = less) THEN
      leftIndex  := leftIndex^.next;

    ELSE
      rightIndex := rightIndex^.next;
    END (*--if*);
  END (*--while*);
END Intersection;
(*--------------------*)


(*
Difference computes the set containing all members of the left set that are not members
of the right set, e.g.,

x IN toSet iff (x IN left) & Â(x IN right)

The algorithm, similar to that given above for union and intersection, loops over the
items of the left and right sets until the end of either is reached.  On each iteration, the
items are compared for the ordering relation between them.  Equal items are skipped and
both indexes advanced, otherwise the index to the smaller item is advanced.  When the
item from the left set is less than that of the right set we know that it is not present in
the right set and can then copy that item over to the target set.  The last step is to copy
the remaining items, if any, from the left set to the destination set.
*)

PROCEDURE Difference    (    left    : Set         (*-- in    *);
                             right   : Set         (*-- in    *);
                         VAR toSet   : Set         (*-- inout *));

VAR   leftIndex   : Link;        (*-- Loop index over left set *)
      rightIndex  : Link;        (*-- Loop index over right set *)
      toIndex     : Link;        (*-- List of target set nodes *)
      compareItem : CompareProc; (*-- Item comparison routine *)
      order       : Relation;    (*-- Ordering relation between items *)

BEGIN
  setError := noerr;
  IF (left = NIL) OR (right = NIL) THEN
    RaiseErrIn(difference, undefined);
    RETURN;
  ELSIF (left^.dataID # right^.dataID) THEN
    RaiseErrIn(difference, typeerror);
    RETURN;
  ELSIF ~Recreate(left^.dataID, toSet) THEN
    RETURN;
  END (*--if*);

  WITH toSet^ DO
    compareItem := CompareOf(dataID);
    assignItem  := AssignOf(dataID);
  END (*--with*);

  setRoutine := difference;
  leftIndex  := left^.first;
  rightIndex := right^.first;
  WHILE (leftIndex # NIL) & (rightIndex # NIL) DO
    order := compareItem(leftIndex^.item, rightIndex^.item);

    IF (order = equal) THEN
      leftIndex  := leftIndex^.next;
      rightIndex := rightIndex^.next;

    ELSIF (order = less) THEN
      IF (toSet^.first = NIL) THEN
        toSet^.first := NewNode(leftIndex^.item, NIL);
        toIndex := toSet^.first;
      ELSE
        toIndex^.next := NewNode(leftIndex^.item, NIL);
        toIndex := toIndex^.next;
      END (*--if*);

      IF (setError = overflow) THEN
        RETURN;
      END (*--if*);

      INC(toSet^.length);
      leftIndex  := leftIndex^.next;

    ELSE
      rightIndex := rightIndex^.next;
    END (*--if*);
  END (*--while*);

  CopySubset(leftIndex, toIndex, toSet);
END Difference;
(*--------------------*)


(*
SymDifference computes the set containing all members of the left or right set that are
not members of the both sets, e.g.,

x IN toSet iff (x IN left) ­ (x IN right)

The algorithm, similar to that given above for union and difference, loops over the items
of the left and right sets until the end of either is reached.  On each iteration, the items are
compared for the ordering relation between them.  Equal items are skipped and both
indexes advanced, otherwise the index to the smaller item is advanced.  When the items
between the two sets are unequal we can then copy the smaller of the two items over to
the target set.  The last step is to copy the remaining items, if any, from either the left or
right set, whichever has items remaining, to the target set.
*)

PROCEDURE SymDifference (    left    : Set         (*-- in    *);
                             right   : Set         (*-- in    *);
                         VAR toSet   : Set         (*-- inout *));

VAR   leftIndex   : Link;        (*-- Loop index over left set *)
      rightIndex  : Link;        (*-- Loop index over right set *)
      toIndex     : Link;        (*-- List of target set nodes *)
      tempNode    : Link;        (*-- Temporary node *)
      compareItem : CompareProc; (*-- Item comparison routine *)
      order       : Relation;    (*-- Ordering relation between items *)

BEGIN
  setError := noerr;
  IF (left = NIL) OR (right = NIL) THEN
    RaiseErrIn(symdifference, undefined);
    RETURN;
  ELSIF (left^.dataID # right^.dataID) THEN
    RaiseErrIn(symdifference, typeerror);
    RETURN;
  ELSIF ~Recreate(left^.dataID, toSet) THEN
    RETURN;
  END (*--if*);

  WITH toSet^ DO
    compareItem := CompareOf(dataID);
    assignItem  := AssignOf(dataID);
  END (*--with*);

  setRoutine := symdifference;
  leftIndex  := left^.first;
  rightIndex := right^.first;

  WHILE (leftIndex # NIL) & (rightIndex # NIL) DO
    order := compareItem(leftIndex^.item, rightIndex^.item);

    IF (order = equal) THEN
      leftIndex  := leftIndex^.next;
      rightIndex := rightIndex^.next;

    ELSE
      Allocate(tempNode, SIZE(Node));
      IF (tempNode = NIL) THEN
        RaiseErrIn(symdifference, overflow);
        RETURN;
      END (*--if*);
      INC(toSet^.length);
      IF (order = less) THEN
        tempNode^.item := assignItem(leftIndex^.item);
        leftIndex  := leftIndex^.next;
      ELSE
        tempNode^.item := assignItem(rightIndex^.item);
        rightIndex := rightIndex^.next;
      END (*--if*);

      IF (toSet^.first = NIL) THEN
        toSet^.first := tempNode;
      ELSE
        toIndex^.next := tempNode;
      END (*--if*);

      toIndex := tempNode;
    END (*--if*);
  END (*--while*);

  IF (leftIndex = NIL) THEN
    CopySubset(rightIndex, toIndex, toSet);
  ELSIF (rightIndex = NIL) THEN
    CopySubset(leftIndex, toIndex, toSet);
  END (*--if*);
END SymDifference;
(*--------------------*)



(*
13.2.5		Selectors

IsDefined returns true if the given set is not NIL and false otherwise, which is the simple
test for a defined set object.
*)

PROCEDURE IsDefined     (    theSet  : Set      (*-- in    *))
                                     : BOOLEAN  (*-- out   *);
BEGIN
  RETURN (theSet # NIL);
END IsDefined;
(*--------------------*)


(*
IsEmpty (as always) returns the logical condition as to the state of the set's length,
which if zero indicates an empty set.
*)

PROCEDURE IsEmpty       (    theSet  : Set      (*-- in    *))
                                     : BOOLEAN  (*-- out   *);
BEGIN
  setError := noerr;
  IF (theSet # NIL) THEN
    RETURN (theSet^.length = 0);
  END (*--if*);
  RaiseErrIn(isempty, undefined);
  RETURN TRUE;
END IsEmpty;
(*--------------------*)


PROCEDURE TypeOf        (    theSet  : Set      (*-- in    *))
                                     : TypeID   (*-- out   *);
BEGIN
  setError := noerr;
  IF (theSet # NIL) THEN
    RETURN theSet^.dataID;
  END (*--if*);
  RaiseErrIn(typeof, undefined);
  RETURN NullType;
END TypeOf;
(*--------------------*)


(*
IsEqual must return true if the two given sets each contain the same items.  After
enforcing the required preconditions, the lengths of the sets are compared in a simple test
for inequality.  If the lengths are equal it is possible for the sets to be equal so we loop
over each item of both sets returning false upon encountering a mismatch between two
items.  True is returned if the loop completes without finding any mismatched items. 
Note that because the lengths are equal, both indexes will reach the end of their respective
set simultaneously.
*)

PROCEDURE IsEqual       (    left    : Set      (*-- in    *);
                             right   : Set      (*-- in    *))
                                     : BOOLEAN  (*-- out   *);

VAR   leftIndex : Link; (*-- Loop index over left set items *)
      rightIndex: Link; (*-- Loop index over right set items *)
	  compare   : CompareProc;	(*-- item comparison routine *)

BEGIN
  setError := noerr;
  IF (left # NIL) & (right # NIL) THEN
    IF (left^.dataID = right^.dataID) THEN
      IF (left^.length = right^.length) THEN

		compare   := CompareOf(left^.dataID);
        leftIndex := left^.first;
        rightIndex:= right^.first;
        WHILE (leftIndex # NIL) DO
          IF compare(leftIndex^.item, rightIndex^.item) # equal THEN
            RETURN FALSE;
          END (*--if*);
          leftIndex := leftIndex^.next;
          rightIndex:= rightIndex^.next;
        END (*--while*);
        RETURN TRUE;
      END (*--if*);

    ELSE
      RaiseErrIn(isequal, typeerror);
    END (*--if*);
  ELSE
    RaiseErrIn(isequal, undefined);
  END (*--if*);

  RETURN FALSE;
END IsEqual;
(*----------------------------*)


(*
NumMembers needs to simply return the current set length or for an undefined set return
zero as it is impossible to have any members in such a set.
*)

PROCEDURE NumMembers    (    theSet  : Set         (*-- in    *))
                                     : CARDINAL    (*-- out   *);
BEGIN
  setError := noerr;
  IF (theSet # NIL) THEN
    RETURN theSet^.length;
  END (*--if*);
  RaiseErrIn(nummembers, undefined);
  RETURN 0;
END NumMembers;
(*----------------------------*)


(*
IsAMember seeks to determine whether the given item is a member of the given set by
scanning each of the items in the set in turn.  There are two conditions that could cause
the loop to terminate prior to reaching the last item: (1) the item and a set item match
indicating that the item is a member of the set, and (2) the item is greater than a set item
indicating non-membership since the set items are linearly ordered.  If the end of the loop
is reached and we have not exited with a match then by implication the item is not
present.
*)

PROCEDURE IsAMember     (    theItem : Item        (*-- in    *);
                             theSet  : Set         (*-- in    *))
                                     : BOOLEAN     (*-- out   *);

VAR   index       : Link;        (*-- Loop index over items *)
      compareItem : CompareProc; (*-- Item comparison routine *)

BEGIN
  setError := noerr;
  IF (theSet # NIL) THEN
    WITH theSet^ DO
      compareItem := CompareOf(dataID);
      index := first;
    END (*--with*);
    WHILE (index # NIL) DO
      IF (theItem = index^.item) THEN
        RETURN TRUE;
      ELSIF (compareItem(index^.item, theItem) = greater) THEN
        RETURN FALSE;
      END (*--if*);
      index := index^.next;
    END (*--while*);
  ELSE
    RaiseErrIn(ismember, undefined);
  END (*--if*);

  RETURN FALSE;
END IsAMember;
(*----------------------------*)


(*
IsSubset after ensuring that the required preconditions are met, proceeds to loop through
the items of the left and right sets attempting to determine if every member of the left set
is also a member of the right set.  Because the items of the list are linearly ordered in
ascending sequence, inequality can be determined more quickly than with a completely
unordered set implementation.  When an item of the left set is less than its counterpart in
the right set we can immediately return false knowing that that item is not present in the
right set.  If the left item is greater we know that we must advance the index into the
right set since the item may yet be found further into the list.  When the items are equal
both indexes are advanced.  When the end of the loop has been reached without premature
exit, the left set can only be a subset of the right if we have examined beyond the end of
the left set.
*)

PROCEDURE IsSubset      (    left    : Set         (*-- in    *);
                             right   : Set         (*-- in    *))
                                     : BOOLEAN     (*-- out   *);

VAR   leftIndex   : Link;        (*-- Loop index over left set *)
      rightIndex  : Link;        (*-- Loop index over right set *)
      compareItem : CompareProc; (*-- Item comparison routine *)
      order       : Relation;    (*-- Ordering relation between items *)

BEGIN
  setError := noerr;
  IF (left = NIL) OR (right = NIL) THEN
    RaiseErrIn(issubset, undefined);
    RETURN FALSE;
  ELSIF (left^.dataID # right^.dataID) THEN
    RaiseErrIn(issubset, typeerror);
    RETURN FALSE;
  END (*--if*);

  compareItem:= CompareOf(left^.dataID);
  leftIndex  := left^.first;
  rightIndex := right^.first;

  WHILE (leftIndex # NIL) & (rightIndex # NIL) DO
    order := compareItem(leftIndex^.item, rightIndex^.item);
    IF (order = equal) THEN
      leftIndex  := leftIndex^.next;
      rightIndex := rightIndex^.next;
    ELSIF (order = less) THEN
      RETURN FALSE;
    ELSE
      rightIndex := rightIndex^.next;
    END (*--if*);
  END (*--while*);
  RETURN (leftIndex = NIL);
END IsSubset;
(*----------------------------*)

PROCEDURE IsProperSubset(    left    : Set         (*-- in    *);
                             right   : Set         (*-- in    *))
                                     : BOOLEAN     (*-- out   *);
BEGIN
  RETURN IsSubset(left, right) & (left^.length < right^.length);
END IsProperSubset;
(*----------------------------*)



(*
13.2.6  Iterators

Both LoopOver and Traverse simply loop through the given set's list of items from the
first to the end of the list passing the item to the given processing procedure parameter.
*)

PROCEDURE LoopOver   (    theSet  : Set            (*-- in    *);
                          process : LoopAccessProc (*-- in    *));

VAR   theNode : Link; (*-- Loop index over items *)

BEGIN
  setError := noerr;
  IF (theSet # NIL) THEN
    theNode := theSet^.first;
    WHILE (theNode # NIL) & process(theNode^.item) DO
      theNode := theNode^.next;
    END (*--while*);
  ELSE
    RaiseErrIn(loopover, undefined);
  END (*--if*);
END LoopOver;
(*----------------------------*)

PROCEDURE Traverse   (    theSet  : Set            (*-- in    *);
                          process : AccessProc     (*-- in    *));

VAR   theNode : Link; (*-- Loop index over items *)

BEGIN
  setError := noerr;
  IF (theSet # NIL) THEN
    theNode := theSet^.first;
    WHILE (theNode # NIL) DO
      process(theNode^.item);
      theNode := theNode^.next;
    END (*--while*);
  ELSE
    RaiseErrIn(traverse, undefined);
  END (*--if*);
END Traverse;
(*----------------------------*)



(*
13.2.7		Module Initialization

In the module initialization the local exception handlers array variables are set to default
handlers (ExitOnError) except for the noerr handler which is given the null handler. 
setError is given the value noerr avoiding an undefined state.
*)

BEGIN
  FOR setError := MIN(Exceptions) TO MAX(Exceptions) DO
    handlers[setError] := ExitOnError;
  END (*--for*);
  handlers[noerr] := NullHandler;
  setError := noerr;
END SetSUMI.


(*
References

[1]	A.V. Aho, J.E. Hopcroft, and J.D. Ullman, Data Structures and Algorithms,
	Addison-Wesley, Reading, MA, 1983, pg. 118.
[2]	A.M. Tenenbaum and M.J. Augenstein, Data Structures Using Pascal, Prentice-Hall,
	Englewood Cliffs, NJ 1981.
*)