(*
13.8		SetSUUI Implementation
*)

IMPLEMENTATION MODULE SetSUUI;
(*==========================================================
    Version  : 1.00  30 Apr 1989  C. Lins
    Compiler : TopSpeed Modula-2
    Code Size: R-  bytes
    Component: Monolithic Structures - Set
               Sequential Unbounded Unmanaged Iterator

    INTRODUCTION
    This module implements the unbounded Set abstraction for
    generic Items using a linearly ordered list.

    REVISION HISTORY
    v1.00  30 Apr 1989  C. Lins
        Initial implementation for TopSpeed Modula-2. Derived from
	SetSUMI module.

    (C) Copyright 1989 Charles A. Lins
=============================================================*)

FROM JPIStorage IMPORT
    (*--Proc*) Allocate, Deallocate;

FROM Items IMPORT
    (*--Type*) Item, CompareProc, AccessProc, LoopAccessProc;

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
    (*--Proc*) CompareOf;

    (*--------------------*)

(*
13.8.1		Internal Unbounded Set Representation
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
13.8.2		Exceptions
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
13.8.3		Local Routines
*)

VAR   setRoutine : Operations;  (*-- Routine calling NewNode *)

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
      item := theItem;
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
13.8.4		Constructors
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

PROCEDURE Destroy       (VAR theSet   : Set       (*-- inout *));
BEGIN
  Clear(theSet);
  IF (setError = noerr) THEN
    Deallocate(theSet, SIZE(theSet^));
  END (*--if*);
END Destroy;
(*--------------------*)

PROCEDURE Clear         (VAR theSet   : Set       (*-- inout *));

VAR   theNode  : Link;        (*-- Set node to be deallocated *)

BEGIN
  setError := noerr;
  IF (theSet # NIL) THEN
    WITH theSet^ DO
      WHILE (first # NIL) DO
        theNode := first;
        first   := first^.next;
        Deallocate(theNode, SIZE(theNode^));
      END (*--while*);
  	  length := 0;
    END (*--with*);
  ELSE
    RaiseErrIn(clear, undefined);
  END (*--if*);
END Clear;
(*--------------------*)

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

PROCEDURE Exclude       (    theItem  : Item      (*-- in    *);
                         VAR fromSet  : Set       (*-- inout *));

VAR   current     : Link;        (*-- Loop index over items *)
      previous    : Link;        (*-- Previous node examined *)
      compareItem : CompareProc; (*-- Item comparison routine *)
      itemOrder   : Relation;    (*-- Relation between items *)

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

    (*-- “current” points to the node to be deleted. *)

    IF (previous = NIL) THEN
      fromSet^.first := current^.next;
    ELSE
      previous^.next := current^.next;
    END (*--if*);
    Deallocate(current, SIZE(current^));
    DEC(fromSet^.length);
  ELSE
    RaiseErrIn(exclude, undefined);
  END (*--if*);
END Exclude;
(*--------------------*)

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
      tempNode^.item := leftIndex^.item;
      leftIndex := leftIndex^.next;

    ELSIF (order = equal) THEN
      tempNode^.item := leftIndex^.item;
      leftIndex := leftIndex^.next;
      rightIndex := rightIndex^.next;

    ELSE
      tempNode^.item := rightIndex^.item;
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
        tempNode^.item := leftIndex^.item;
        leftIndex  := leftIndex^.next;
      ELSE
        tempNode^.item := rightIndex^.item;
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
13.8.5		Selectors
*)

PROCEDURE IsDefined     (    theSet  : Set      (*-- in    *))
                                     : BOOLEAN  (*-- out   *);
BEGIN
  RETURN (theSet # NIL);
END IsDefined;
(*--------------------*)

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

PROCEDURE IsEqual       (    left    : Set      (*-- in    *);
                             right   : Set      (*-- in    *))
                                     : BOOLEAN  (*-- out   *);

VAR   leftIndex : Link; (*-- Loop index over left set items *)
      rightIndex: Link; (*-- Loop index over right set items *)

BEGIN
  setError := noerr;
  IF (left # NIL) & (right # NIL) THEN
    IF (left^.dataID = right^.dataID) THEN
      IF (left^.length = right^.length) THEN

        leftIndex := left^.first;
        rightIndex:= right^.first;
        WHILE (leftIndex # NIL) DO
          IF (leftIndex^.item # rightIndex^.item) THEN
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
13.8.6		Iterators
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
13.8.7		Module Initialization
*)

BEGIN
  FOR setError := MIN(Exceptions) TO MAX(Exceptions) DO
    handlers[setError] := ExitOnError;
  END (*--for*);
  handlers[noerr] := NullHandler;
  setError := noerr;
END SetSUUI.
