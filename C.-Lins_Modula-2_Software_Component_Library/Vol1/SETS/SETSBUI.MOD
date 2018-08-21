IMPLEMENTATION MODULE SetSBUI;
(*==========================================================
    Version  : 1.00  30 Apr 1989  C. Lins
    Compiler : TopSpeed Modula-2
    Code Size: R-  bytes
    Component: Monolithic Structures - Set
               Sequential Bounded Unmanaged Iterator

    REVISION HISTORY
    v1.00  30 Apr 1989  C. Lins
        Initial implementation. Derived from SetSBMI module.

    (C) Copyright 1989 Charles A. Lins
==========================================================*)

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


TYPE  ItemsArray = ARRAY SizeRange OF Item;
TYPE  BoundedSet = RECORD
        dataID : TypeID;     (*-- defined data type *)
        size   : SizeRange;  (*-- defined Set size *)
        length : CARDINAL;   (*-- current Set length, := 0 *)
        items  : ItemsArray; (*-- ordered array [1..size] of Item *)
      END (*-- BoundedSet *);

TYPE  Set = POINTER TO BoundedSet;

    (*--------------------*)

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


PROCEDURE CopySubset (    routine : Operations (*-- in    *);
                          fromSet : Set        (*-- in    *);
                          index   : SizeRange  (*-- in    *);
                      VAR toSet   : Set        (*-- inout *));
BEGIN
  WITH toSet^ DO
    WHILE (index <= fromSet^.length) DO
      IF (length < size) THEN
        INC(length);
        items[length] := fromSet^.items[index];
        INC(index);
      ELSE
        RaiseErrIn(routine, overflow);
        RETURN;
      END (*--if*);
    END (*--while*);
  END (*--with*);
END CopySubset;
(*--------------------*)

PROCEDURE Recreate (    theType : TypeID    (*-- in    *);
                        theSize : SizeRange (*-- in    *);
                    VAR theSet  : Set       (*-- inout *))
                                : BOOLEAN   (*-- out   *);
BEGIN
  IF (theSet # NIL) THEN
    Clear(theSet);
    theSet^.dataID := theType;
  ELSE
    theSet := Create(theSize, theType);
  END (*--if*);
  RETURN (setError = noerr);
END Recreate;
(*--------------------*)


PROCEDURE Create        (    theSize  : SizeRange (*-- in    *);
                             theType  : TypeID    (*-- in    *))
                                      : Set       (*-- out   *);

CONST staticSize = SIZE(BoundedSet) - SIZE(ItemsArray);
CONST itemSize   = SIZE(Item);

VAR   newSet : Set;

BEGIN
  setError := noerr;
  Allocate(newSet, staticSize + itemSize * theSize);
  IF (newSet # NIL) THEN
    WITH newSet^ DO
      size   := theSize;
      dataID := theType;
      length := 0;
    END (*--with*);
    RETURN newSet;
  END (*--if*);
  RaiseErrIn(create, overflow);
  RETURN NullSet;
END Create;
(*--------------------*)

PROCEDURE Destroy       (VAR theSet   : Set       (*-- inout *));

CONST staticSize = SIZE(BoundedSet) - SIZE(ItemsArray);
CONST itemSize   = SIZE(Item);

BEGIN
  Clear(theSet);
  IF (setError = noerr) THEN
    Deallocate(theSet, staticSize + itemSize * theSet^.size);
  END (*--if*);
END Destroy;
(*--------------------*)

PROCEDURE Clear         (VAR theSet   : Set       (*-- inout *));
BEGIN
  setError := noerr;
  IF (theSet # NIL) THEN
    WITH theSet^ DO
      length := 0;
    END (*--with*);
  ELSE
    RaiseErrIn(clear, undefined);
  END (*--if*);
END Clear;
(*--------------------*)

PROCEDURE Assign        (    theSet   : Set       (*-- in    *);
                         VAR toSet    : Set       (*-- inout *));

VAR   index : CARDINAL;   (*-- Loop index over items *)

BEGIN
  setError := noerr;
  IF (theSet # NIL) THEN
    WITH theSet^ DO
      IF Recreate(dataID, size, toSet) THEN
        IF (length <= toSet^.size) THEN
          FOR index := MIN(SizeRange) TO length DO
            toSet^.items[index] := items[index];
          END (*--for*);
          toSet^.length := length;
        ELSE
          RaiseErrIn(assign, overflow);
        END (*--if*);
      END (*--if*);
    END (*--with*);
  ELSE
    RaiseErrIn(assign, undefined);
  END (*--if*);
END Assign;
(*--------------------*)

PROCEDURE Include       (    theItem  : Item      (*-- in    *);
                         VAR inSet    : Set       (*-- inout *));

VAR   index       : CARDINAL;    (*-- Loop index in search for theItem *)
      jndex       : CARDINAL;    (*-- Loop index in shifting items *)
      compareItem : CompareProc; (*-- Item comparison routine *)
      itemOrder   : Relation;    (*-- Relation between items *)

BEGIN
  setError := noerr;
  IF (inSet # NIL) THEN
    WITH inSet^ DO
      compareItem := CompareOf(dataID);
      index := MIN(SizeRange);
      LOOP
        IF (index > length) THEN
          EXIT (*--loop*);
        END (*--if*);

        itemOrder := compareItem(items[index], theItem);
        IF (itemOrder = equal) THEN
          RETURN;
        ELSIF (itemOrder = greater) THEN
          EXIT (*--loop*);
        END (*--if*);

        INC(index);
      END (*--loop*);

      IF (length < size) THEN

        FOR jndex := length TO index BY -1 DO
          items[jndex + 1] := items[jndex];
        END (*--for*);

        INC(length);
        items[index] := theItem;

      ELSE
        RaiseErrIn(include, overflow);
      END (*--if*);
    END (*--with*);
  ELSE
    RaiseErrIn(include, undefined);
  END (*--if*);
END Include;
(*--------------------*)

PROCEDURE Exclude       (    theItem  : Item      (*-- in    *);
                         VAR fromSet  : Set       (*-- inout *));

VAR   index       : CARDINAL;    (*-- Loop index over items *)
      compareItem : CompareProc; (*-- Item comparison routine *)
      itemOrder   : Relation;    (*-- Relation between items *)

BEGIN
  setError := noerr;
  IF (fromSet # NIL) THEN
    WITH fromSet^ DO
      compareItem := CompareOf(dataID);
      index := MIN(SizeRange);
      LOOP
        IF (index > length) THEN
          RETURN;
        END (*--if*);

        itemOrder := compareItem(items[index], theItem);
        IF (itemOrder = equal) THEN
          EXIT (*--loop*);
        ELSIF (itemOrder = greater) THEN
          RETURN;
        END (*--if*);

        INC(index);
      END (*--loop*);

      WHILE (index < length) DO
        INC(index);
        items[index - 1] := items[index];
      END (*--while*);
      DEC(length);

    END (*--with*);
  ELSE
    RaiseErrIn(exclude, undefined);
  END (*--if*);
END Exclude;
(*--------------------*)

PROCEDURE Union         (    left     : Set       (*-- in    *);
                             right    : Set       (*-- in    *);
                         VAR toSet    : Set       (*-- inout *));

VAR   leftIndex   : CARDINAL;    (*-- Loop index over left set *)
      rightIndex  : CARDINAL;    (*-- Loop index over right set *)
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
  ELSIF ~Recreate(left^.dataID, left^.size, toSet) THEN
    RETURN;
  END (*--if*);

  WITH toSet^ DO
    compareItem := CompareOf(dataID);
  END (*--with*);

  leftIndex  := MIN(SizeRange);
  rightIndex := MIN(SizeRange);
  WHILE (leftIndex <= left^.length) & (rightIndex <= right^.length) DO
    order := compareItem(left^.items[leftIndex],
                         right^.items[rightIndex]);
    WITH toSet^ DO
      IF (length < size) THEN
        INC(length);
      ELSE
        RaiseErrIn(union, overflow);
        RETURN;
      END (*--if*);
      IF (order = less) THEN
        items[length] := left^.items[leftIndex];
        INC(leftIndex);
      ELSIF (order = equal) THEN
        items[length] := left^.items[leftIndex];
        INC(leftIndex);
        INC(rightIndex);
      ELSE
        items[length] := right^.items[rightIndex];
        INC(rightIndex);
      END (*--if*);
    END (*--with*);
  END (*--while*);

  IF (leftIndex > left^.length) THEN
    CopySubset(union, right, rightIndex, toSet);
  ELSIF (rightIndex > right^.length) THEN
    CopySubset(union, left, leftIndex, toSet);
  END (*--if*);
END Union;
(*--------------------*)

PROCEDURE Intersection  (    left    : Set         (*-- in    *);
                             right   : Set         (*-- in    *);
                         VAR toSet   : Set         (*-- inout *));

VAR   leftIndex   : CARDINAL;    (*-- Loop index over left set *)
      rightIndex  : CARDINAL;    (*-- Loop index over right set *)
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
  ELSIF ~Recreate(left^.dataID, left^.size, toSet) THEN
    RETURN;
  END (*--if*);

  WITH toSet^ DO
    compareItem := CompareOf(dataID);
  END (*--with*);

  leftIndex  := MIN(SizeRange);
  rightIndex := MIN(SizeRange);
  WHILE (leftIndex <= left^.length) & (rightIndex <= right^.length) DO
    order := compareItem(left^.items[leftIndex],
                         right^.items[rightIndex]);
    IF (order = equal) THEN
      WITH toSet^ DO
        IF (length < size) THEN
          INC(length);
          items[length] := left^.items[leftIndex];
        ELSE
          RaiseErrIn(intersection, overflow);
          RETURN;
        END (*--if*);
      END (*--with*);
      INC(leftIndex);
      INC(rightIndex);
    ELSIF (order = less) THEN
      INC(leftIndex);
    ELSE
      INC(rightIndex);
    END (*--if*);
  END (*--while*);
END Intersection;
(*--------------------*)

PROCEDURE Difference    (    left    : Set         (*-- in    *);
                             right   : Set         (*-- in    *);
                         VAR toSet   : Set         (*-- inout *));

VAR   leftIndex   : CARDINAL;    (*-- Loop index over left set *)
      rightIndex  : CARDINAL;    (*-- Loop index over right set *)
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
  ELSIF ~Recreate(left^.dataID, left^.size, toSet) THEN
    RETURN;
  END (*--if*);

  WITH toSet^ DO
    compareItem := CompareOf(dataID);
  END (*--with*);

  leftIndex  := MIN(SizeRange);
  rightIndex := MIN(SizeRange);
  WHILE (leftIndex <= left^.length) & (rightIndex <= right^.length) DO
    order := compareItem(left^.items[leftIndex],
                         right^.items[rightIndex]);
    IF (order = equal) THEN
      INC(leftIndex);
      INC(rightIndex);
    ELSIF (order = less) THEN
      WITH toSet^ DO
        IF (length < size) THEN
          INC(length);
          items[length] := left^.items[leftIndex];
        ELSE
          RaiseErrIn(difference, overflow);
          RETURN;
        END (*--if*);
      END (*--with*);
      INC(leftIndex);
    ELSE
      INC(rightIndex);
    END (*--if*);
  END (*--while*);

  CopySubset(difference, left, leftIndex, toSet);
END Difference;
(*--------------------*)

PROCEDURE SymDifference (    left    : Set         (*-- in    *);
                             right   : Set         (*-- in    *);
                         VAR toSet   : Set         (*-- inout *));

VAR   leftIndex   : CARDINAL;    (*-- Loop index over left set *)
      rightIndex  : CARDINAL;    (*-- Loop index over right set *)
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
  ELSIF ~Recreate(left^.dataID, left^.size, toSet) THEN
    RETURN;
  END (*--if*);

  WITH toSet^ DO
    compareItem := CompareOf(dataID);
  END (*--with*);

  leftIndex  := MIN(SizeRange);
  rightIndex := MIN(SizeRange);
  WHILE (leftIndex <= left^.length) & (rightIndex <= right^.length) DO
    order := compareItem(left^.items[leftIndex],
                         right^.items[rightIndex]);
    IF (order = equal) THEN
      INC(leftIndex);
      INC(rightIndex);
    ELSE
      WITH toSet^ DO
        IF (length < size) THEN
          INC(length);
        ELSE
          RaiseErrIn(symdifference, overflow);
          RETURN;
        END (*--if*);
        IF (order = less) THEN
          items[length] := left^.items[leftIndex];
          INC(leftIndex);
        ELSE
          items[length] := right^.items[rightIndex];
          INC(rightIndex);
        END (*--if*);
      END (*--with*);
    END (*--if*);
  END (*--while*);

  IF (leftIndex > left^.length) THEN
    CopySubset(symdifference, right, rightIndex, toSet);
  ELSIF (rightIndex > right^.length) THEN
    CopySubset(symdifference, left, leftIndex, toSet);
  END (*--if*);
END SymDifference;
(*--------------------*)


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

PROCEDURE SizeOf        (    theSet  : Set      (*-- in    *))
                                     : CARDINAL (*-- out   *);
BEGIN
  setError := noerr;
  IF (theSet # NIL) THEN
    RETURN theSet^.size;
  END (*--if*);
  RaiseErrIn(sizeof, undefined);
  RETURN 0;
END SizeOf;
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

VAR   index : CARDINAL; (*-- Loop index over items *)

BEGIN
  setError := noerr;
  IF (left # NIL) & (right # NIL) THEN
    IF (left^.dataID = right^.dataID) THEN
      IF (left^.length = right^.length) THEN
        FOR index := MIN(SizeRange) TO left^.length DO
          IF (left^.items[index] # right^.items[index]) THEN
            RETURN FALSE;
          END (*--if*);
        END (*--for*);
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

VAR   index       : CARDINAL;    (*-- Loop index over items *)
      compareItem : CompareProc; (*-- Item comparison routine *)
      order       : Relation;    (*-- Ordering relation between items *)

BEGIN
  setError := noerr;
  IF (theSet # NIL) THEN
    WITH theSet^ DO
      compareItem := CompareOf(dataID);
      FOR index := MIN(SizeRange) TO length DO
        IF (theItem = items[index]) THEN
          RETURN TRUE;
        ELSE
          order := compareItem(items[index], theItem);
          IF (order = greater) THEN
            RETURN FALSE;
          END (*--if*);
        END (*--if*);
      END (*--for*);
    END (*--with*);
  ELSE
    RaiseErrIn(ismember, undefined);
  END (*--if*);

  RETURN FALSE;
END IsAMember;
(*----------------------------*)

PROCEDURE IsSubset      (    left    : Set         (*-- in    *);
                             right   : Set         (*-- in    *))
                                     : BOOLEAN     (*-- out   *);

VAR   leftIndex   : CARDINAL;    (*-- Loop index over left set *)
      rightIndex  : CARDINAL;    (*-- Loop index over right set *)
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
  leftIndex  := MIN(SizeRange);
  rightIndex := MIN(SizeRange);
  WHILE (leftIndex <= left^.length) & (rightIndex <= right^.length) DO
    order := compareItem(left^.items[leftIndex],
                         right^.items[rightIndex]);
    IF (order = equal) THEN
      INC(leftIndex);
      INC(rightIndex);
    ELSIF (order = less) THEN
      RETURN FALSE;
    ELSE
      INC(rightIndex);
    END (*--if*);
  END (*--while*);
  RETURN (leftIndex > left^.length);
END IsSubset;
(*----------------------------*)

PROCEDURE IsProperSubset(    left    : Set         (*-- in    *);
                             right   : Set         (*-- in    *))
                                     : BOOLEAN     (*-- out   *);
BEGIN
  RETURN IsSubset(left, right) & (left^.length < right^.length);
END IsProperSubset;
(*----------------------------*)


PROCEDURE LoopOver   (    theSet  : Set            (*-- in    *);
                          process : LoopAccessProc (*-- in    *));

VAR   index : CARDINAL; (*-- Loop index over items *)

BEGIN
  setError := noerr;
  IF (theSet # NIL) THEN
    WITH theSet^ DO
      FOR index := MIN(SizeRange) TO length DO
        IF ~process(items[index]) THEN
          RETURN;
        END (*--if*);
      END (*--for*);
    END (*--with*);
  ELSE
    RaiseErrIn(loopover, undefined);
  END (*--if*);
END LoopOver;
(*----------------------------*)

PROCEDURE Traverse   (    theSet  : Set            (*-- in    *);
                          process : AccessProc     (*-- in    *));

VAR   index : CARDINAL; (*-- Loop index over items *)

BEGIN
  setError := noerr;
  IF (theSet # NIL) THEN
    WITH theSet^ DO
      FOR index := MIN(SizeRange) TO length DO
        process(items[index]);
      END (*--for*);
    END (*--with*);
  ELSE
    RaiseErrIn(traverse, undefined);
  END (*--if*);
END Traverse;
(*----------------------------*)


BEGIN
  FOR setError := MIN(Exceptions) TO MAX(Exceptions) DO
    handlers[setError] := ExitOnError;
  END (*--for*);
  handlers[noerr] := NullHandler;
  setError := noerr;
END SetSBUI.
