(*
12.3	SetSBMI Implementation
*)


IMPLEMENTATION MODULE SetSBMI;
(*==========================================================
    Version  : 1.00  30 Apr 1989  C. Lins
    Compiler : TopSpeed Modula-2 Compiler
    Code Size: R-  bytes
    Component: Monolithic Structures - Set
               Sequential Bounded Managed Iterator

    INTRODUCTION
    This module provides the implementation of the bounded
    Set abstraction for generic Items using an ordered array.

    REVISION HISTORY
    v1.00  30 Apr 1989  C. Lins
        Initial implementation for TopSpeed Modula-2.

    (C) Copyright 1989 Charles A. Lins
==========================================================*)

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
12.3.1	Internal Bounded Set Representation

«Illustration Here»

Figure 12.1

Like the internal representations for the bounded stack and string, a bounded set is
represented as a pointer to a record containing an array of items of a variable size.  In
addition, the data type ID, physical array size (maximum number of items), and the
current length (number of items currently present in the array).  The representation
invariants that must be enforced by the module are:

1.	MIN(SizeRange) <= size <= MAX(SizeRange)
2.	MIN(SizeRange) <= length <= size
3.	items[x] < items[x+1], for all x such that x < length

The third invariant states that elements of the items array must be linearly ordered in
ascending sequence.
*)

TYPE  ItemsArray = ARRAY SizeRange OF Item;
TYPE  BoundedSet = RECORD
        dataID : TypeID;     (*-- defined data type *)
        size   : SizeRange;  (*-- defined Set size *)
        length : CARDINAL;   (*-- current Set length, := 0 *)
        items  : ItemsArray; (*-- ordered array [1..size] of Item *)
      END (*-- BoundedSet *);

TYPE  Set = POINTER TO BoundedSet;


(*
12.2.2	Exceptions

To support the exception handling mechanism two variables are needed.  The first,
setError, is used to record the exception result from each operation; while handlers  is an
array of exception handling procedures indexed by the exception result.

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
12.3.3	Local Routines


12.3.3.1	CopySubset

Several of the set algorithms require the ability to copy the all remaining items from a
given set to a partially created target set.  CopySubset  performs this operation by
looping through the fromSet starting from the given index to the end of the fromSet,
copying each item to the target set along the way.  If during the copying process the
target set size is insufficient for all of the items from the source set, the overflow
exception is raised and the operation is aborted.  (A more efficient implementation for
checking overflow would be to calculate the number of items to copy from the source
(length - fromIndex + 1) and compare this to the number of available positions remaining
in the target (size - length) and if the result is greater then invoke overflow).


12.3.3.2	Recreate

All routines that accept a target set as an inout parameter need to either (1) clear the
existing set of its contents if the set already exists, or (2) create a new, empty set to be
target of the operation.  The Recreate  routine provides such a capability, returning true if
successful.  It should be noted that failure only occurs as a result when the set must be
created.
*)

PROCEDURE CopySubset (    routine : Operations (*-- in    *);
                          fromSet : Set        (*-- in    *);
                          index   : SizeRange  (*-- in    *);
                      VAR toSet   : Set        (*-- inout *));

VAR   assignItem : AssignProc; (*-- Item assignment routine, if any *)

BEGIN
  assignItem := AssignOf(fromSet^.dataID);
  WITH toSet^ DO
    WHILE (index <= fromSet^.length) DO
      IF (length < size) THEN
        INC(length);
        items[length] := assignItem(fromSet^.items[index]);
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


(*
12.3.4	Constructors

Create begins by clearing the setError field under the assumption of a successful result.

The header for the set must then be allocated in a local variable since the function result
cannot be manipulated but only returned.  The key to this allocation step is the
calculation of the number of bytes necessary based on the size of an individual item and
the number of items requested.  We must not forget the space for storing theSize, the
Type, and the set length.  The constant staticSize  accomplishes this regardless of the
number and size of these “static” fields.  The calculation is unaffected by changes in the
number or size of these fields that may come about due to future maintenance.  If the
bounded set could not be allocated, the overflow exception must be raised, and the NullSet
returned.

At this point, all possibility of failure has been avoided and the bounded set header can be
initialized to its empty state, and the size limit and data type ID  can be stored for this
bounded set.  Lastly, the new set can be returned to the caller.
*)

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


(*
Destroy takes advantage that Clear sets setError to noerr and raises the undefined set
exception.  So if Clear succeeds, Destroy simply releases the allocated set header.
*)

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


(*
Clear sets setError to noerr and checks for an undefined set raising the undefined set
exception if necessary.  After asserting a valid set, the item disposal routine is retrieved
for the set, followed by the deallocation of every item in the set.  Once this has been
taken care of, the set length is adjusted to the empty state.
*)

PROCEDURE Clear         (VAR theSet   : Set       (*-- inout *));

VAR   freeItem : DisposeProc; (*-- Item disposal routine, if any *)
      index    : CARDINAL;    (*-- Loop index over items *)

BEGIN
  setError := noerr;
  IF (theSet # NIL) THEN
    WITH theSet^ DO
      freeItem := DisposeOf(dataID);
      FOR index := MIN(SizeRange) TO length DO
        freeItem(items[index]);
      END (*--for*);
      length := 0;
    END (*--with*);
  ELSE
    RaiseErrIn(clear, undefined);
  END (*--if*);
END Clear;
(*--------------------*)


(*
Assignment for bounded objects is simpler to implement than their unbounded
counterparts since the opportunity for overflow is restricted to when the target object is
being (re-)created.

If the target object exists and is capable of holding all of the source object's items the
target can be safely cleared and its data type updated appropriately.  Otherwise, the
overflow exception is raised and the assignment operation aborted.  When the target object
is initially undefined it must be created using the data type and size attributes of the
source object.  If overflow does not occur, the actual assignment can commence,
otherwise its suffices to exit as Create has already raised the exception.

The assignment operator cannot be used to copy the whole items array as only a slice of
the array's index range was actually allocated and who knows what other dynamically
allocated objects follow it in memory.  Nor can assignment be used to copy individual
items as the data type of those items is unknown; using assignment for dynamically
allocated items would cause structural sharing of items, which is not desired.
*)

PROCEDURE Assign        (    theSet   : Set       (*-- in    *);
                         VAR toSet    : Set       (*-- inout *));

VAR   assignItem : AssignProc; (*-- Item assignment routine, if any *)
      index      : CARDINAL;   (*-- Loop index over items *)

BEGIN
  setError := noerr;
  IF (theSet # NIL) THEN
    WITH theSet^ DO
      IF Recreate(dataID, size, toSet) THEN
        IF (length <= toSet^.size) THEN
          assignItem := AssignOf(dataID);
          FOR index := MIN(SizeRange) TO length DO
            toSet^.items[index] := assignItem(items[index]);
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


(*
Include must add the given item to the set if it is not already a member or to simply exit
if the item is already a member (these semantics are compatible with Modula-2's INCL
operation).  We could use the IsAMember selector except the index where the item is not
found is necessary to insert the item in its appropriate position within the ordered array.
Once we have this position, we make room for the new item by shifting all items above
the index position up by one index position in the array.  Of course, there must be room
for the new item in the set.  Then the new item is inserted into its proper place.
*)

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


(*
Exclude undoes what Include did to add an item to the set; if the item is found then all
items above it in the array are shifted downward one position and the set length is updated
to reflect removal of the item.  If the item is not found the routine simply exits to be
compatible with Modula-2's EXCL operation.
*)

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


(*
Union computes the set containing all members of left and right, e.g.,

x IN toSet iff (x IN left) OR (x IN right).

The algorithm used is a variation on the array merge from Augenstein and Tanenbaum [2,
pg. 414] and the set intersection algorithm presented in Aho, Hopcroft, and Ullman [1,
pg. 117].

The algorithm loops over the items of the left and right sets until the end of either is
reached.  On each iteration, the items are compared for the ordering relation between them.
This is used to determine from which set an item is copied to the target set, and which
indexes to advance.  In this manner, all items are processed only once and duplicate items
in the target set are avoided. The toSet length is used as the running index for adding the
resulting items of the union.

The last step is to copy the remaining items, if any, from either the left set or the right
set to the destination set.
*)

PROCEDURE Union         (    left     : Set       (*-- in    *);
                             right    : Set       (*-- in    *);
                         VAR toSet    : Set       (*-- inout *));

VAR   leftIndex   : CARDINAL;    (*-- Loop index over left set *)
      rightIndex  : CARDINAL;    (*-- Loop index over right set *)
      compareItem : CompareProc; (*-- Item comparison routine *)
      order       : Relation;    (*-- Ordering relation between items *)
      assignItem  : AssignProc;  (*-- Item assignment routine *)

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
    assignItem  := AssignOf(dataID);
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
        items[length] := assignItem(left^.items[leftIndex]);
        INC(leftIndex);
      ELSIF (order = equal) THEN
        items[length] := assignItem(left^.items[leftIndex]);
        INC(leftIndex);
        INC(rightIndex);
      ELSE
        items[length] := assignItem(right^.items[rightIndex]);
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


(*
Intersection computes the set containing all members in both left and right sets, e.g.,

x IN toSet iff (x IN left) AND (x IN right).

The algorithm, similar to the set intersection algorithm given in Aho, Hopcroft, and
Ullman [1, pg. 117] for ordered lists, loops over the items of the left and right sets until
the end of either is reached.  On each iteration, the items are compared for the ordering
relation between them.  Equal items are copied to the target set and both indexes are
advanced, otherwise the index to the smaller item is advanced.  In this manner, all items
are processed only once and duplicate items in the target set are avoided.  The toSet length
is used as the running index for adding the resulting items of the intersection.
*)

PROCEDURE Intersection  (    left    : Set         (*-- in    *);
                             right   : Set         (*-- in    *);
                         VAR toSet   : Set         (*-- inout *));

VAR   leftIndex   : CARDINAL;    (*-- Loop index over left set *)
      rightIndex  : CARDINAL;    (*-- Loop index over right set *)
      compareItem : CompareProc; (*-- Item comparison routine *)
      order       : Relation;    (*-- Ordering relation between items *)
      assignItem  : AssignProc;  (*-- Item assignment routine *)

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
    assignItem  := AssignOf(dataID);
  END (*--with*);

  leftIndex  := MIN(SizeRange);
  rightIndex := MIN(SizeRange);
  WHILE (leftIndex  <= left^.length) & (rightIndex < right^.length) DO
    order := compareItem(left^.items[leftIndex],
                         right^.items[rightIndex]);
    IF (order = equal) THEN
      WITH toSet^ DO
        IF (length < size) THEN
          INC(length);
          items[length] := assignItem(left^.items[leftIndex]);
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


(*
Difference computes the set containing all members of the left set that are not members
of the right set, e.g.,

x IN toSet iff (x IN left) & ¬(x IN right)

The algorithm, similar to the above algorithm for intersection, loops over the items of
the left and right sets until the end of either is reached.  On each iteration, the items are
compared for the ordering relation between them.  Equal items are skipped and both
indexes advanced, otherwise the index to the smaller item is advanced.  When the item
from the left set is less than that of the right set we know that it is not present in the
right set and can copy that item over to the target set.  The toSet length is used as the
running index for adding the resulting items of the difference.  The last step is to copy the
remaining items, if any, from the left set to the destination set.
*)

PROCEDURE Difference    (    left    : Set         (*-- in    *);
                             right   : Set         (*-- in    *);
                         VAR toSet   : Set         (*-- inout *));

VAR   leftIndex   : CARDINAL;    (*-- Loop index over left set *)
      rightIndex  : CARDINAL;    (*-- Loop index over right set *)
      compareItem : CompareProc; (*-- Item comparison routine *)
      order       : Relation;    (*-- Ordering relation between items *)
      assignItem  : AssignProc;  (*-- Item assignment routine *)

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
    assignItem  := AssignOf(dataID);
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
          items[length] := assignItem(left^.items[leftIndex]);
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


(*
SymDifference computes the set containing all members of the left or right set that are
not members of both sets, e.g.,

x IN toSet iff (x IN left) ≠ (x IN right)

The algorithm, similar to that given above for union and difference, loops over the items
of the left and right sets until the end of either is reached.  On each iteration, the items are
compared for the ordering relation between them.  Equal items are skipped and both
indexes advanced, otherwise the index to the smaller item is advanced.  When the items
between the two sets are unequal we can then copy the smaller of the two items over to
the target set.  The toSet length is used as the running index for adding the resulting
items of the symmetric difference.  The last step is to copy the remaining items, if any,
from either the left or right set, whichever has items remaining, to the target set.
*)

PROCEDURE SymDifference (    left    : Set         (*-- in    *);
                             right   : Set         (*-- in    *);
                         VAR toSet   : Set         (*-- inout *));

VAR   leftIndex   : CARDINAL;    (*-- Loop index over left set *)
      rightIndex  : CARDINAL;    (*-- Loop index over right set *)
      compareItem : CompareProc; (*-- Item comparison routine *)
      order       : Relation;    (*-- Ordering relation between items *)
      assignItem  : AssignProc;  (*-- Item assignment routine *)

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
    assignItem  := AssignOf(dataID);
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
          items[length] := assignItem(left^.items[leftIndex]);
          INC(leftIndex);
        ELSE
          items[length] := assignItem(right^.items[rightIndex]);
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



(*
12.3.4		Selectors

IsDefined returns true if the given set is non-NIL, which is the simplest test for a defined
set object.
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


(*
Sizeof simply returns the defined size for the given set or a default value for an undefined
set.  TypeOf is similar except that it deals with the set's data type ID.
*)

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


(*
IsEqual must return true if the two given sets each contain the same items.  After
enforcing the required preconditions, the lengths of the sets are compared in a simple test
for inequality.  If the lengths are equal it is possible for the sets to be equal so we loop
over each item of both sets returning false upon encountering the first inequality between
two items.  True is returned if the loop completes without finding any mismatched items.
*)

PROCEDURE IsEqual       (    left    : Set      (*-- in    *);
                             right   : Set      (*-- in    *))
                                     : BOOLEAN  (*-- out   *);

VAR   index : CARDINAL; (*-- Loop index over items *)
      compare : CompareProc;	(*-- item comparison routine *)

BEGIN
  setError := noerr;
  IF (left # NIL) & (right # NIL) THEN
    IF (left^.dataID = right^.dataID) THEN
      IF (left^.length = right^.length) THEN
	compare := CompareOf(left^.dataID);
        FOR index := MIN(SizeRange) TO left^.length DO
          IF compare(left^.items[index], right^.items[index]) # equal THEN
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


(*
IsSubset after ensuring that the required preconditions are met, proceeds to loop through
the items of the left and right sets attempting to determine if every member of the left set
is also a member of the right set.  Because the items of the array are linearly ordered in
ascending sequence inequality can be determined quicker than with a completely unordered
set implementation.  When an item of the left set is less than its counterpart in the right
set we can immediately return false knowing that that item is not present in the right set.
If the left item is greater then we know that we must advance the index into the right set
since the item may yet be found further into the array.  When the items are equal both
indexes are advanced.  When the end of the loop has been reached without premature exit,
the left set can only be a subset of the right if we have examined beyond the end of the
left set.
*)

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



(*
12.3.6		Iterators

Both iterators are simply loops over each of the items in the given set.  LoopOver may
be terminated before reaching the last item in the set if the access procedure returns false.
*)

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



(*
12.2.7	Module Initialization

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
END SetSBMI.



(*
References

[1]	A. Aho, J. Hopcroft and J. Ullman, Data Structures and Algorithms, Addison-Wesley,
	Reading, MA 1983.
[2]	A.M. Tenenbaum and M.J. Augenstein, Data Structures Using Pascal, Prentice-Hall,
	Englewood Cliffs, NJ 1981.
*)