(*
5.4 Bounded Map Implementation
*)

IMPLEMENTATION MODULE MapSBMN;
(*====================================================================
    Version  : 2.0 16 Sep 1990 C. Lins
    Compiler : Generic pc Modula-2
    Component: Monolithic Structures - Map (Opaque version)
               Sequential Bounded Managed Non-Iterator

    INTRODUCTION
    This module provides operations for the bounded map abstract
   data type. This implementation uses an ordered array as
   the underlying representation.

    REVISION HISTORY
    v1.00a 16 May 1988  C. Lins
      Initial implementation for TML Modula-2.
       8/27/89     CL      Free range value in Bind.
       8/26/89     CL      Add SizeOf selector.
   v2.00  16 Sep 1990  C. Lins
     Created generic pc version
   (C) Copyright 1990 Charles A. Lins
=====================================================================*)

FROM Relations IMPORT
   (*--Type*) Relation;

FROM Items IMPORT
   (*--Cons*) NullItem,
   (*--Type*) Item, AssignProc, CompareProc, DisposeProc;

FROM SCLStorage IMPORT
    (*--Proc*) Allocate, Deallocate;

FROM MapTypes IMPORT
    (*--Type*) Exceptions, Operations, ComponentID;

FROM ErrorHandling IMPORT
    (*--Type*) HandlerProc,
    (*--Proc*) Raise, NullHandler, ExitOnError;

FROM TypeManager IMPORT
    (*--Cons*) NullType,
    (*--Type*) TypeID,
    (*--Proc*) AssignOf, CompareOf, DisposeOf;


    (*-----------------------*)

(*
5.4.1 Type Declarations

The physical structures used to represent the Map are the same here as
for the unordered array implementation presented in the previous section
(5.2). The difference is in how the Pair records are ordered within the
MapItems array. Here, the pairs are ordered in acsending order based on
domain values. This organization facilitates searching for a given item
of the domain (binary search can be used), with the consequence that
insertions are slower as the proper ordering must be maintained.
*)

TYPE  Pair = RECORD
        d : Domain;
        r : Range;
      END (*--Pair*);

TYPE  MapItems = ARRAY MapSize OF Pair;

TYPE  BoundedMap = RECORD
        domain : TypeID;   (*-- data type id for the domain *)
        range  : TypeID;   (*-- data type id for the range *)
        size   : CARDINAL; (*-- maximum # of items in map *)
        extent : CARDINAL; (*-- current # of items in map *)
        items  : MapItems; (*-- array [1..theSize] *)
      END (*--BoundedMap*);

TYPE  Map = POINTER TO BoundedMap;


(*---------------------------------*)
(*--          EXCEPTIONS         --*)

VAR   mapError : Exceptions;
VAR   handler  : ARRAY Exceptions OF HandlerProc;

PROCEDURE MapError   ()              : Exceptions  (*-- out   *);
BEGIN
  RETURN mapError;
END MapError;
(*-------------------------*)

PROCEDURE GetHandler (    theError   : Exceptions  (*-- in    *))
                                     : HandlerProc (*-- out   *);
BEGIN
  RETURN handler[theError];
END GetHandler;
(*-------------------------*)

PROCEDURE SetHandler (    theError   : Exceptions  (*-- in    *);
                          theHandler : HandlerProc (*-- in    *));
BEGIN
  handler[theError] := theHandler;
END SetHandler;
(*-------------------------*)

PROCEDURE RaiseErrIn (    theRoutine : Operations (*-- in    *);
                          theError   : Exceptions (*-- in    *));
BEGIN
  mapError := theError;
  Raise(ComponentID + ModuleID, theRoutine, theError, handler[theError]);
END RaiseErrIn;
(*-------------------------*)


(*
5.4.3 Local Routines

Search implements a binary search on the items array of a map. The
routine is used by the map operations Unbind and IsBound to quickly
determine if a given item of the domain is present in the ordered
array and return its array index, if any. Note that when the items
array is empty (i.e., extent = zero), the value of high upon
termination of the binary search is also zero. Complexity O(log2 n).
*)

PROCEDURE Search   (    theMap  : Map      (*-- in    *);
                        theItem : Domain   (*-- in    *);
                    VAR atIndex : CARDINAL (*-- out   *))
                                : BOOLEAN  (*-- out   *);

VAR  middle  : CARDINAL;    (*-- indexes for binary search *)
     low     : CARDINAL;
     high    : CARDINAL;
     compare : CompareProc; (*-- domain item comparison routine *)

BEGIN
  WITH theMap^ DO
    compare := CompareOf(domain);
    low     := MIN(MapSize);
    high    := extent;

    WHILE low < high DO
      middle := (low + high) DIV 2;
      IF compare(items[middle].d, theItem) = less THEN
        low := middle + 1;
      ELSE
        high := middle;
      END (*--if*);
     END (*--while*);

    atIndex := high;
    RETURN (high >= MIN(MapSize)) & (items[high].d = theItem);
  END (*--with*);
END Search;
(*-------------------------*)


(*---------------------------------*)
(*--        CONSTRUCTORS         --*)

(*
Create attempts to form a new, empty map object. The routine uses
the standard strategy employed throughout all of the bounded
components presented in the series of allocating an object containing
an array of varying size. If the allocation is successful, the data
types of the domain and range are saved along with the map's maximum
size, while the extent (the number of items present in the map) is
initialized to zero. Note that Allocate set the pointer to NIL if
unable to get the specified number of bytes from the system allocation
routine. Complexity O(1).
*)

PROCEDURE Create  (    theDomain : TypeID  (*-- in    *);
                       theRange  : TypeID  (*-- in    *);
                       theSize   : MapSize (*-- in    *))
                                 : Map     (*-- out   *);

CONST baseSize = SIZE(BoundedMap) - SIZE(MapItems);
CONST pairSize = SIZE(Pair);

VAR   newMap : Map;

BEGIN
  mapError := noerr;
  Allocate(newMap, baseSize + pairSize * theSize);
  IF (newMap = NIL) THEN
   RaiseErrIn(create, overflow);
  ELSE
    WITH newMap^ DO
      domain := theDomain;
      range  := theRange;
      size   := theSize;
      extent := 0;
    END (*--with*);
  END (*--if*);
  RETURN newMap;
END Create;
(*-------------------------*)

(*
Destroy is the inverse of Create, making a Map ‘undefined’. To
simplify matters we use Clear to test for an undefined map and
to remove all existing domain/range pairs from the map, if any.
If an error does not occur then we may dispose of the map object
itself. Note that the version of Deallocate given here, automatically
disposes of the proper amount of space (i.e., that which was
originally allocated to the object. Complexity O(n).
*)

PROCEDURE Destroy (VAR theMap    : Map     (*-- inout *));
CONST baseSize = SIZE(BoundedMap) - SIZE(MapItems);
CONST pairSize = SIZE(Pair);
BEGIN
  Clear(theMap);
  IF (mapError = noerr) THEN
    Deallocate(theMap, baseSize + pairSize * theMap^.size);
  END (*--if*);
END Destroy;
(*-------------------------*)

(*
Clear removes any domain/range pairs from the map, if any. The data
types of the domain and range are used to retrieve the dynamic memory
deallocation routines, if any, associated with the domain and range.
This allows us to store arbitrarily complex structures within the map.
Complexity O(n).
*)

PROCEDURE Clear   (    theMap    : Map     (*-- inout *));

VAR   index      : CARDINAL;    (*-- loop index over item pairs *)
      freeDomain : DisposeProc; (*-- disposal routine for items of Domain *)
      freeRange  : DisposeProc; (*-- disposal routine for items of Range *)

BEGIN
  mapError := noerr;
  IF (theMap = NIL) THEN
    RaiseErrIn(clear, undefined);
  ELSE
    WITH theMap^ DO
      freeDomain := DisposeOf(domain);
      freeRange  := DisposeOf(range);
      FOR index := MIN(MapSize) TO extent DO
        WITH items[index] DO
          freeDomain(d);
          freeRange(r);
        END (*--with*);
      END (*--for*);
      extent := 0;
    END (*--with*);
  END (*--if*);
END Clear;
(*-------------------------*)

(*
Assign duplicates a map object, including the domain/range pairs and
the relationship between them. If the target map, toMap, is undefined
on entry to the routine it is automatically created with the same
domain and range data types, and the same maximum size as the source
map. If the source map is not empty then it is cleared of its contents
and its domain/range data types are altered to match those of the source
map. This step is called recreating the target map. Once completed without
error, the assignment takes place using a simple loop over the number of
items in the source map. Complexity O(m+n).
*)

PROCEDURE Assign  (    theMap    : Map     (*-- in    *);
                   VAR toMap     : Map     (*-- inout *));

VAR  index        : CARDINAL;   (*-- loop index over items of Domain *)
     assignDomain : AssignProc; (*-- domain items assignment routine *)
     assignRange  : AssignProc; (*-- range items assignment routine *)

BEGIN
  mapError := noerr;
  IF (theMap = NIL) THEN
    RaiseErrIn(assign, undefined);
  ELSIF (theMap = toMap) THEN
    RETURN;
  ELSIF (toMap = NIL) THEN
    WITH theMap^ DO
      toMap := Create(domain, range, size);
    END (*--with*);
  ELSIF (theMap^.extent <= toMap^.size) THEN
    Clear(toMap);
    WITH theMap^ DO
      toMap^.domain := domain;
      toMap^.range  := range;
    END (*--with*);
  ELSE
    RaiseErrIn(assign, overflow);
  END (*--if*);

  IF (mapError = noerr) THEN
    WITH theMap^ DO
      assignDomain := AssignOf(domain);
      assignRange  := AssignOf(range);
      FOR index := MIN(MapSize) TO extent DO
        WITH items[index] DO
          toMap^.items[index].d := assignDomain(d);
          toMap^.items[index].r := assignRange(r);
        END (*--with*);
      END (*--for*);
      toMap^.extent := extent;
    END (*--with*);
  END (*--if*);
END Assign;
(*-------------------------*)

(*
Bind attempts to add (or update) the domain/range pair of items to
the given map. If the item of the domain already exists in the map
the item of the range is updated to that given. The algorithm must
therefore be able to detect the insertion of duplicate domain items.
We must also leave the array in an ordered state for future binary
searches to work correctly. There are two approaches that may be used:

1)  use a binary search to determine the presence or absence of the
   given domain item. If the item already exists in the array this
   will be done in O(log n) steps and this is much more efficient
   than a linear sequential search which uses O(n) steps. Only if the
   item is not present in the array do we use Gonnet's insertion into
   an ordered array algorithm [GONN, pg. 31].

2) simply use an ordered sequential search on the array. If the item
   is found, the range is updated with the new value and the search
   terminated. Otherwise, as soon as a domain item is found in the
   array that is greater than the domain item being added, the array
   is expanded by shifting items to make room for the new element.
   The complexity of this approach is on the order of O(n) for both
   insertion and update operations.

The choice between these two methods depends on the number of updates
relative to the number of insertions. If there are many more updates to
the map than insertions, the first approach is clearly better in the long
run. If the map is never updated (only insertions occur) the extra
binary search of the first approach is extraneous work; though the
difference between the two may be insignificant the for small map sizes
supported by this module.

We shall employ the former tactic on the assumption that efficient
updating of the map is desired.

The insertion algorithm below combines shifting items into higher array
indexes while it searches from the end of the array for the proper
location to insert the new domain/range pair. Once the loop terminates
by either (1) finding an element less than the new domain item, or (2)
reaching the front of the array; the new pair can be inserted at the
index'th + 1 location in the array.

Overall complexity O(n log2 n); for update operations O(log2 n).
*)

PROCEDURE Bind    (    theMap    : Map     (*-- inout *);
                       theItemIn : Domain  (*-- in    *);
                       toItemIn  : Range   (*-- in    *));

VAR  index   : CARDINAL;    (*-- insertion loop index *)
     compare : CompareProc; (*-- domain item comparison routine *)
     free    : DisposeProc;

BEGIN
  mapError := noerr;
  IF (theMap = NIL) THEN
    RaiseErrIn(bind, undefined);
  ELSIF (theMap^.extent = theMap^.size) THEN
    RaiseErrIn(bind, overflow);
  ELSIF Search(theMap, theItemIn, index) THEN
    free := DisposeOf(theMap^.range);
    free(theMap^.items[index].r);
    theMap^.items[index].r := toItemIn;
  ELSE
    WITH theMap^ DO
      compare := CompareOf(domain);
      index   := extent;
      INC(extent);
      WHILE (index >= MIN(MapSize)) &
            (compare(items[index].d, theItemIn) = greater) DO
        items[index+1] := items[index];
        DEC(index);
      END (*--while*);
      WITH items[index+1] DO
        d := theItemIn;
        r := toItemIn;
      END (*--with*);
    END (*--with*);
  END (*--if*);
END Bind;
(*-------------------------*)

(*
Unbind searches for and then removes the domain/range pair of items
associated with the given domain item. If the item is not in the map
then the map is left unchanged (though the notbound exception is raised).
Because the array of items is ordered a binary search can be used to
determine if the item is indeed present in the array. If it is, then all
items above the index where the item is found must be shifted down one
position within the array.
Complexity O(n log2 n).
*)

PROCEDURE Unbind  (    theMap    : Map     (*-- inout *);
                       theItemIn : Domain  (*-- in    *));

VAR  index : CARDINAL;    (*-- index of found item, if any *)
     free  : DisposeProc; (*-- disposal routine for items *)

BEGIN
  mapError := noerr;
  IF (theMap = NIL) THEN
    RaiseErrIn(unbind, undefined);
  ELSIF (theMap^.extent = 0) OR ~Search(theMap, theItemIn, index) THEN
    RaiseErrIn(unbind, notbound);
  ELSE
    WITH theMap^ DO
      WITH items[index] DO
        free := DisposeOf(domain);
        free(d);
        free := DisposeOf(range);
        free(r);
      END (*--with*);
      FOR index := index+1 TO extent DO
        items[index-1] := items[index];
      END (*--for*);
      DEC(extent);
    END (*--with*);
  END (*--if*);
END Unbind;
(*-------------------------*)


(*---------------------------------*)
(*--         SELECTORS           --*)

(*
IsDefined simply treats a NIL map as the representation for a map
that has not been created. Complexity O(1).
*)

PROCEDURE IsDefined (    theMap    : Map        (*-- in    *))
                                   : BOOLEAN    (*-- out   *);
BEGIN
  RETURN theMap # NIL;
END IsDefined;
(*-------------------------*)

(*
IsEmpty returns true if the map contains no domain/range pairs, and
false otherwise. Because we have maintained a count of pairs during
insertions and deletions this state can be determined quickly. Thus
we have complexity on the order of O(1).
*)

PROCEDURE IsEmpty   (    theMap    : Map        (*-- in    *))
                                   : BOOLEAN    (*-- out   *);
BEGIN
  mapError := noerr;
  IF (theMap = NIL) THEN
    RaiseErrIn(isempty, undefined);
    RETURN TRUE;
  END (*--if*);
  RETURN theMap^.extent = 0;
END IsEmpty;
(*-------------------------*)

(*
IsEqual determines if the left and right mappings contain the same
domain/range bindings. Initially, we avoid undefined maps [section
3.4.n of our specification] and mismatched domain data types, in both
cases signaling an exception and returning false. Next the trivial
case where the maps have different numbers of elements is handled
by avoiding the comparison loop. Only if the two maps have the same
number of elements do we iterate over the domain/range pairs in each.
As soon as an inequality is encountered we can return false. Otherwise,
if the iteration covers all of the elements then the maps must be equal.
Complexity: O(Min(m,n))
*)

PROCEDURE IsEqual   (    left      : Map        (*-- in    *);
                         right     : Map        (*-- in    *))
                                   : BOOLEAN    (*-- out   *);

VAR   index : CARDINAL; (*-- loop index over map item pairs *)

BEGIN
  mapError := noerr;
  IF (left = NIL) OR (right = NIL) THEN
    RaiseErrIn(isequal, undefined);
  ELSIF (left^.domain # right^.domain) THEN
    RaiseErrIn(isequal, typeerror);

  ELSIF (left^.extent = right^.extent) THEN
    WITH left^ DO
      FOR index := MIN(MapSize) TO extent DO
        IF (items[index].d # right^.items[index].d) OR
           (items[index].r # right^.items[index].r) THEN
          RETURN FALSE;
        END (*--if*);
      END (*--for*);
    END (*--with*);
    RETURN TRUE;
  END (*--if*);
  RETURN FALSE;
END IsEqual;
(*-------------------------*)

(*
SizeOf returns the maximum number of domain/range pairs allowed in the
map object. Undefined maps raise an exception and return zero.
Complexity O(1)
*)

PROCEDURE SizeOf    (    theMap    : Map        (*-- in    *))
                                   : CARDINAL   (*-- out   *);
BEGIN
  mapError := noerr;
  IF (theMap = NIL) THEN
    RaiseErrIn(sizeof, undefined);
    RETURN 0;
  END (*--if*);
  RETURN theMap^.size;
END SizeOf;
(*-------------------------*)

(*
ExtentOf returns the number of domain/range pairs present in the
map object. Undefined maps raise an exception and return zero.
Complexity O(1)
*)

PROCEDURE ExtentOf  (    theMap    : Map        (*-- in    *))
                                   : CARDINAL   (*-- out   *);
BEGIN
  mapError := noerr;
  IF (theMap = NIL) THEN
    RaiseErrIn(extentof, undefined);
    RETURN 0;
  END (*--if*);
  RETURN theMap^.extent;
END ExtentOf;
(*-------------------------*)

(*
DomainOf and RangeOf simply return the data type IDs for the domain
and range, respectively. Undefined maps cause the NullType to be
returned. Complexity O(1)
*)

PROCEDURE DomainOf  (    theMap    : Map        (*-- in    *))
                                   : TypeID     (*-- out   *);
BEGIN
  mapError := noerr;
  IF (theMap = NIL) THEN
    RaiseErrIn(domainof, undefined);
    RETURN NullType;
  END (*--if*);
  RETURN theMap^.domain;
END DomainOf;
(*-------------------------*)

PROCEDURE RangeOf   (    theMap    : Map        (*-- in    *))
                                   : TypeID     (*-- out   *);
BEGIN
  mapError := noerr;
  IF (theMap = NIL) THEN
    RaiseErrIn(rangeof, undefined);
    RETURN NullType;
  END (*--if*);
  RETURN theMap^.range;
END RangeOf;
(*-------------------------*)

(*
IsBound uses the binary search, provided by the routine Search, to
determine the presence of the given item. BoundTo is similar, except
that the bound item of the range is returned. IsBoundTo combines the
facilities provided by both routines. Complexity O(log2 n).
*)

PROCEDURE IsBound   (    theMap    : Map        (*-- in    *);
                         theItem   : Domain     (*-- in    *))
                                   : BOOLEAN    (*-- out   *);

VAR   index : CARDINAL;    (*-- index of item if found *)

BEGIN
  mapError := noerr;
  IF (theMap = NIL) THEN
    RaiseErrIn(isbound, undefined);
    RETURN FALSE;
  END (*--if*);
  RETURN Search(theMap, theItem, index);
END IsBound;
(*-------------------------*)

PROCEDURE BoundTo   (    theMap    : Map        (*-- in    *);
                         theItem   : Domain     (*-- in    *))
                                   : Range      (*-- out   *);

VAR   index : CARDINAL;    (*-- index of item if found *)

BEGIN
  mapError := noerr;
  IF (theMap = NIL) THEN
    RaiseErrIn(boundto, undefined);
  ELSIF Search(theMap, theItem, index) THEN
    RETURN theMap^.items[index].r;
  END (*--if*);
  RETURN NullItem;
END BoundTo;
(*-------------------------*)

PROCEDURE IsBoundTo (    theMap    : Map        (*-- in    *);
                         theItem   : Domain     (*-- in    *);
                     VAR toItem    : Range      (*-- out   *))
                                   : BOOLEAN    (*-- out   *);

VAR   index   : CARDINAL;    (*-- index of item if found *)

BEGIN
  mapError := noerr;
  IF (theMap = NIL) THEN
    RaiseErrIn(isboundto, undefined);
  ELSIF Search(theMap, theItem, index) THEN
    toItem := theMap^.items[index].r;
    RETURN TRUE;
  END (*--if*);
  toItem := NullItem;
  RETURN FALSE;
END IsBoundTo;
(*-------------------------*)


(*
5.4.7 Module Initialization

The module's local variables are initialized to known states.
mapError is used to fill the handlers array with a routine
that will exit the program when an exception is raised (saving the
declaration of a special loop control variable for this purpose).
The condition noerr is given the NullHandler which is presumed to
do nothing.  Applying MIN and MAX to cover all exceptions followed
by resetting the handler for noerr ensures that this initialization
will be unaffected by any future changes to the number of Exceptions
or their order of declaration within the enumeration.  Since a FOR loop
control variable is undefined following the loop, mapError must be
set to indicate that an error has not yet occurred.
*)

BEGIN
  FOR mapError := MIN(Exceptions) TO MAX(Exceptions) DO
    SetHandler(mapError, ExitOnError);
  END (*--for*);
  SetHandler(noerr, NullHandler);
  mapError := noerr;
  NullMap := NIL;
END MapSBMN.