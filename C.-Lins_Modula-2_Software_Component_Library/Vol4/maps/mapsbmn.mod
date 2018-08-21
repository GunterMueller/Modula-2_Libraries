(*
11.2 Bounded Map Implementation
*)

IMPLEMENTATION MODULE MapSBMN;
(*==============================================================
    Version  : 2.0 16 Sep 1990 C. Lins
    Compiler : Generic pc Modula-2
    Component: Monolithic Structures - Map (Opaque version)
               Sequential Bounded Managed Non-Iterator

    INTRODUCTION
    This module provides operations for the bounded map abstract
   data type. This implementation uses an unordered array as
   the underlying representation.

    REVISION HISTORY
    v1.01 16 May 1988  C. Lins
      Initial implementation for TML Modula-2.
    v1.02 18 Apr 1989  C. Lins
      Added use of component id constant.
       8/27/89     CL      Free range value in Bind.
       8/26/89     CL      Add SizeOf selector.
   v2.00  16 Sep 1990  C. Lins
     Created generic pc version
   (C) Copyright 1990 Charles A. Lins
===============================================================*)

FROM SCLStorage IMPORT
    (*--Proc*) Allocate, Deallocate;

FROM Items IMPORT
   (*--Cons*) NullItem,
   (*--Type*) Item, AssignProc, CompareProc, DisposeProc;

FROM Relations IMPORT
   (*--type*) Relation;

FROM ErrorHandling IMPORT
    (*--Type*) HandlerProc,
    (*--Proc*) Raise, NullHandler, ExitOnError;

FROM TypeManager IMPORT
    (*--Cons*) NullType,
    (*--Type*) TypeID,
    (*--Proc*) AssignOf, CompareOf, DisposeOf;

FROM MapTypes IMPORT
    (*--Type*) Exceptions, Operations, ComponentID;


    (*-----------------------*)

(*
5.2.1  Type Declarations
*)

TYPE  Pair = RECORD
        d : Domain;     (*-- value of the domain *)
        r : Range;      (*-- mapped to value of the range *)
      END (*--Pair*);

TYPE  MapItems = ARRAY MapSize OF Pair;

TYPE  BoundedMap = RECORD
        domain : TypeID;   (*-- data type id for the domain *)
        range  : TypeID;   (*-- data type id for the range *)
        size   : CARDINAL; (*-- maximum # of items in map *)
        extent : CARDINAL; (*-- current # of items in map *)
        items  : MapItems; (*-- array [1..size] *)
      END (*--BoundedMap*);

TYPE  Map = POINTER TO BoundedMap;


(*
5.2.2  Exceptions
*)

VAR   mapError : Exceptions;
VAR   handler  : ARRAY Exceptions OF HandlerProc;

PROCEDURE MapError   ()              : Exceptions  (*--out  *);
BEGIN
  RETURN mapError;
END MapError;
(*-------------------------*)

PROCEDURE GetHandler (    theError   : Exceptions  (*--in   *))
                                     : HandlerProc (*--out  *);
BEGIN
  RETURN handler[theError];
END GetHandler;
(*-------------------------*)

PROCEDURE SetHandler (    theError   : Exceptions  (*--in   *);
                          theHandler : HandlerProc (*--in   *));
BEGIN
  handler[theError] := theHandler;
END SetHandler;
(*-------------------------*)

PROCEDURE RaiseErrIn (    theRoutine : Operations (*--in   *);
                          theError   : Exceptions (*--in   *));
BEGIN
  mapError := theError;
  Raise(ComponentID + ModuleID, theRoutine, theError, handler[theError]);
END RaiseErrIn;
(*-------------------------*)

(*
11.3.3 Local Routines

Search implements a linear search on the items array of a map. The
routine is used by the map operations Unbind and IsBound to
determine if a given item of the domain is present in the unordered
array and return its array index, if any. Complexity O(n).
*)

PROCEDURE Search   (    theMap     : Map       (*--in   *);
                        theDomain  : Domain    (*--in   *);
                    VAR theIndex   : CARDINAL  (*--out  *))
                                   : BOOLEAN   (*--out  *);

VAR      compare : CompareProc; (*-- domain value comparison routine *)

BEGIN
  WITH theMap^ DO
    compare := CompareOf(domain);
    theIndex:= MIN(MapSize);
    WHILE (theIndex <= extent) DO
      IF compare(theDomain, items[theIndex].d) = equal THEN
        RETURN TRUE;
      END (*--if*);
      INC(theIndex);
    END (*--while*);
  END (*--with*);
  RETURN FALSE;
END Search;
(*-------------------------*)


(*
5.2.3  Constructors
*)

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

PROCEDURE Create  (    theDomain : TypeID  (*--in   *);
                       theRange  : TypeID  (*--in   *);
                       theSize   : MapSize (*--in   *))
                                 : Map     (*--out  *);

CONST baseSize = SIZE(BoundedMap) - SIZE(MapItems);
CONST pairSize = SIZE(Pair);

VAR   newMap : Map;

BEGIN
  mapError := noerr;
  Allocate(newMap, baseSize + pairSize * theSize);
  IF (newMap = NullMap) THEN
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
Destroy is the inverse of Create, making a Map ÔundefinedÕ. To
simplify matters we use Clear to test for an undefined map and
to remove all existing domain/range pairs from the map, if any.
If an error does not occur then we may dispose of the map object
itself. Note that the version of Deallocate given here, automatically
disposes of the proper amount of space (i.e., that which was
originally allocated to the object. Complexity O(n).
*)

PROCEDURE Destroy (VAR theMap    : Map     (*--inout*));
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
Clear removes all domain/range pairs from the map, if any. The data
types of the domain and range are used to retrieve the dynamic memory
deallocation routines, if any, associated with the domain and range.
This allows us to store arbitrarily complex structures within the map.
Complexity O(n).
*)

PROCEDURE Clear   (    theMap    : Map     (*--inout*));

VAR   index      : CARDINAL;    (*--loop index over item pairs*)
      freeDomain : DisposeProc; (*--disposal routine for items of Domain*)
      freeRange  : DisposeProc; (*--disposal routine for items of Range*)

BEGIN
  mapError := noerr;
  IF (theMap = NullMap) THEN
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
map. If the source map is not empty, it is cleared of its contents
and its domain/range data types are altered to match those of the source
map. This step is called recreating the target map. Once completed without
error, the assignment takes place using a simple loop over the number of
items in the source map. Complexity O(m+n).
*)

PROCEDURE Assign  (    theMap    : Map     (*--in   *);
                   VAR toMap     : Map     (*--inout*));

VAR  index        : CARDINAL;   (*--loop index over items of Domain*)
     assignDomain : AssignProc; (*--domain items assignment routine*)
     assignRange  : AssignProc; (*--range items assignment routine*)

BEGIN
  mapError := noerr;
  IF (theMap = NullMap) THEN
    RaiseErrIn(assign, undefined);
  ELSIF (theMap = toMap) THEN
    RETURN;
  ELSIF (toMap = NullMap) THEN
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
Because the array is unordered, the only approach is to use a sequential
search to find the desired item, if it is present. This is similar to
the second approach discussed for the ordered array representation. The
complexity is O(n).
*)

PROCEDURE Bind    (    theMap    : Map     (*--inout*);
                       theItemIn : Domain  (*--in   *);
                       toItemIn  : Range   (*--in   *));

VAR  index : CARDINAL; (*--where domain value was found*)
     free  : DisposeProc;

BEGIN
  mapError := noerr;
  IF (theMap = NullMap) THEN
    RaiseErrIn(bind, undefined);
  ELSIF (theMap^.extent = theMap^.size) THEN
    RaiseErrIn(bind, overflow);
  ELSIF Search(theMap, theItemIn, index) THEN
    free := DisposeOf(theMap^.range);
    free(theMap^.items[index].r);
    theMap^.items[index].r := toItemIn;
  ELSE
    WITH theMap^ DO
      INC(extent);
      WITH items[extent] DO
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
then the map is left unchanged. Because the array of items is unordered
a sequential search must be used to determine if the item is indeed present
in the array. If the item is found it is overwritten with the last item
in the array (recovering dynamically allocated space, if any), the count
of items is decremented, and the routine is exited immediately. Thus, if
the end of the FOR loop is reached, the given domain item is not bound to
any value of the range.
Complexity O(n).
*)

PROCEDURE Unbind  (    theMap    : Map     (*--inout*);
                       theItemIn : Domain  (*--in   *));

VAR  index : CARDINAL;    (*--where domain value was found*)
     free  : DisposeProc; (*--disposal routine for items*)

BEGIN
  mapError := noerr;
  IF (theMap = NullMap) THEN
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
      items[index] := items[extent];
      DEC(extent);
    END (*--with*);
  END (*--if*);
END Unbind;
(*-------------------------*)


(*
5.2.4  Selectors
*)

(*
IsDefined simply treats a NIL map as the representation for a map
that has not been created. Complexity O(1).
*)

PROCEDURE IsDefined (    theMap    : Map        (*--in   *))
                                   : BOOLEAN    (*--out  *);
BEGIN
  RETURN (theMap # NullMap);
END IsDefined;
(*-------------------------*)

(*
IsEmpty returns true if the map contains no domain/range pairs, and
false otherwise. Because we have maintained a count of pairs during
insertions and deletions this state can be determined quickly. Thus
we have complexity on the order of O(1).
*)

PROCEDURE IsEmpty   (    theMap    : Map        (*--in   *))
                                   : BOOLEAN    (*--out  *);
BEGIN
  mapError := noerr;
  IF (theMap = NullMap) THEN
    RaiseErrIn(isempty, undefined);
    RETURN TRUE;
  END (*--if*);
  RETURN theMap^.extent = 0;
END IsEmpty;
(*-------------------------*)

(*
IsEqual determines if the left and right mappings contain the same
domain/range bindings. Initially, we avoid undefined maps [section
10.4.n of our specification] and mismatched domain data types, in both
cases signaling an exception and returning false. Next the trivial
case where the maps have different numbers of elements is handled
by avoiding the comparison loop.

As the arrays are unordered the whole of the right map must be searched
for each item of the left map. Thus, this version of IsEqual has a
complexity on the order of O(n**2). Of course, if this operation is
not used, or used only infrequently, by the client, this may be immaterial.
*)

PROCEDURE IsEqual   (    left      : Map        (*--in   *);
                         right     : Map        (*--in   *))
                                   : BOOLEAN    (*--out  *);

VAR  index : CARDINAL; (*--loop index over left map items of Domain*)
     jndex : CARDINAL; (*--loop index over right map items of Domain*)
     compareDomain : CompareProc; (*--domain comparison routine*)
     compareRange  : CompareProc; (*--range comparison routine*)

BEGIN
  mapError := noerr;
  IF (left = NullMap) OR (right = NullMap) THEN
    RaiseErrIn(isequal, undefined);
  ELSIF (left^.domain # right^.domain) OR
        (left^.range # right^.range) THEN
    RaiseErrIn(isequal, typeerror);

  ELSIF (left^.extent = right^.extent) THEN
    WITH left^ DO
      compareDomain := CompareOf(domain);
      compareRange  := CompareOf(range);
      FOR index := MIN(MapSize) TO extent DO
        jndex := MIN(MapSize);
        LOOP
          IF (jndex > extent) THEN
            RETURN FALSE;
          ELSIF compareDomain(items[index].d, right^.items[jndex].d) = equal THEN
            IF compareRange(items[index].r, right^.items[jndex].r) = equal THEN
              EXIT (*--loop*);
            ELSE
              RETURN FALSE;
            END (*--if*);
          ELSE
            INC(jndex);
          END (*--if*);
        END (*--loop*);
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

PROCEDURE ExtentOf  (    theMap    : Map        (*--in   *))
                                   : CARDINAL   (*--out  *);
BEGIN
  mapError := noerr;
  IF (theMap = NullMap) THEN
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

PROCEDURE DomainOf  (    theMap    : Map        (*--in   *))
                                   : TypeID     (*--out  *);
BEGIN
  mapError := noerr;
  IF (theMap = NullMap) THEN
    RaiseErrIn(domainof, undefined);
    RETURN NullType;
  END (*--if*);
  RETURN theMap^.domain;
END DomainOf;
(*-------------------------*)

PROCEDURE RangeOf   (    theMap    : Map        (*--in   *))
                                   : TypeID     (*--out  *);
BEGIN
  mapError := noerr;
  IF (theMap = NullMap) THEN
    RaiseErrIn(rangeof, undefined);
    RETURN NullType;
  END (*--if*);
  RETURN theMap^.range;
END RangeOf;
(*-------------------------*)

(*
IsBound, BoundTo, and IsBoundTo all use a sequential search to determine
the presence of the given item of the domain. This must be done as an
unordered array is used for the internal map representation. Each routine
returns the appropriate value(s) depending on its function result.
Complexity O(n).
*)

PROCEDURE IsBound   (    theMap    : Map        (*--in   *);
                         theItem   : Domain     (*--in   *))
                                   : BOOLEAN    (*--out  *);

VAR   index : CARDINAL;    (*-- index of item if found *)

BEGIN
  mapError := noerr;
  IF (theMap = NullMap) THEN
    RaiseErrIn(isbound, undefined);
    RETURN FALSE;
  END (*--if*);
  RETURN Search(theMap, theItem, index);
END IsBound;
(*-------------------------*)

PROCEDURE BoundTo   (    theMap    : Map        (*--in   *);
                         theItem   : Domain     (*--in   *))
                                   : Range      (*--out  *);

VAR   index : CARDINAL;    (*-- index of item if found *)

BEGIN
  mapError := noerr;
  IF (theMap = NullMap) THEN
    RaiseErrIn(boundto, undefined);
  ELSIF Search(theMap, theItem, index) THEN
    RETURN theMap^.items[index].r;
  END (*--if*);
  RETURN NullItem;
END BoundTo;
(*-------------------------*)

PROCEDURE IsBoundTo (    theMap    : Map        (*--in   *);
                         theItem   : Domain     (*--in   *);
                     VAR toItem    : Range      (*--out  *))
                                   : BOOLEAN    (*--out  *);

VAR   index : CARDINAL;    (*-- index of item if found *)

BEGIN
  mapError := noerr;
  IF (theMap = NullMap) THEN
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
5.2.7 Module Initialization

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