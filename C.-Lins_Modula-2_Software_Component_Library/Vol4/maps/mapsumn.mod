(*
12.2 Unbounded Map Implementation
*)

IMPLEMENTATION MODULE MapSUMN;
(*==============================================================
    Version  : 2.0 16 Sep 1990 C. Lins
    Compiler : Generic pc Modula-2
    Component: Monolithic Structures - Map (Opaque version)
               Sequential Unbounded Managed Non-Iterator

    INTRODUCTION
    This module provides operations for the bounded map abstract
   data type. This implementation uses an unordered list as
   the underlying representation.

    REVISION HISTORY
       8/26/89     CL      Initial version.
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
12.2.1 Type Declarations
*)

TYPE  MapItem = POINTER TO Pair;

TYPE  Pair = RECORD
        d   : Domain;       (*-- value of the domain *)
        r   : Range;        (*-- mapped to value of the range *)
        next: MapItem;
      END (*--Pair*);

TYPE  UnboundedMap = RECORD
        domain : TypeID;    (*-- data type id for the domain *)
        range  : TypeID;    (*-- data type id for the range *)
        extent : CARDINAL;  (*-- current # of items in map *)
        items  : MapItem;   (*-- head of list of pairs *)
      END (*--UnboundedMap*);

TYPE  Map = POINTER TO UnboundedMap;


(*
12.2.2 Exceptions
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

PROCEDURE Search    (    theMap    : Map        (*--in   *);
                         theItem   : Domain     (*--in   *);
                     VAR theIndex  : MapItem    (*--out  *))
                                   : BOOLEAN    (*--out  *);

VAR   compareDomain : CompareProc; (*-- domain value comparison routine *)

BEGIN
  WITH theMap^ DO
    compareDomain := CompareOf(domain);
    theIndex := items;
  END (*--with*);
  WHILE (theIndex # NIL) DO
    IF compareDomain(theItem, theIndex^.d) = equal THEN
      RETURN TRUE;
    END (*--if*);
    theIndex := theIndex^.next;
  END (*--while*);
  RETURN FALSE;
END Search;
(*-------------------------*)

(*
12.2.3 Constructors
*)

(*
Create attempts to form a new, empty map object. If the allocation is
successful, the data
types of the domain and range are saved, while the extent (the number
of items present in the map) is
initialized to zero. Note that Allocate set the pointer to NIL if
unable to get the specified number of bytes from the system allocation
routine. Complexity O(1).
*)

PROCEDURE Create  (    theDomain : TypeID  (*--in   *);
                       theRange  : TypeID  (*--in   *))
                                 : Map     (*--out  *);

VAR   newMap : Map;

BEGIN
  mapError := noerr;
  Allocate(newMap, SIZE(UnboundedMap));
  IF (newMap = NullMap) THEN
    RaiseErrIn(create, overflow);
  ELSE
    WITH newMap^ DO
      domain := theDomain;
      range  := theRange;
      extent := 0;
      items  := NIL;
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

PROCEDURE Destroy (VAR theMap    : Map     (*--inout*));
BEGIN
  Clear(theMap);
  IF (mapError = noerr) THEN
    Deallocate(theMap, SIZE(UnboundedMap));
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

VAR   freeDomain : DisposeProc; (*--disposal routine for items of Domain*)
      freeRange  : DisposeProc; (*--disposal routine for items of Range*)
      oldItem    : MapItem;

BEGIN
  mapError := noerr;
  IF (theMap = NullMap) THEN
    RaiseErrIn(clear, undefined);
  ELSE
    WITH theMap^ DO
      freeDomain := DisposeOf(domain);
      freeRange  := DisposeOf(range);
      WHILE (items # NIL) DO
        WITH items^ DO
          freeDomain(d);
          freeRange(r);
        END (*--with*);
        oldItem := items;
        items := items^.next;
        Deallocate(oldItem, SIZE(Pair));
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

VAR  index        : MapItem;   (*--loop index over items of Domain*)
     assignDomain : AssignProc; (*--domain items assignment routine*)
     assignRange  : AssignProc; (*--range items assignment routine*)
     newPair      : MapItem;

BEGIN
  mapError := noerr;
  IF (theMap = NullMap) THEN
    RaiseErrIn(assign, undefined);
  ELSIF (theMap = toMap) THEN
    RETURN;
  ELSIF (toMap = NullMap) THEN
    WITH theMap^ DO
      toMap := Create(domain, range);
    END (*--with*);
  ELSE
    Clear(toMap);
    WITH theMap^ DO
      toMap^.domain := domain;
      toMap^.range  := range;
   END (*--with*);
  END (*--if*);

  IF (mapError = noerr) THEN
    WITH theMap^ DO
      assignDomain := AssignOf(domain);
      assignRange  := AssignOf(range);
      index := items;
      WHILE (index # NIL) DO
        Allocate(newPair, SIZE(Pair));
        IF (newPair # NIL) THEN
          newPair^.d := assignDomain(index^.d);
          newPair^.r := assignRange(index^.r);
          newPair^.next := toMap^.items;
          toMap^.items := newPair;
        ELSE
          RaiseErrIn(assign, overflow);
          RETURN;
        END (*--if*);
        index := index^.next;
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

VAR   index : MapItem; (*--loop index over item pairs*)
      free  : DisposeProc;

BEGIN
  mapError := noerr;
  IF (theMap = NullMap) THEN
    RaiseErrIn(bind, undefined);
  ELSIF Search(theMap, theItemIn, index) THEN
    free := DisposeOf(theMap^.range);
    free(index^.r);
    index^.r := toItemIn;
  ELSE
    Allocate(index, SIZE(Pair));
    IF (index = NIL) THEN
      RaiseErrIn(bind, overflow);
    ELSE
      WITH index^ DO
        d := theItemIn;
        r := toItemIn;
        next := theMap^.items;
      END (*--with*);
      WITH theMap^ DO
        INC(extent);
        items := index;
      END (*--with*);
    END (*--if*);
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

VAR  index : MapItem;    (*--loop index over item pairs*)
     prior : MapItem;
     free  : DisposeProc; (*--disposal routine for items*)
     compareDomain : CompareProc;

BEGIN
  mapError := noerr;
  IF (theMap = NullMap) THEN
    RaiseErrIn(unbind, undefined);
  ELSIF (theMap^.extent = 0) THEN
    RaiseErrIn(unbind, notbound);
  ELSE
    WITH theMap^ DO
      compareDomain := CompareOf(domain);
      index := items;
      prior := NIL;
      WHILE (index # NIL) DO
        WITH index^ DO
          IF compareDomain(theItemIn, d) = equal THEN
            free := DisposeOf(domain);
            free(d);
            free := DisposeOf(range);
            free(r);
            IF (prior = NIL) THEN
              items := index^.next;
            ELSE
              prior^.next := index^.next;
            END (*--if*);
            Deallocate(index, SIZE(Pair));
            DEC(extent);
            RETURN;
          END (*--if*);
        END (*--with*);
        prior := index;
        index := index^.next;
      END (*--while*);
    END (*--with*);
    RaiseErrIn(unbind, notbound);
  END (*--if*);
END Unbind;
(*-------------------------*)


(*
12.2.4 Selectors
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

VAR  index : MapItem; (*--loop index over left map items of Domain*)
     jndex : MapItem; (*--loop index over right map items of Domain*)
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
      index := items;
      WHILE (index # NIL) DO
        jndex := right^.items;
        LOOP
          IF (jndex = NIL) THEN
            RETURN FALSE;
          ELSIF compareDomain(index^.d, jndex^.d) = equal THEN
            IF compareRange(index^.r, jndex^.r) = equal THEN
              EXIT (*--loop*);
            ELSE
              RETURN FALSE;
            END (*--if*);
          ELSE
            jndex := jndex^.next;
          END (*--if*);
        END (*--loop*);
        index := index^.next;
      END (*--while*);
    END (*--with*);
    RETURN TRUE;
  END (*--if*);
  RETURN FALSE;
END IsEqual;
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

VAR   index : MapItem; (*--where domain item was found*)

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

VAR   index : MapItem; (*--where domain item was found*)

BEGIN
  mapError := noerr;
  IF (theMap = NullMap) THEN
    RaiseErrIn(boundto, undefined);
  ELSIF Search(theMap, theItem, index) THEN
    RETURN index^.r;
  END (*--if*);
  RETURN NullItem;
END BoundTo;
(*-------------------------*)

PROCEDURE IsBoundTo (    theMap    : Map        (*--in   *);
                         theItem   : Domain     (*--in   *);
                     VAR toItem    : Range      (*--out  *))
                                   : BOOLEAN    (*--out  *);

VAR   index : MapItem; (*--where domain item was found*)

BEGIN
  mapError := noerr;
  IF (theMap = NullMap) THEN
    RaiseErrIn(isboundto, undefined);
  ELSIF Search(theMap, theItem, index) THEN
    toItem := index^.r;
    RETURN TRUE;
  END (*--if*);
  toItem := NullItem;
  RETURN FALSE;
END IsBoundTo;
(*-------------------------*)


(*
12.2.6 Module Initialization

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
END MapSUMN.