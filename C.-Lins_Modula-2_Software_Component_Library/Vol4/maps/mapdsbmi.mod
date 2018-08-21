(*
4.3 Discrete Map Implementation
*)

IMPLEMENTATION MODULE MapDSBMI;
(*====================================================================
    Version  : 2.0 16 Sep 1990 C. Lins
    Compiler : Generic pc Modula-2
    Component: Monolithic Structures - Map (Opaque version)
               Discrete Sequential Bounded Managed Iterator

    INTRODUCTION
    This module provides operations for the discrete map abstract
   data type.

    REVISION HISTORY
    v1.01a 15 May 1988  C. Lins
      Initial implementation for TML Modula-2.

   v2.00  16 Sep 1990  C. Lins
     Created generic pc version
   (C) Copyright 1990 Charles A. Lins
=====================================================================*)

FROM SYSTEM IMPORT
   (*--Type*) BYTE;

FROM SCLStorage IMPORT
    (*--Proc*) Allocate, Deallocate;

FROM MapTypes IMPORT
    (*--Type*) Exceptions, Operations;

FROM ErrorHandling IMPORT
    (*--Type*) HandlerProc,
    (*--Proc*) Raise, NullHandler, ExitOnError;

FROM TypeManager IMPORT
    (*--Type*) TypeID;


    (*-----------------------*)

(*
4.3.1  Type Declarations
*)

TYPE  Map = POINTER TO DiscreteMap;

TYPE  MapItems = ARRAY Domain OF Range;

TYPE  DiscreteMap = RECORD
        domain : DomainType;
        range  : RangeType;
        size   : INTEGER;
        extent : INTEGER;
        items  : MapItems;
      END (*--DiscreteMap*);


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
4.3.2  Local Routines
*)

PROCEDURE ValidDomain (    theItem   : Domain     (*-- in    *);
                           theDomain : DomainType (*-- in    *))
                                     : BOOLEAN    (*-- out   *);
BEGIN
  WITH theDomain DO
    RETURN (first <= theItem) & (theItem <= last);
  END (*--with*);
END ValidDomain;
(*-------------------------*)

PROCEDURE ValidRange  (    theItem   : Domain     (*-- in    *);
                           theRange  : RangeType  (*-- in    *))
                                     : BOOLEAN    (*-- out   *);
BEGIN
  WITH theRange DO
    RETURN (first <= theItem) & (theItem <= last);
  END (*--with*);
END ValidRange;
(*-------------------------*)

PROCEDURE AdjustDomain (    theItem   : Domain     (*-- in    *);
                            theDomain : DomainType (*-- in    *))
                                      : Item       (*-- out   *);
BEGIN
  RETURN theItem - theDomain.first; (*-- force zero-based *)
END AdjustDomain;
(*-------------------------*)


(*---------------------------------*)
(*--        CONSTRUCTORS         --*)

PROCEDURE Create  (    theDomain : DomainType (*-- in    *);
                       theRange  : RangeType  (*-- in    *))
                                 : Map        (*-- out   *);

CONST baseSize = SIZE(DiscreteMap) - SIZE(MapItems);
CONST byteSize = SIZE(Item);

VAR   newMap  : Map;
VAR   theSize : INTEGER;

BEGIN
  mapError := noerr;
  IF theDomain.first > theDomain.last THEN
    RaiseErrIn(create, outofdomain);
    newMap := NullMap;
  ELSIF theRange.first > theRange.last THEN
    RaiseErrIn(create, outofrange);
    newMap := NullMap;
  ELSE
    WITH theDomain DO
      theSize := last - first + 1;
    END (*--with*);
    Allocate(newMap, baseSize + byteSize * theSize);
    IF (newMap = NIL) THEN
      RaiseErrIn(create, overflow);
    ELSE
      WITH newMap^ DO
        domain := theDomain;
        range  := theRange;
        size   := theSize;
      END (*--with*);
      Clear(newMap);
    END (*--if*);
  END (*--if*);
  RETURN newMap;
END Create;
(*-------------------------*)

PROCEDURE Destroy (VAR theMap    : Map     (*-- inout *));
CONST baseSize = SIZE(DiscreteMap) - SIZE(MapItems);
CONST byteSize = SIZE(Item);
BEGIN
  mapError := noerr;
  IF (theMap = NIL) THEN
    RaiseErrIn(destroy, undefined);
  ELSE
    Deallocate(theMap, baseSize + byteSize * theMap^.size);
  END (*--if*);
END Destroy;
(*-------------------------*)

PROCEDURE Clear   (VAR theMap    : Map     (*-- inout *));

VAR   index : INTEGER; (*-- loop index over items of Domain *)

BEGIN
  mapError := noerr;
  IF (theMap = NIL) THEN
    RaiseErrIn(clear, undefined);
  ELSE
    WITH theMap^ DO
      FOR index := MIN(Item) TO size-1 DO
        items[index] := range.undefined;
      END (*--for*);
      extent := 0;
    END (*--with*);
  END (*--if*);
END Clear;
(*-------------------------*)

PROCEDURE Assign  (    theMap    : Map     (*-- in    *);
                   VAR toMap     : Map     (*-- inout *));

VAR   index : INTEGER; (*-- loop index over items of Domain *)

BEGIN
  mapError := noerr;
  IF (theMap = NIL) THEN
    RaiseErrIn(assign, undefined);
  ELSIF (theMap = toMap) THEN
    RETURN;
  ELSIF (toMap = NIL) THEN
    toMap := Create(theMap^.domain, theMap^.range);
  ELSE
    Destroy(toMap);
    toMap := Create(theMap^.domain, theMap^.range);
  END (*--if*);
  
  IF (mapError = noerr) THEN
    WITH theMap^ DO
      FOR index := 0 TO size-1 DO
       toMap^.items[index] := items[index];
      END (*--for*);
      toMap^.extent := extent;
    END (*--with*);
  END (*--if*);
END Assign;
(*-------------------------*)

PROCEDURE Bind    (VAR theMap    : Map     (*-- inout *);
                       theItemIn : Domain  (*-- in    *);
                       toItemIn  : Range   (*-- in    *));

VAR   adjustedDomain : Item; (*-- numeric equivalent of Domain *)

BEGIN
  mapError := noerr;
  IF (theMap = NIL) THEN
    RaiseErrIn(bind, undefined);
  ELSIF ~ValidDomain(theItemIn, theMap^.domain) THEN
    RaiseErrIn(bind, outofdomain);
  ELSIF ~ValidRange(toItemIn, theMap^.range) THEN
    RaiseErrIn(bind, outofrange);
  ELSE
    WITH theMap^ DO
     adjustedDomain := AdjustDomain(theItemIn, domain);
     IF (items[adjustedDomain] = range.undefined) THEN
       INC(extent);
     END (*--if*);
     items[adjustedDomain] := toItemIn;
   END (*--with*);
  END (*--if*);
END Bind;
(*-------------------------*)

PROCEDURE Unbind  (VAR theMap    : Map     (*-- inout *);
                       theItemIn : Domain  (*-- in    *));

VAR   adjustedDomain : Item; (*-- numeric equivalent of Domain *)

BEGIN
  mapError := noerr;
  IF (theMap = NIL) THEN
    RaiseErrIn(unbind, undefined);
  ELSIF ~ValidDomain(theItemIn, theMap^.domain) THEN
    RaiseErrIn(unbind, outofdomain);
  ELSE
    WITH theMap^ DO
     adjustedDomain := AdjustDomain(theItemIn, domain);
     IF (items[adjustedDomain] # range.undefined) THEN
       DEC(extent);
     END (*--if*);
     items[adjustedDomain] := range.undefined;
   END (*--with*);
  END (*--if*);
END Unbind;
(*-------------------------*)


(*---------------------------------*)
(*--         SELECTORS           --*)

PROCEDURE IsDefined (    theMap    : Map        (*-- in    *))
                                   : BOOLEAN    (*-- out   *);
BEGIN
  RETURN theMap # NIL;
END IsDefined;
(*-------------------------*)

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

PROCEDURE IsEqual   (    left      : Map        (*-- in    *);
                         right     : Map        (*-- in    *))
                                   : BOOLEAN    (*-- out   *);

VAR   index : INTEGER; (*-- loop index over items of Domain *)

BEGIN
  mapError := noerr;
  IF (left = NIL) OR (right = NIL) THEN
    RaiseErrIn(isequal, undefined);
  ELSIF (left^.domain.dataType # right^.domain.dataType) THEN
    RaiseErrIn(isequal, typeerror);
  ELSE
    WITH left^ DO
      FOR index := MIN(Item) TO size-1 DO
        IF (items[index] # right^.items[index]) THEN
          RETURN FALSE;
        END (*--if*);
      END (*--for*);
    END (*--with*);
    RETURN TRUE;
  END (*--if*);
  RETURN FALSE;
END IsEqual;
(*-------------------------*)

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

PROCEDURE DomainOf  (    theMap    : Map        (*-- in    *);
                     VAR theDomain : DomainType (*-- out   *));
BEGIN
  mapError := noerr;
  IF (theMap = NIL) THEN
    RaiseErrIn(domainof, undefined);
  ELSE
    theDomain := theMap^.domain;
  END (*--if*);
END DomainOf;
(*-------------------------*)

PROCEDURE RangeOf   (    theMap    : Map        (*-- in    *);
                     VAR theRange  : RangeType  (*-- out   *));
BEGIN
  mapError := noerr;
  IF (theMap = NIL) THEN
    RaiseErrIn(rangeof, undefined);
  ELSE
    theRange := theMap^.range;
  END (*--if*);
END RangeOf;
(*-------------------------*)


PROCEDURE IsBound   (    theMap    : Map        (*-- in    *);
                         theItem   : Domain     (*-- in    *))
                                   : BOOLEAN    (*-- out   *);
BEGIN
  mapError := noerr;
  IF (theMap = NIL) THEN
    RaiseErrIn(isbound, undefined);
  ELSIF ~ValidDomain(theItem, theMap^.domain) THEN
    RaiseErrIn(isbound, outofdomain);
  ELSE
    WITH theMap^ DO
      RETURN items[AdjustDomain(theItem, domain)] # range.undefined;
    END (*--with*);
  END (*--if*);
  RETURN FALSE;
END IsBound;
(*-------------------------*)

PROCEDURE BoundTo   (    theMap    : Map        (*-- in    *);
                         theItem   : Domain     (*-- in    *))
                                   : Range      (*-- out   *);
BEGIN
  mapError := noerr;
  IF (theMap = NIL) THEN
    RaiseErrIn(boundto, undefined);
  ELSIF ~ValidDomain(theItem, theMap^.domain) THEN
    RaiseErrIn(boundto, outofdomain);
  ELSE
    WITH theMap^ DO
      RETURN items[AdjustDomain(theItem, domain)];
    END (*--with*);
  END (*--if*);
  RETURN theMap^.range.undefined;
END BoundTo;
(*-------------------------*)

PROCEDURE IsBoundTo (    theMap    : Map        (*-- in    *);
                         theItem   : Domain     (*-- in    *);
                     VAR toItem    : Range      (*-- out   *))
                                   : BOOLEAN    (*-- out   *);
BEGIN
  mapError := noerr;
  IF (theMap = NIL) THEN
    RaiseErrIn(isboundto, undefined);
  ELSIF ~ValidDomain(theItem, theMap^.domain) THEN
    RaiseErrIn(isboundto, outofdomain);
  ELSE
    WITH theMap^ DO
      toItem := items[AdjustDomain(theItem, domain)];
      RETURN toItem # range.undefined;
    END (*--with*);
  END (*--if*);
  RETURN FALSE;
END IsBoundTo;
(*-------------------------*)


(*---------------------------------*)
(*--         ITERATORS           --*)

PROCEDURE LoopOver   (    theMap    : Map            (*-- in    *);
                          theProcess: LoopAccessProc (*-- in    *));

VAR   index : Item; (*-- loop index over items of Domain *)
      first : Item; (*-- numeric equivalent of Domain.first *)

BEGIN
  mapError := noerr;
  IF (theMap = NIL) THEN
    RaiseErrIn(loopover, undefined);
  ELSE
    WITH theMap^ DO
      first := domain.first;
      FOR index := MIN(Item) TO VAL(Item, size-1) DO
        IF (items[index] # range.undefined) THEN
          IF ~theProcess(VAL(Domain, index + first), items[index]) THEN
            RETURN;
          END (*--if*);
        END (*--if*);
      END (*--for*);
    END (*--with*);
  END (*--if*);
END LoopOver;
(*-------------------------*)

PROCEDURE LoopChange (    theMap    : Map            (*-- in    *);
                          theProcess: LoopChangeProc (*-- in    *));

VAR   index : Item; (*-- loop index over items of Domain *)
      first : Item; (*-- numeric equivalent of Domain.first *)

BEGIN
  mapError := noerr;
  IF (theMap = NIL) THEN
    RaiseErrIn(loopchange, undefined);
  ELSE
    WITH theMap^ DO
      first := domain.first;
      FOR index := MIN(Item) TO VAL(Item, size-1) DO
        IF (items[index] # range.undefined) THEN
          IF ~theProcess(VAL(Domain, index + first), items[index]) THEN
            RETURN;
          END (*--if*);
        END (*--if*);
      END (*--for*);
    END (*--with*);
  END (*--if*);
END LoopChange;
(*-------------------------*)


PROCEDURE Traverse   (    theMap    : Map            (*-- in    *);
                          theProcess: AccessProc     (*-- in    *));

VAR   index : Item; (*-- loop index over items of Domain *)
      first : Item; (*-- numeric equivalent of Domain.first *)

BEGIN
  mapError := noerr;
  IF (theMap = NIL) THEN
    RaiseErrIn(traverse, undefined);
  ELSE
    WITH theMap^ DO
      first := domain.first;
      FOR index := MIN(Item) TO VAL(Item, size-1) DO
        IF (items[index] # range.undefined) THEN
          theProcess(VAL(Domain, index + first), items[index]);
        END (*--if*);
      END (*--for*);
    END (*--with*);
  END (*--if*);
END Traverse;
(*-------------------------*)

PROCEDURE TravChange (    theMap    : Map            (*-- in    *);
                          theProcess: ChangeProc     (*-- in    *));

VAR   index : Item; (*-- loop index over items of Domain *)
      first : Item; (*-- numeric equivalent of Domain.first *)

BEGIN
  mapError := noerr;
  IF (theMap = NIL) THEN
    RaiseErrIn(travchange, undefined);
  ELSE
    WITH theMap^ DO
      first := domain.first;
      FOR index := MIN(Item) TO VAL(Item, size-1) DO
        IF (items[index] # range.undefined) THEN
          theProcess(VAL(Domain, index + first), items[index]);
        END (*--if*);
      END (*--for*);
    END (*--with*);
  END (*--if*);
END TravChange;
(*-------------------------*)


(*
4.3.7 Module Initialization

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
END MapDSBMI.

(*
References

*)