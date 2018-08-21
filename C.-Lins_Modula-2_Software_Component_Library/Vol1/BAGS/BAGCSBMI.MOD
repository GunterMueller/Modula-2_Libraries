IMPLEMENTATION MODULE BagCSBMI;
(*==============================================================
    Version  : 1.00  30 Apr 1989  C. Lins
    Compiler : TopSpeed Modula-2
    Code Size: R-  bytes
    Component: Monolithic Structure - Bag
               Character Sequential Bounded Managed Iterator

    INTRODUCTION
    This module supports the abstract data type bag for discrete
    values of CHARs.

    REVISION HISTORY
    v1.00  30 Apr 1989  C. Lins
      Initial implementation for TopSpeed Modula-2.

    (C) Copyright 1989 Charles A. Lins
==============================================================*)

FROM JPIStorage IMPORT
    (*--Proc*) Allocate, Deallocate;

FROM CharItems IMPORT
    (*--Type*) Item, Continue;

FROM BagEnum IMPORT
    (*--Type*) Exceptions, Operations, ComponentID;

FROM ErrorHandling IMPORT
    (*--Type*) HandlerProc,
    (*--Proc*) NullHandler, Raise, ExitOnError;

    (*-----------------------*)

TYPE  ItemsArray = ARRAY Item OF CARDINAL;
TYPE  BoundedBag = RECORD
        items : ItemsArray;
      END (*-- BoundedBag *);

TYPE  Bag = POINTER TO BoundedBag;


    (*-----------------------*)

VAR   theEmptyBag : ItemsArray; (*-- Predefined bag, initialized to 0 *)

    (*-----------------------*)
    (*--     EXCEPTIONS    --*)

VAR   bagError   : Exceptions;
VAR   handlers   : ARRAY Exceptions OF HandlerProc;


    (*-----------------------*)

PROCEDURE BagError    ()             : Exceptions (*-- out   *);
BEGIN
  RETURN bagError;
END BagError;
(*----------------------------*)

PROCEDURE GetHandler  (    ofError   : Exceptions  (*-- in    *))
                                     : HandlerProc (*-- out   *);
BEGIN
  RETURN handlers[ofError];
END GetHandler;
(*----------------------------*)

PROCEDURE SetHandler  (    ofError   : Exceptions  (*-- in    *);
                           toHandler : HandlerProc (*-- in    *));
BEGIN
  handlers[ofError] := toHandler;
END SetHandler;
(*----------------------------*)

PROCEDURE RaiseErrIn (    theRoutine : Operations    (*-- in    *);
                          theError   : Exceptions    (*-- in    *));
BEGIN
  bagError := theError;
  Raise(ComponentID + ModuleID, theRoutine, theError, handlers[theError]);
END RaiseErrIn;
(*----------------------------*)

PROCEDURE Recreate   (VAR theBag : Bag     (*-- inout *))
                                 : BOOLEAN (*-- out   *);
BEGIN
  IF (theBag = NullBag) THEN
    theBag := Create();
  END (*--if*);
  RETURN (theBag # NIL);
END Recreate;
(*----------------------------*)


    (*---------------------------------*)
    (*--        CONSTRUCTORS         --*)

PROCEDURE Create        ()           : Bag         (*-- out   *);

VAR   newBag : Bag;

BEGIN
  bagError := noerr;
  Allocate(newBag, SIZE(BoundedBag));
  IF (newBag = NullBag) THEN
	RaiseErrIn(create, overflow);
  ELSE
    newBag^.items := theEmptyBag;
  END (*--if*);
  RETURN newBag;
END Create;
(*----------------------------*)

PROCEDURE Destroy       (VAR theBag    : Bag         (*-- inout *));
BEGIN
  Clear(theBag);
  IF (bagError = noerr) THEN
    Deallocate(theBag, SIZE(BoundedBag));
  END (*--if*);
END Destroy;
(*----------------------------*)

PROCEDURE Clear         (VAR theBag    : Bag         (*-- inout *));
BEGIN
  bagError := noerr;
  IF (theBag # NullBag) THEN
    theBag^.items := theEmptyBag;
  ELSE
    RaiseErrIn(clear, undefined);
  END (*--if*);
END Clear;
(*----------------------------*)

PROCEDURE Assign        (    theBag  : Bag         (*-- in    *);
                         VAR toBag   : Bag         (*-- inout *));
BEGIN 
  bagError := noerr;
  IF (theBag # NullBag) THEN
    IF Recreate(toBag) THEN
      toBag^.items := theBag^.items;
    END (*--if*);
  ELSE
    RaiseErrIn(assign, undefined);
  END (*--if*);
END Assign;
(*----------------------------*)

PROCEDURE Include       (    theItem : Item        (*-- in    *);
       	       	         VAR inBag   : Bag         (*-- inout *));
BEGIN
  bagError := noerr;
  IF (inBag # NullBag) THEN
    WITH inBag^ DO
      IF (items[theItem] < MAX(CARDINAL)) THEN
	INC(items[theItem]);
      ELSE
	RaiseErrIn(include, overflow);
      END (*--if*);
    END (*--with*);
  ELSE
    RaiseErrIn(include, undefined);
  END (*--if*);
END Include;
(*----------------------------*)

PROCEDURE Exclude       (    theItem : Item        (*-- in    *);
       	       	         VAR fromBag : Bag         (*-- inout *));
BEGIN
  bagError := noerr;
  IF (fromBag # NullBag) THEN
    WITH fromBag^ DO
      IF (items[theItem] > MIN(CARDINAL)) THEN
	DEC(items[theItem]);
      ELSE
	RaiseErrIn(exclude, notinbag);
      END (*--if*);
    END (*--with*);
  ELSE
    RaiseErrIn(exclude, undefined);
  END (*--if*);
END Exclude;
(*----------------------------*)

PROCEDURE Union         (    left   : Bag         (*-- in    *);
       	       	             right  : Bag         (*-- in    *);
                         VAR toBag  : Bag         (*-- inout *));

VAR   index : Item; (*-- loop index over items *)

BEGIN
  bagError := noerr;
  IF (left # NullBag) & (right # NullBag) THEN
    IF Recreate(toBag) THEN
      WITH toBag^ DO
        FOR index := MIN(Item) TO MAX(Item) DO
          items[index] := left^.items[index] + right^.items[index];
        END (*--for*);
      END (*--with*);
    END (*--if*);
  ELSE
    RaiseErrIn(union, undefined);
  END (*--if*);
END Union;
(*----------------------------*)

PROCEDURE Intersection  (    left   : Bag         (*-- in    *);
       	       	             right  : Bag         (*-- in    *);
                         VAR toBag  : Bag         (*-- inout *));

VAR   index : Item; (*-- loop index over items *)

BEGIN
  bagError := noerr;
  IF (left # NullBag) & (right # NullBag) THEN
    IF Recreate(toBag) THEN
      WITH toBag^ DO
        FOR index := MIN(Item) TO MAX(Item) DO
          IF (left^.items[index] < right^.items[index]) THEN
            items[index] := left^.items[index];
	  ELSE
            items[index] := right^.items[index];
	  END (*--if*);
        END (*--for*);
      END (*--with*);
    END (*--if*);
  ELSE
    RaiseErrIn(intersection, undefined);
  END (*--if*);
END Intersection;
(*----------------------------*)

PROCEDURE Difference    (    left    : Bag         (*-- in    *);
       	       	             right   : Bag         (*-- in    *);
                         VAR toBag   : Bag         (*-- inout *));

VAR   index : Item; (*-- loop index over items *)

BEGIN
  bagError := noerr;
  IF (left # NullBag) & (right # NullBag) THEN
    IF Recreate(toBag) THEN
      WITH toBag^ DO
        FOR index := MIN(Item) TO MAX(Item) DO
	  IF (right^.items[index] = 0) THEN
	    items[index] := left^.items[index];
	  ELSIF (left^.items[index] > right^.items[index]) THEN
            items[index] := left^.items[index] - right^.items[index];
	  ELSE
	    items[index] := 0;
	  END (*--if*);
        END (*--for*);
      END (*--with*);
    END (*--if*);
  ELSE
    RaiseErrIn(difference, undefined);
  END (*--if*);
END Difference;
(*----------------------------*)

PROCEDURE SymDifference (    left    : Bag         (*-- in    *);
       	       	             right   : Bag         (*-- in    *);
                         VAR toBag   : Bag         (*-- inout *));

VAR   index : Item; (*-- loop index over items *)

BEGIN
  bagError := noerr;
  IF (left # NullBag) & (right # NullBag) THEN
    IF Recreate(toBag) THEN
      WITH toBag^ DO
        FOR index := MIN(Item) TO MAX(Item) DO
	  IF (right^.items[index] = 0) THEN
	    items[index] := left^.items[index];
	  ELSIF (left^.items[index] > right^.items[index]) THEN
            items[index] := left^.items[index] - right^.items[index];
	  ELSE
            items[index] := right^.items[index] - left^.items[index];
	  END (*--if*);
        END (*--for*);
      END (*--with*);
    END (*--if*);
  ELSE
    RaiseErrIn(symdifference, undefined);
  END (*--if*);
END SymDifference;
(*----------------------------*)


    (*---------------------------------*)
    (*--          SELECTORS          --*)

PROCEDURE IsDefined     (    theBag    : Bag         (*-- in    *))
                                       : BOOLEAN     (*-- out   *);
BEGIN
  RETURN theBag # NullBag;
END IsDefined;
(*----------------------------*)

PROCEDURE IsEmpty       (    theBag    : Bag         (*-- in    *))
                                       : BOOLEAN     (*-- out   *);

VAR   index : Item; (*-- loop index over items *)

BEGIN
  bagError := noerr;
  IF (theBag # NullBag) THEN
    WITH theBag^ DO
      FOR index := MIN(Item) TO MAX(Item) DO
        IF (items[index] # 0) THEN
          RETURN FALSE;
        END (*--if*);
      END (*--for*);
    END (*--with*);
  ELSE
    RaiseErrIn(isempty, undefined);
  END (*--if*);
  RETURN TRUE;
END IsEmpty;
(*----------------------------*)

PROCEDURE IsEqual       (    left      : Bag         (*-- in    *);
       	       	       	     right     : Bag         (*-- in    *))
                                       : BOOLEAN     (*-- out   *);

VAR   index : Item;   (*-- loop index over items *)

BEGIN
  bagError := noerr;
  IF (left # NullBag) & (right # NullBag) THEN
    WITH left^ DO
      FOR index := MIN(Item) TO MAX(Item) DO
        IF (items[index] # right^.items[index]) THEN
          RETURN FALSE;
        END (*--if*);
      END (*--for*);
    END (*--with*);
    RETURN TRUE;
  ELSE
    RaiseErrIn(isequal, undefined);
  END (*--if*);
  RETURN FALSE;
END IsEqual;
(*----------------------------*)

PROCEDURE NumMembers    (    theBag  : Bag         (*-- in    *))
                                     : CARDINAL    (*-- out   *);

VAR   index : Item;     (*-- loop index over items *)
      count : CARDINAL; (*-- working sum of items in the bag *)

BEGIN
  bagError := noerr;
  count    := 0;
  IF (theBag # NullBag) THEN
    WITH theBag^ DO
      FOR index := MIN(Item) TO MAX(Item) DO
        INC(count, items[index]);
      END (*--for*);
    END (*--with*);
  ELSE
    RaiseErrIn(nummembers, undefined);
  END (*--if*);
  RETURN count;
END NumMembers;
(*----------------------------*)

PROCEDURE UniqueMembers (    theBag  : Bag         (*-- in    *))
                                     : CARDINAL    (*-- out   *);

VAR   index : Item;     (*-- loop index over items *)
      count : CARDINAL; (*-- working sum of unique items in the bag *)

BEGIN
  bagError := noerr;
  count    := 0;
  IF (theBag # NullBag) THEN
    WITH theBag^ DO
      FOR index := MIN(Item) TO MAX(Item) DO
	IF (items[index] > 0) THEN
          INC(count);
	END (*--if*);
      END (*--for*);
    END (*--with*);
  ELSE
    RaiseErrIn(uniquemembers, undefined);
  END (*--if*);
  RETURN count;
END UniqueMembers;
(*----------------------------*)

PROCEDURE IsAMember     (    theItem : Item        (*-- in    *);
       	       	             theBag  : Bag         (*-- in    *))
                                     : BOOLEAN     (*-- out   *);
BEGIN
  bagError := noerr;
  IF (theBag # NullBag) THEN
    RETURN theBag^.items[theItem] > 0;
  ELSE
    RaiseErrIn(ismember, undefined);
  END (*--if*);
  RETURN FALSE;
END IsAMember;
(*----------------------------*)

PROCEDURE NumberOf      (    theItem : Item        (*-- in    *);
       	       	             theBag  : Bag         (*-- in    *))
                                     : CARDINAL    (*-- out   *);
BEGIN
  bagError := noerr;
  IF (theBag # NullBag) THEN
    RETURN theBag^.items[theItem];
  ELSE
    RaiseErrIn(numberof, undefined);
  END (*--if*);
  RETURN 0;
END NumberOf;
(*----------------------------*)

PROCEDURE IsSubset      (    left    : Bag         (*-- in    *);
                             right   : Bag         (*-- in    *))
                                     : BOOLEAN     (*-- out   *);

VAR   index : Item; (*-- loop index over items *)

BEGIN
  bagError := noerr;
  IF (left # NullBag) & (right # NullBag) THEN
    WITH left^ DO
      FOR index := MIN(Item) TO MAX(Item) DO
        IF (items[index] > right^.items[index]) THEN
          RETURN FALSE;
        END (*--if*);
      END (*--for*);
    END (*--with*);
    RETURN TRUE;
  ELSE
    RaiseErrIn(issubset, undefined);
  END (*--if*);
  RETURN FALSE;
END IsSubset;
(*----------------------------*)

PROCEDURE IsProperSubset (    left   : Bag        (*-- in    *);
       	       	       	      right  : Bag        (*-- in    *))
                                     : BOOLEAN    (*-- out   *);
BEGIN
  RETURN IsSubset(left, right) & ~IsEqual(left, right);
END IsProperSubset;
(*----------------------------*)

PROCEDURE LoopOver      (    theBag  : Bag            (*-- in    *);
       	       	       	     process : LoopAccessProc (*-- in    *));

VAR   index : Item; (*-- loop index over items *)

BEGIN
  bagError := noerr;
  IF (theBag # NullBag) THEN
    WITH theBag^ DO
      FOR index := MIN(Item) TO MAX(Item) DO
        IF (items[index] # 0) THEN
          IF ~process(index, items[index]) THEN
            RETURN; (*-- Premature exit, by command of visiting process *)
          END (*--if*);
        END (*--if*);
      END (*--for*);
    END (*--with*);
  ELSE
    RaiseErrIn(loopover, undefined);
  END (*--if*);
END LoopOver;
(*----------------------------*)

PROCEDURE Traverse      (    theBag  : Bag         (*-- in    *);
       	       	       	     process : AccessProc  (*-- in    *));

VAR   index : Item; (*-- loop index over bitsets *)

BEGIN
  bagError := noerr;
  IF (theBag # NullBag) THEN
    WITH theBag^ DO
      FOR index := MIN(Item) TO MAX(Item) DO
        IF (items[index] # 0) THEN
          process(index, items[index]);
        END (*--if*);
      END (*--for*);
    END (*--with*);
  ELSE
    RaiseErrIn(traverse, undefined);
  END (*--if*);
END Traverse;
(*----------------------------*)


(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
(*~~ MODULE INITIALIZATION  ~~*)

VAR   index : Item; (*-- loop index over items *)

BEGIN
  FOR index := MIN(Item) TO MAX(Item) DO
    theEmptyBag[index] := 0;
  END (*--for*);

  FOR bagError := MIN(Exceptions) TO MAX(Exceptions) DO
    handlers[bagError] := ExitOnError;
  END (*--for*);
  handlers[noerr] := NullHandler;
  bagError := noerr;
END BagCSBMI.
