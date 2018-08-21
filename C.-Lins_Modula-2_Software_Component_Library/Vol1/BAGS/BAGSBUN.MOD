IMPLEMENTATION MODULE BagSBUN;
(*==============================================================
    Version  : 1.00  02 May 1989  C. Lins
    Compiler : TopSpeed Modula-2
    Component: Monolithic Structures - Bag
               Sequential Bounded Unmanaged Non-Iterator
    Code Size: R-  bytes

    INTRODUCTION
    This module provides the Bag ADT for generic Items.
    Implements a Bag using an ordered array.

    REVISION HISTORY
    v1.00  02 May 1989  C. Lins
      Initial implementation derived from BagSBUI module. The
	  TypeID is used for retrieval of an Item comparison routine.

    (C) Copyright 1989 Charles A. Lins
================================================================*)

FROM JPIStorage IMPORT
    (*--Proc*) Allocate, Deallocate;

FROM Items IMPORT
    (*--Type*) Item, CompareProc;

FROM ErrorHandling IMPORT
    (*--Type*) HandlerProc,
    (*--Proc*) Raise, NullHandler, ExitOnError;

FROM Relations IMPORT
    (*--Type*) Relation;

FROM BagEnum IMPORT
    (*--Type*) Exceptions, Operations, ComponentID;

FROM TypeManager IMPORT
    (*--Cons*) NullType,
    (*--Type*) TypeID,
	(*--Proc*) CompareOf;

    (*--------------------*)

TYPE  ItemNode = RECORD
        item : Item;     (*-- the actual value *)
		count: CARDINAL; (*-- number of times the value occurs *)
      END (*-- ItemNode *);

TYPE  ItemsArray = ARRAY BagSize OF ItemNode;
TYPE  BoundedBag = RECORD
		dataID : TypeID;     (*-- defined data type *)
        size   : BagSize;    (*-- defined Bag size *)
        length : CARDINAL;   (*-- current Bag length, := 0 *)
        items  : ItemsArray; (*-- ordered array [1..size] of ItemNode *)
      END (*-- BoundedBag *);

TYPE  Bag = POINTER TO BoundedBag;


    (*---------------------------------*)
    (*--          EXCEPTIONS         --*)

VAR   bagError  : Exceptions;
VAR   handlers  : ARRAY Exceptions OF HandlerProc;


    (*-----------------------*)

PROCEDURE BagError () : Exceptions (*-- out   *);
BEGIN
  RETURN bagError;
END BagError;
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
  bagError := theError;
  Raise(ComponentID + ModuleID, theRoutine, theError, handlers[theError]);
END RaiseErrIn;
(*--------------------*)


    (*---------------------------------*)
    (*--       LOCAL ROUTINES        --*)

PROCEDURE CopySubset (    routine : Operations (*-- in    *);
                          fromBag : Bag        (*-- in    *);
                          index   : BagSize    (*-- in    *);
                      VAR toBag   : Bag        (*-- inout *));
BEGIN
  WITH toBag^ DO
    WHILE (index <= fromBag^.length) DO
      INC(length);
	  WITH items[length] DO
        item  := fromBag^.items[index].item;
	    count := fromBag^.items[index].count;
      END (*--with*);
      INC(index);
    END (*--while*);
  END (*--with*);
END CopySubset;
(*--------------------*)


PROCEDURE MinCount      (    left     : CARDINAL  (*-- in    *);
                             right    : CARDINAL  (*-- in    *))
							          : CARDINAL  (*-- out   *);
BEGIN
  IF (left < right) THEN
    RETURN left;
  END (*--if*);
  RETURN right;
END MinCount;
(*--------------------*)

PROCEDURE Max (    left     : CARDINAL  (*--in   *);
                   right    : CARDINAL  (*--in   *))
							: CARDINAL  (*--out  *);
BEGIN
  IF (left > right) THEN
    RETURN left;
  END (*--if*);
  RETURN right;
END Max;
(*--------------------*)


    (*---------------------------------*)
    (*--        CONSTRUCTORS         --*)

PROCEDURE Create        (    theSize  : BagSize   (*-- in    *);
                             theType  : TypeID    (*-- in    *))
                                      : Bag       (*-- out   *);

CONST staticSize = SIZE(BoundedBag) - SIZE(ItemsArray);
CONST itemSize   = SIZE(ItemNode);

VAR   newBag : Bag;

BEGIN
  bagError := noerr;
  Allocate(newBag, staticSize + itemSize * theSize);
  IF (newBag = NullBag) THEN
    RaiseErrIn(create, overflow);
  ELSE
    WITH newBag^ DO
	  dataID := theType;
      size   := theSize;
      length := 0;
    END (*--with*);
  END (*--if*);
  RETURN newBag;
END Create;
(*--------------------*)

PROCEDURE Destroy       (VAR theBag   : Bag       (*-- inout *));

CONST staticSize = SIZE(BoundedBag) - SIZE(ItemsArray);
CONST itemSize   = SIZE(ItemNode);

BEGIN
  Clear(theBag);
  IF (bagError = noerr) THEN
    Deallocate(theBag, staticSize + itemSize * theBag^.size);
  END (*--if*);
END Destroy;
(*--------------------*)

PROCEDURE Clear         (VAR theBag   : Bag    (*-- inout *));

VAR   index    : CARDINAL;    (*-- Loop index over items *)

BEGIN
  bagError := noerr;
  IF (theBag # NullBag) THEN
  	theBag^.length := 0;
  ELSE
    RaiseErrIn(clear, undefined);
  END (*--if*);
END Clear;
(*--------------------*)

PROCEDURE Assign        (    theBag   : Bag    (*-- in    *);
                         VAR toBag    : Bag    (*-- inout *));

  PROCEDURE Recreate () : BOOLEAN (*--out  *);
  BEGIN
    IF (toBag = NullBag) THEN
      toBag := Create(theBag^.size, theBag^.dataID);
    ELSIF (theBag^.length <= toBag^.size) THEN
      Clear(toBag);
	  toBag^.dataID := theBag^.dataID;
    ELSE
      RaiseErrIn(assign, overflow);
    END (*--if*);
    RETURN (bagError = noerr);
  END Recreate;
  (*--------------------*)

VAR   index : CARDINAL;   (*-- Loop index over items *)

BEGIN
  bagError := noerr;
  IF (theBag # NullBag) THEN
    IF Recreate() THEN
      WITH theBag^ DO
        FOR index := MIN(BagSize) TO length DO
		  WITH items[index] DO
            toBag^.items[index].item := item;
			toBag^.items[index].count:= count;
		  END (*--with*);
        END (*--for*);
        toBag^.length := length;
      END (*--with*);
	END (*--if*);
  ELSE
    RaiseErrIn(assign, undefined);
  END (*--if*);
END Assign;
(*--------------------*)

PROCEDURE Include       (    theItem  : Item   (*-- in    *);
       	       	         VAR inBag    : Bag    (*-- inout *));

VAR   index       : CARDINAL; (*-- Loop index in search for theItem *)
      jndex       : CARDINAL; (*-- Loop index in shifting items *)
	  compareItem : CompareProc; (*-- Item comparison routine *)
	  itemOrder   : Relation;    (*-- Relation between items *)

BEGIN
  bagError := noerr;
  IF (inBag # NullBag) THEN
    WITH inBag^ DO
	  compareItem := CompareOf(dataID);
      index := MIN(BagSize);
      LOOP
	    IF (index > length) THEN
          EXIT (*--loop*);
        END (*--if*);

		itemOrder := compareItem(items[index].item, theItem);

        IF (itemOrder = equal) THEN
		  INC(items[index].count);
          RETURN;

		ELSIF (itemOrder = greater) THEN
          EXIT (*--loop*);
        END (*--if*);

        INC(index);
      END (*--loop*);

      IF (length < size) THEN

        (*-- Make room for new item at the current index *)

        FOR jndex := length TO index BY -1 DO
          items[jndex + 1] := items[jndex];
        END (*--for*);

        (*-- Insert the new item *)

        INC(length);
        WITH items[index] DO
		  item := theItem;
		  count:= 1;
		END (*--with*);

      ELSE
        RaiseErrIn(include, overflow);
      END (*--if*);
    END (*--with*);
  ELSE
    RaiseErrIn(include, undefined);
  END (*--if*);
END Include;
(*--------------------*)

PROCEDURE Exclude       (    theItem  : Item   (*-- in    *);
       	       	         VAR fromBag  : Bag    (*-- inout *));

VAR   index       : CARDINAL;    (*-- Loop index over items *)
      jndex       : CARDINAL;    (*-- Loop index for item deletion *)
	  compareItem : CompareProc; (*-- Item comparison routine *)
	  itemOrder   : Relation;    (*-- Relation between items *)

BEGIN
  bagError := noerr;
  IF (fromBag # NullBag) THEN
    WITH fromBag^ DO
	  compareItem := CompareOf(dataID);
      FOR index := MIN(BagSize) TO length DO

		itemOrder := compareItem(items[index].item, theItem);

        IF (itemOrder = equal) THEN
          IF (items[index].count > 1) THEN
            DEC(items[index].count);
          ELSE

            (*-- Remove the Item at index by shifting all items
              -- above index down one position. Then adjust the length
		      -- to account for the removal of the item. *)

            WHILE (jndex < length) DO
              INC(jndex);
              items[jndex - 1] := items[jndex];
            END (*--while*);
            DEC(length);
          END (*--if*);
		  RETURN;

		ELSIF (itemOrder = greater) THEN
          RETURN;
        END (*--if*);
      END (*--for*);
    END (*--with*);
  ELSE
    RaiseErrIn(exclude, undefined);
  END (*--if*);
END Exclude;
(*--------------------*)

PROCEDURE Union         (    left     : Bag    (*-- in    *);
       	       	             right    : Bag    (*-- in    *);
                         VAR toBag    : Bag    (*-- inout *));

(*-- Compute the Bag containing all members of left and right.
  -- x IN toBag iff (x IN left) OR (x IN right).
  -- The toBag length is used as the running index for adding
  -- the resulting items of the union.
  -- The algorithm used is a variation on the array merge from
  -- reference [1], pg. 414. *)

VAR   leftIndex   : CARDINAL;    (*-- Loop index over left bag *)
      rightIndex  : CARDINAL;    (*-- Loop index over right bag *)
	  compareItem : CompareProc; (*-- Item comparison routine *)
	  order       : Relation;    (*-- Ordering relation between items *)

BEGIN
  bagError := noerr;
  IF (left = NullBag) OR (right = NullBag) THEN
    RaiseErrIn(union, undefined);
	RETURN;
  ELSIF (left^.dataID # right^.dataID) THEN
    RaiseErrIn(union, typeerror);
	RETURN;
  END (*--if*);

  IF (toBag = NullBag) THEN
    WITH left^ DO
  	  IF (length + right^.length > MAX(BagSize)) THEN
	    RaiseErrIn(union, overflow);
	    RETURN;
	  END (*--if*);
      toBag := Create(length + right^.length, dataID);
	  IF (toBag = NullBag) THEN
	    RETURN;
	  END (*--if*);
	END (*--with*);
  ELSIF (left^.length + right^.length > toBag^.size) THEN
	RaiseErrIn(union, overflow);
	RETURN;
  ELSE
    Clear(toBag);
    toBag^.dataID := left^.dataID;
  END (*--if*);

  compareItem := CompareOf(toBag^.dataID);

  leftIndex  := MIN(BagSize);
  rightIndex := MIN(BagSize);
  WHILE (leftIndex <= left^.length) & (rightIndex <= right^.length) DO
    order := compareItem(left^.items[leftIndex].item,
	                     right^.items[rightIndex].item);
	WITH toBag^ DO
	  INC(length);
	  WITH items[length] DO
        IF (order = less) THEN
          item := left^.items[leftIndex].item;
		  count:= left^.items[leftIndex].count;
          INC(leftIndex);
	    ELSIF (order = equal) THEN
          item := left^.items[leftIndex].item;
		  count:= left^.items[leftIndex].count +
		          right^.items[rightIndex].count;
          INC(leftIndex);
          INC(rightIndex);
        ELSE
          item := right^.items[rightIndex].item;
		  count:= right^.items[rightIndex].count;
          INC(rightIndex);
        END (*--if*);
      END (*--with*);
    END (*--with*);
  END (*--while*);

  (*-- Copy remaining items, if any, from either the left bag or
    -- the right bag to the destination bag. *)

  IF (leftIndex > left^.length) THEN
    CopySubset(union, right, rightIndex, toBag);
  ELSIF (rightIndex > right^.length) THEN
    CopySubset(union, left, leftIndex, toBag);
  END (*--if*);
END Union;
(*--------------------*)

PROCEDURE Intersection  (    left    : Bag      (*-- in    *);
       	       	             right   : Bag      (*-- in    *);
                         VAR toBag   : Bag      (*-- inout *));

(*-- Compute the bag containing all members in both left and right.
  -- x IN toBag iff (x IN left) AND (x IN right).
  -- The toBag length is used as the running index for adding
  -- the resulting items of the intersection. The count field for
  -- any item added to the destination is the smaller of the two
  -- count fields from the source bags. *)

VAR   leftIndex   : CARDINAL;    (*-- Loop index over left bag *)
      rightIndex  : CARDINAL;    (*-- Loop index over right bag *)
	  compareItem : CompareProc; (*-- Item comparison routine *)
	  order       : Relation;    (*-- Ordering relation between items *)
	  largerLength: BagSize;	 (*-- Larger length of left & right bags *)

BEGIN
  bagError := noerr;
  IF (left = NullBag) OR (right = NullBag) THEN
    RaiseErrIn(intersection, undefined);
	RETURN;
  ELSIF (left^.dataID # right^.dataID) THEN
    RaiseErrIn(intersection, typeerror);
	RETURN;
  END (*--if*);
  
  largerLength := Max(left^.length, right^.length);
  IF (toBag = NullBag) THEN
    toBag := Create(largerLength, left^.dataID);
	IF (toBag = NullBag) THEN
	  RETURN;
	END (*--if*);

  ELSIF (largerLength > toBag^.size) THEN
	RaiseErrIn(intersection, overflow);
	RETURN;
  ELSE
    Clear(toBag);
    toBag^.dataID := left^.dataID;
  END (*--if*);

  compareItem := CompareOf(toBag^.dataID);

  leftIndex  := MIN(BagSize);
  rightIndex := MIN(BagSize);
  WHILE (leftIndex <= left^.length) & (rightIndex <= right^.length) DO
    order := compareItem(left^.items[leftIndex].item,
	                     right^.items[rightIndex].item);
    IF (order = equal) THEN
	  WITH toBag^ DO
	    INC(length);
		WITH items[length] DO
          item := left^.items[leftIndex].item;
		  count:= MinCount(left^.items[leftIndex].count,
	                       right^.items[rightIndex].count);
	    END (*--with*);
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

PROCEDURE Difference    (    left    : Bag      (*-- in    *);
       	       	             right   : Bag      (*-- in    *);
                         VAR toBag   : Bag      (*-- inout *));

(*-- Compute the bag containing all members of left that are not
  -- members of right. x IN toBag iff (x IN left) & Â(x IN right)
  -- If there is no match, then the item is added to the destination
  -- bag. If there is a match, a positive difference between the
  -- left and right bag counts for the item causes that value to
  -- be used as the count for the new item. *)

VAR   leftIndex   : CARDINAL;    (*-- Loop index over left bag *)
      rightIndex  : CARDINAL;    (*-- Loop index over right bag *)
	  compareItem : CompareProc; (*-- Item comparison routine *)
	  order       : Relation;    (*-- Ordering relation between items *)
	  largerLength: BagSize;	 (*-- Larger length of left & right bags *)

BEGIN
  bagError := noerr;
  IF (left = NullBag) OR (right = NullBag) THEN
    RaiseErrIn(difference, undefined);
	RETURN;
  ELSIF (left^.dataID # right^.dataID) THEN
    RaiseErrIn(difference, typeerror);
	RETURN;
  END (*--if*);
  
  largerLength := Max(left^.length, right^.length);
  IF (toBag = NullBag) THEN
    toBag := Create(largerLength, left^.dataID);
	IF (toBag = NullBag) THEN
	  RETURN;
	END (*--if*);

  ELSIF (largerLength > toBag^.size) THEN
	RaiseErrIn(intersection, overflow);
	RETURN;
  ELSE
    Clear(toBag);
    toBag^.dataID := left^.dataID;
  END (*--if*);

  compareItem := CompareOf(toBag^.dataID);

  leftIndex  := MIN(BagSize);
  rightIndex := MIN(BagSize);
  WHILE (leftIndex <= left^.length) & (rightIndex <= right^.length) DO
    order := compareItem(left^.items[leftIndex].item,
	                     right^.items[rightIndex].item);
    IF (order = equal) THEN
	  IF (left^.items[leftIndex].count >
	      right^.items[rightIndex].count) THEN
	    WITH toBag^ DO
          INC(length);
	      WITH items[length] DO
            item  := left^.items[leftIndex].item;
		    count := left^.items[leftIndex].count - right^.items[rightIndex].count;
          END (*--with*);
        END (*--with*);
	  END (*--if*);
      INC(leftIndex);
      INC(rightIndex);
    ELSIF (order = less) THEN
	  WITH toBag^ DO
        INC(length);
	    WITH items[length] DO
          item  := left^.items[leftIndex].item;
		  count := left^.items[leftIndex].count;
        END (*--with*);
      END (*--with*);
      INC(leftIndex);
    ELSE
      INC(rightIndex);
    END (*--if*);
  END (*--while*);

  (*-- Copy remaining items, if any, from the left bag to the
    -- destination bag. *)

  CopySubset(difference, left, leftIndex, toBag);
END Difference;
(*--------------------*)

PROCEDURE SymDifference (    left    : Bag      (*-- in    *);
       	       	             right   : Bag      (*-- in    *);
                         VAR toBag   : Bag      (*-- inout *));

(*-- Compute the Bag containing all members of left or right that
  -- are not members of both.
  -- x IN toBag iff (x IN left) # (x IN right) *)

VAR   leftIndex   : CARDINAL;    (*-- Loop index over left bag *)
      rightIndex  : CARDINAL;    (*-- Loop index over right bag *)
	  compareItem : CompareProc; (*-- Item comparison routine *)
	  order       : Relation;    (*-- Ordering relation between items *)
	  largerLength: BagSize;	 (*-- Larger length of left & right bags *)

BEGIN
  bagError := noerr;
  IF (left = NullBag) OR (right = NullBag) THEN
    RaiseErrIn(symdifference, undefined);
	RETURN;
  ELSIF (left^.dataID # right^.dataID) THEN
    RaiseErrIn(symdifference, typeerror);
	RETURN;
  END (*--if*);
  
  largerLength := Max(left^.length, right^.length);
  IF (toBag = NullBag) THEN
    toBag := Create(largerLength, left^.dataID);
	IF (toBag = NullBag) THEN
	  RETURN;
	END (*--if*);

  ELSIF (largerLength > toBag^.size) THEN
	RaiseErrIn(symdifference, overflow);
	RETURN;
  ELSE
    Clear(toBag);
    toBag^.dataID := left^.dataID;
  END (*--if*);

  compareItem := CompareOf(toBag^.dataID);

  leftIndex  := MIN(BagSize);
  rightIndex := MIN(BagSize);
  WHILE (leftIndex <= left^.length) & (rightIndex <= right^.length) DO
    order := compareItem(left^.items[leftIndex].item,
	                     right^.items[rightIndex].item);
    IF (order = equal) THEN
	  IF (left^.items[leftIndex].count >
	      right^.items[rightIndex].count) THEN
	    WITH toBag^ DO
          INC(length);
	      WITH items[length] DO
            item  := left^.items[leftIndex].item;
		    count := left^.items[leftIndex].count - right^.items[rightIndex].count;
          END (*--with*);
        END (*--with*);
	  END (*--if*);
      INC(leftIndex);
      INC(rightIndex);
    ELSE
      WITH toBag^ DO
        IF (length < size) THEN
          INC(length);
        ELSE
          RaiseErrIn(symdifference, overflow);
          RETURN;
        END (*--if*);
        IF (order = less) THEN
	      WITH items[length] DO
            item  := left^.items[leftIndex].item;
			count := left^.items[leftIndex].count;
          END (*--with*);
          INC(leftIndex);
        ELSE
	      WITH items[length] DO
            item  := right^.items[rightIndex].item;
			count := right^.items[rightIndex].count;
          END (*--with*);
          INC(rightIndex);
        END (*--if*);
      END (*--with*);
    END (*--if*);
  END (*--while*);

  (*-- Copy remaining items, if any, from either the left set or
    -- the right set to the destination set. *)

  IF (leftIndex > left^.length) THEN
    CopySubset(symdifference, right, rightIndex, toBag);
  ELSIF (rightIndex > right^.length) THEN
    CopySubset(symdifference, left, leftIndex, toBag);
  END (*--if*);
END SymDifference;
(*--------------------*)


    (*---------------------------------*)
    (*--         SELECTORS           --*)

PROCEDURE IsDefined     (    theBag  : Bag      (*-- in    *))
                                     : BOOLEAN  (*-- out   *);
BEGIN
  RETURN theBag # NullBag;
END IsDefined;
(*--------------------*)

PROCEDURE IsEmpty       (    theBag  : Bag      (*-- in    *))
                                     : BOOLEAN  (*-- out   *);
BEGIN
  bagError := noerr;
  IF (theBag # NullBag) THEN
    RETURN (theBag^.length = 0);
  END (*--if*);

  RaiseErrIn(isempty, undefined);
  RETURN TRUE;
END IsEmpty;
(*--------------------*)

PROCEDURE SizeOf        (    theBag  : Bag      (*-- in    *))
                                     : CARDINAL (*-- out   *);
BEGIN
  bagError := noerr;
  IF (theBag # NullBag) THEN
    RETURN theBag^.size;
  END (*--if*);

  RaiseErrIn(sizeof, undefined);
  RETURN 0;
END SizeOf;
(*--------------------*)

PROCEDURE TypeOf        (    theBag  : Bag      (*-- in    *))
                                     : TypeID   (*-- out   *);
BEGIN
  bagError := noerr;
  IF (theBag # NullBag) THEN
    RETURN theBag^.dataID;
  END (*--if*);

  RaiseErrIn(typeof, undefined);
  RETURN NullType;
END TypeOf;
(*--------------------*)

PROCEDURE IsEqual       (    left    : Bag      (*-- in    *);
                             right   : Bag      (*-- in    *))
                                     : BOOLEAN  (*-- out   *);

VAR   index : CARDINAL; (*-- Loop index over items *)

BEGIN
  bagError := noerr;
  IF (left # NullBag) & (right # NullBag) THEN
    IF (left^.dataID = right^.dataID) THEN
      IF (left^.length = right^.length) THEN

(*
Scan both Bags looking for the first mismatch (inequality) indicating that the
Bags are unequal. If the FOR loop completes then the Bags must be equal.
*)
        FOR index := MIN(BagSize) TO left^.length DO
          IF (left^.items[index].item # right^.items[index].item) THEN
            RETURN FALSE;
		  ELSIF (left^.items[index].count # right^.items[index].count) THEN
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

  (*-- the Bags must be unequal, either by having different
    -- lengths, mismatched types, or an undefined Bag exception. *)

  RETURN FALSE;
END IsEqual;
(*----------------------------*)

PROCEDURE NumMembers    (    theBag  : Bag      (*-- in    *))
                                     : CARDINAL (*-- out   *);

VAR   totalCount : CARDINAL;
      index      : CARDINAL;

BEGIN
  bagError   := noerr;
  totalCount := 0;
  IF (theBag # NullBag) THEN
    WITH theBag^ DO
      FOR index := MIN(BagSize) TO length DO
	    INC(totalCount, items[index].count);
	  END (*--for*);
	END (*--with*);
  ELSE
    RaiseErrIn(nummembers, undefined);
  END (*--if*);

  RETURN totalCount;
END NumMembers;
(*----------------------------*)

PROCEDURE UniqueMembers (    theBag  : Bag      (*-- in    *))
                                     : CARDINAL (*-- out   *);
BEGIN
  bagError := noerr;
  IF (theBag # NullBag) THEN
    RETURN theBag^.length;
  END (*--if*);

  RaiseErrIn(uniquemembers, undefined);
  RETURN 0;
END UniqueMembers;
(*----------------------------*)

PROCEDURE IsAMember     (    theItem : Item     (*-- in    *);
       	       	             theBag  : Bag      (*-- in    *))
                                     : BOOLEAN  (*-- out   *);

VAR   index       : CARDINAL;    (*-- Loop index over items *)
	  compareItem : CompareProc; (*-- Item comparison routine *)
	  order       : Relation;    (*-- Ordering relation between items *)

BEGIN
  bagError := noerr;
  IF (theBag # NullBag) THEN
    WITH theBag^ DO
	  compareItem := CompareOf(dataID);
      FOR index := MIN(BagSize) TO length DO
        IF (theItem = items[index].item) THEN
          RETURN TRUE;
		ELSE
	      order := compareItem(items[index].item, theItem);
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

PROCEDURE NumberOf      (    theItem : Item     (*-- in    *);
       	       	             theBag  : Bag      (*-- in    *))
                                     : CARDINAL (*-- out   *);

VAR   index       : CARDINAL;    (*-- Loop index over items *)
	  compareItem : CompareProc; (*-- Item comparison routine *)
	  order       : Relation;    (*-- Ordering relation between items *)

BEGIN
  bagError := noerr;
  IF (theBag # NullBag) THEN
    WITH theBag^ DO
	  compareItem := CompareOf(dataID);
      FOR index := MIN(BagSize) TO length DO
        WITH items[index] DO
          IF (theItem = item) THEN
            RETURN count;
		  ELSE
	        order := compareItem(item, theItem);
            IF (order = greater) THEN
		      RETURN 0;
            END (*--if*);
          END (*--if*);
		END (*--with*);
      END (*--for*);
    END (*--with*);
  ELSE
    RaiseErrIn(numberof, undefined);
  END (*--if*);

  RETURN 0;
END NumberOf;
(*----------------------------*)

PROCEDURE IsSubset      (    left    : Bag      (*-- in    *);
                             right   : Bag      (*-- in    *))
                                     : BOOLEAN  (*-- out   *);

VAR   leftIndex   : CARDINAL;    (*-- Loop index over left bag *)
      rightIndex  : CARDINAL;    (*-- Loop index over right bag *)
	  compareItem : CompareProc; (*-- Item comparison routine *)
	  order       : Relation;    (*-- Ordering relation between items *)

BEGIN
  bagError := noerr;
  IF (left # NullBag) & (right # NullBag) THEN
    IF (left^.dataID # right^.dataID) THEN
      RaiseErrIn(issubset, typeerror);
    ELSE

	  compareItem:= CompareOf(left^.dataID);
	  leftIndex  := MIN(BagSize);
	  rightIndex := MIN(BagSize);
	  WHILE (leftIndex <= left^.length) & (rightIndex <= right^.length) DO
	    order := compareItem(left^.items[leftIndex].item,
	             	        right^.items[rightIndex].item);
	    IF (order = equal) THEN
		  IF (left^.items[leftIndex].count >
		      right^.items[rightIndex].count) THEN
	        RETURN FALSE;
	      END (*--if*);
	      INC(leftIndex);
	      INC(rightIndex);
	    ELSIF (order = less) THEN
	      RETURN FALSE;
	    ELSE
	      INC(rightIndex);
	    END (*--if*);
	  END (*--while*);
	  RETURN (leftIndex > left^.length);
	END (*--if*);
  ELSE
    RaiseErrIn(issubset, undefined);
  END (*--if*);
  RETURN FALSE;
END IsSubset;
(*----------------------------*)

PROCEDURE IsProperSubset(    left    : Bag      (*-- in    *);
       	       	             right   : Bag      (*-- in    *))
                                     : BOOLEAN  (*-- out   *);
BEGIN
  RETURN IsSubset(left, right) & (left^.length < right^.length);
END IsProperSubset;
(*----------------------------*)


(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
(*~~ MODULE INITIALIZATION  ~~*)

BEGIN
  FOR bagError := MIN(Exceptions) TO MAX(Exceptions) DO
    handlers[bagError] := ExitOnError;
  END (*--for*);
  handlers[noerr] := NullHandler;
  bagError := noerr;
END BagSBUN.