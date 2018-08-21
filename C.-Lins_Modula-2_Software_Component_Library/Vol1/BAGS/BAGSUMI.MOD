IMPLEMENTATION MODULE BagSUMI;
(*==============================================================
    Version  : 1.00  02 May 1989  C. Lins
    Compiler : TopSpeed Modula-2
    Component: Monolithic Structures - Bag
               Sequential Unbounded Managed Iterator
    Code Size: R-  bytes

    INTRODUCTION
    This module provides the Bag ADT for generic Items. Uses an
	ordered, linear list for the items.

    REVISION HISTORY
    v1.00  02 May 1989  C. Lins
        Initial implementation for TopSpeed Modula-2.

    (C) Copyright 1989 Charles A. Lins
================================================================*)

FROM JPIStorage IMPORT
    (*--Proc*) Allocate, Deallocate;

FROM Items IMPORT
    (*--Type*) Item, AssignProc, CompareProc, DisposeProc;

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
	(*--Proc*) AssignOf, CompareOf, DisposeOf;

    (*--------------------*)

TYPE  Link = POINTER TO Node;
TYPE  Node = RECORD      (*-- a bag item node *)
        item : Item;     (*-- the item nodes' data *)
		count: CARDINAL; (*-- number of these items *)
		next : Link;     (*-- link to next node in list *)
	  END (*-- Node *);
CONST NullLink = NIL;

TYPE  UnboundedBag = RECORD (*-- bag header *)
		dataID : TypeID;    (*-- defined data type *)
        length : CARDINAL;  (*-- current bag length, := 0 *)
        first  : Link;      (*-- link to first Item node *)
      END (*-- UnboundedBag *);

TYPE  Bag = POINTER TO UnboundedBag;


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

VAR   bagRoutine : Operations;  (*-- Routine calling NewNode *)
VAR   assignItem : AssignProc;  (*-- Item assignment routine *)

PROCEDURE NewNode       (    theItem : Item        (*-- in    *);
       	       	             theCount: CARDINAL    (*-- in    *);
       	       	             theNext : Link        (*-- in    *))
							         : Link        (*-- out   *);

VAR   newLink : Link;

BEGIN
  Allocate(newLink, SIZE(Node));
  IF (newLink = NullLink) THEN
    RaiseErrIn(bagRoutine, overflow);
  ELSE
    WITH newLink^ DO
	  item  := assignItem(theItem);
	  count := theCount;
	  next  := theNext;
	END (*--with*);
  END (*--if*);
  RETURN newLink;
END NewNode;
(*--------------------*)

PROCEDURE TailInsert (    theNode : Link (*--in   *);
					  VAR first   : Link (*--inout*);
					  VAR last    : Link (*--inout*));
BEGIN
  IF (first = NullLink) THEN
	first := theNode;
  ELSE
	last^.next := theNode;
  END (*--if*);
  last := theNode;
END TailInsert;
(*--------------------*)

PROCEDURE CopySubset (    fromIndex  : Link       (*-- in    *);
                          toIndex    : Link       (*-- in    *);
                      VAR toBag      : Bag        (*-- inout *));

VAR   tempNode : Link;

BEGIN
  LOOP
    IF (fromIndex = NullLink) THEN
	  EXIT (*--loop*);
	ELSE
	  WITH fromIndex^ DO
	    tempNode := NewNode(item, count, NIL);
	  END (*--with*);
	  IF (tempNode = NullLink) THEN
	    EXIT (*--loop*);
	  END (*--if*);
	  INC(toBag^.length);
	  TailInsert(tempNode, toBag^.first, toIndex);
      fromIndex := fromIndex^.next;
	END (*--if*);
  END (*--loop*);
END CopySubset;
(*--------------------*)

PROCEDURE Recreate (VAR theBag  : Bag       (*--inout*);
                        theType : TypeID    (*--in   *))
                                : BOOLEAN   (*--out  *);
BEGIN
  IF (theBag = NullBag) THEN
    theBag := Create(theType);
  ELSE
    Clear(theBag);
    theBag^.dataID := theType;
  END (*--if*);
  RETURN (bagError = noerr);
END Recreate;
(*--------------------*)


    (*---------------------------------*)
    (*--        CONSTRUCTORS         --*)

PROCEDURE Create        (    theType  : TypeID    (*-- in    *))
                                      : Bag       (*-- out   *);

(*-- Attempt to create a new bag variable of the given type.
  -- Algorithm:
  --  attempt to create the header for the bag
  --  if successful
  --    attempt to add the header to the list of valid bags
  --    if successful
  --      initialize the bag header, and
  --      return the new bag
  --    otherwise, release the bag header
  --    fi
  --  fi
  --  raise the overflow exception
  -- end. *)

VAR   newBag : Bag; (*-- new bag variable being created *)

BEGIN
  bagError := noerr;
  Allocate(newBag, SIZE(UnboundedBag));
  IF (newBag = NullBag) THEN
    RaiseErrIn(create, overflow);
  ELSE
    WITH newBag^ DO
	  dataID := theType;
      length := 0;
	  first  := NullLink;
    END (*--with*);
  END (*--if*);
  RETURN newBag;
END Create;
(*--------------------*)

PROCEDURE Destroy       (VAR theBag   : Bag       (*-- inout *));

(*-- Destroy a previously created bag.
  -- Algorithm:
  -- clear the bag of existing nodes and their items, setting the
  -- bag error flag to “noerr” if the bag is defined
  -- if successful
  --   remove the bag from the list of valid bags, and
  --   release the dynamically allocated space used by the header
  -- fi
  -- end. *)

BEGIN
  Clear(theBag);
  IF (bagError = noerr) THEN
    Deallocate(theBag, SIZE(UnboundedBag));
  END (*--if*);
END Destroy;
(*--------------------*)

PROCEDURE Clear         (VAR theBag   : Bag       (*-- inout *));

(*-- Remove all nodes and items from a previously created bag.
  -- Algorithm:
  -- if the bag has been defined
  --   retrieve the item disposal routine for the bags' data type
  --   loop through each item node of the bag
  --     release dynamically allocated space for the item, if any
  --     release dynamically allocated space for the node
  --   pool
  --   reset the “first” link field in the bag header to NIL, and
  --   reset the “length” field in the bag header to zero
  -- otherwise
  --   raise the undefined exception
  -- fi
  -- end. *)

VAR   freeItem : DisposeProc; (*-- Item disposal routine, if any *)
      theNode  : Link;        (*-- Bag node to be deallocated *)

BEGIN
  bagError := noerr;
  IF (theBag # NullBag) THEN
    WITH theBag^ DO
	  freeItem := DisposeOf(dataID);
	  WHILE (first # NullLink) DO
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

PROCEDURE Assign        (    theBag   : Bag       (*-- in    *);
                         VAR toBag    : Bag       (*-- inout *));

(*-- Assign the items of one bag to another bag.
  -- Algorithm:
  --   if the source bag has been created
  --     if the target bag has been created
  --       destroy the target bag in its current incarnation
  --     fi
  --     create the target header as a duplicate of the source header
  --     if the source bag is not empty
  --       copy the source bag items to the target bag in the same
  --       order that they appear in the source bag
  --     fi
  --   otherwise
  --     raise the undefined exception
  --   fi
  -- end. *)

VAR   fromIndex : Link; (*-- Loop index over source bag items *)
	  toIndex   : Link; (*-- new item node in target bag *)

BEGIN
  bagError := noerr;
  bagRoutine := assign;
  IF (theBag # NullBag) THEN
    IF Recreate(toBag, theBag^.dataID) THEN
	  IF (theBag^.first = NullLink) THEN
		RETURN;
	  END (*--if*);
	  assignItem := AssignOf(theBag^.dataID);

      WITH theBag^.first^ DO
	    toBag^.first := NewNode(item, count, NullLink);
	  END (*--with*);
	  IF (bagError = overflow) THEN
        RETURN;
	  END (*--if*);
	  toIndex   := toBag^.first;
	  fromIndex := theBag^.first;

      WHILE (fromIndex^.next # NIL) DO
	    fromIndex := fromIndex^.next;
        WITH fromIndex^ DO
	      toIndex^.next := NewNode(item, count, NullLink);
	    END (*--with*);
	    IF (bagError = overflow) THEN
		  RETURN;
        END (*--if*);
        toIndex := toIndex^.next;
      END (*--while*);
      toBag^.length := theBag^.length;
    END (*--if*);
  ELSE
    RaiseErrIn(assign, undefined);
  END (*--if*);
END Assign;
(*--------------------*)

PROCEDURE Include       (    theItem  : Item      (*-- in    *);
       	       	         VAR inBag    : Bag       (*-- inout *));

(*-- Bag Inclusion for linearly ordered list.  Algorithm is similar
  -- to that from reference [2], pg. 118. *)

VAR   current     : Link;        (*-- Loop index in search of theItem *)
      previous    : Link;        (*-- Previous node examined *)
	  newNode     : Link;        (*-- For new node to add *)
	  compareItem : CompareProc; (*-- Item comparison routine *)
	  itemOrder   : Relation;    (*-- Ordering relation between items *)

BEGIN
  bagError := noerr;
  IF (inBag # NullBag) THEN
	compareItem := CompareOf(inBag^.dataID);
    current     := inBag^.first;
	previous    := NullLink;
    LOOP
	  IF (current = NullLink) THEN
        EXIT (*--loop*); (*-- theItem is not in the bag *)
      END (*--if*);

	  itemOrder := compareItem(current^.item, theItem);

      IF (itemOrder = equal) THEN (*-- theItem is in the bag *)
	    INC(current^.count);
        RETURN;

	  ELSIF (itemOrder = greater) THEN
        EXIT (*--loop*); (*-- theItem is not in the bag *)
      END (*--if*);

      previous := current; (*-- Keep looking *)
      current  := current^.next;
    END (*--loop*);

    (*-- Insert the new item *)

	Allocate(newNode, SIZE(Node));
	IF (newNode = NullLink) THEN
	  RaiseErrIn(include, overflow);
    ELSE
	  WITH newNode^ DO
	    item  := theItem;
		count := 1;
	  END (*--with*);
	  IF (previous = NullLink) THEN
	    newNode^.next := inBag^.first;
		inBag^.first  := newNode;
	  ELSE
	    newNode^.next  := current;
		previous^.next := newNode;
      END (*--if*);
      INC(inBag^.length);
    END (*--if*);
  ELSE
    RaiseErrIn(include, undefined);
  END (*--if*);
END Include;
(*--------------------*)

PROCEDURE Exclude       (    theItem  : Item      (*-- in    *);
       	       	         VAR fromBag  : Bag       (*-- inout *));

VAR   current     : Link;        (*-- Loop index over items *)
      previous    : Link;        (*-- Previous node examined *)
	  compareItem : CompareProc; (*-- Item comparison routine *)
	  itemOrder   : Relation;    (*-- Relation between items *)
	  freeItem    : DisposeProc; (*-- Item disposal routine *)

BEGIN
  bagError := noerr;
  IF (fromBag # NullBag) THEN
	compareItem := CompareOf(fromBag^.dataID);
    current     := fromBag^.first;
	previous    := NullLink;
    LOOP
	  IF (current = NullLink) THEN
        RETURN; (*-- theItem is not in the bag *)
      END (*--if*);

	  itemOrder := compareItem(current^.item, theItem);

      IF (itemOrder = equal) THEN (*-- theItem is in the bag *)
	    IF (current^.count > 1) THEN
		  DEC(current^.count);
		  RETURN;
		ELSE
          EXIT (*--loop*);
		END (*--if*);
      ELSIF (itemOrder = greater) THEN
        RETURN;          (*-- theItem is not in the bag *)
      END (*--if*);

      previous := current;       (*-- Keep looking *)
      current  := current^.next;
    END (*--loop*);

    (*-- “current” points to the node to be deleted. *)

    IF (previous = NullLink) THEN
	  fromBag^.first := current^.next;
	ELSE
	  previous^.next := current^.next;
	END (*--if*);
	freeItem := DisposeOf(fromBag^.dataID);
	freeItem(current^.item);
	Deallocate(current, SIZE(current^));
    DEC(fromBag^.length);
  ELSE
    RaiseErrIn(exclude, undefined);
  END (*--if*);
END Exclude;
(*--------------------*)

PROCEDURE Union         (    left     : Bag       (*-- in    *);
       	       	             right    : Bag       (*-- in    *);
                         VAR toBag    : Bag       (*-- inout *));

(*-- Compute the bag containing all members of left and right.
  -- x IN toBag iff (x IN left) OR (x IN right). *)

VAR   leftIndex   : Link;        (*-- Loop index over left bag *)
      rightIndex  : Link;        (*-- Loop index over right bag *)
	  toIndex     : Link;        (*-- List of target bag nodes *)
	  tempNode    : Link;        (*-- Temporary node *)
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
  ELSIF ~Recreate(toBag, left^.dataID) THEN
    RETURN;
  END (*--if*);

  WITH toBag^ DO
    compareItem := CompareOf(dataID);
    assignItem  := AssignOf(dataID);
  END (*--with*);

  bagRoutine := union;
  leftIndex  := left^.first;
  rightIndex := right^.first;
  WHILE (leftIndex # NullLink) & (rightIndex # NullLink) DO
	Allocate(tempNode, SIZE(Node));
	IF (tempNode = NullLink) THEN
	  RaiseErrIn(union, overflow);
	  RETURN;
	END (*--if*);
	tempNode^.next := NullLink;

    order := compareItem(leftIndex^.item, rightIndex^.item);
	INC(toBag^.length);

    IF (order = less) THEN
	  WITH tempNode^ DO
        item  := assignItem(leftIndex^.item);
	    count := leftIndex^.count;
	  END (*--with*);
      leftIndex := leftIndex^.next;
	ELSIF (order = equal) THEN
	  WITH tempNode^ DO
        item  := assignItem(leftIndex^.item);
	    count := leftIndex^.count + rightIndex^.count;
	  END (*--with*);
      leftIndex  := leftIndex^.next;
      rightIndex := rightIndex^.next;
    ELSE
	  WITH tempNode^ DO
        item  := assignItem(rightIndex^.item);
	    count := rightIndex^.count;
	  END (*--with*);
      rightIndex := rightIndex^.next;
    END (*--if*);

	TailInsert(tempNode, toBag^.first, toIndex);

  END (*--while*);

  (*-- Copy remaining items, if any, from either the left bag or
    -- the right bag to the destination bag. *)

  IF (leftIndex = NullLink) THEN
    CopySubset(rightIndex, toIndex, toBag);
  ELSIF (rightIndex = NullLink) THEN
    CopySubset(leftIndex, toIndex, toBag);
  END (*--if*);
END Union;
(*--------------------*)

PROCEDURE Intersection  (    left    : Bag         (*-- in    *);
       	       	             right   : Bag         (*-- in    *);
                         VAR toBag   : Bag         (*-- inout *));

(*-- Compute the bag containing all members in both left and right.
  -- x IN toBag iff (x IN left) AND (x IN right).
  -- Algorithm:
  --   if both source bags have been created
  --     if both source bags have the same data type
  --       if the target bag has been created
  --         destroy the target bag in its current incarnation
  --       fi
  --       create the target header as a duplicate of the source header
  --       if creating the target bag header was successful
  --         loop through both the left and right bags
  --         pool
  --       otherwise
  --         raise the overflow exception
  --       fi
  --     otherwise
  --       raise the type error exception
  --     fi
  --   otherwise
  --     raise the undefined exception
  --   fi
  -- end. *)

VAR   leftIndex   : Link;        (*-- Loop link over left bag *)
      rightIndex  : Link;        (*-- Loop link over right bag *)
	  toIndex     : Link;        (*-- List of target bag nodes *)
	  tempNode    : Link;		 (*-- temporary for new node *)
	  compareItem : CompareProc; (*-- Item comparison routine *)
	  order       : Relation;    (*-- Ordering relation between items *)
	  newCount    : CARDINAL;    (*-- Smaller of left/right counts *)

BEGIN
  bagError := noerr;
  IF (left = NullBag) OR (right = NullBag) THEN
    RaiseErrIn(intersection, undefined);
	RETURN;
  ELSIF (left^.dataID # right^.dataID) THEN
    RaiseErrIn(intersection, typeerror);
	RETURN;
  ELSIF ~Recreate(toBag, left^.dataID) THEN
    RETURN;
  END (*--if*);

  WITH toBag^ DO
    compareItem := CompareOf(dataID);
    assignItem  := AssignOf(dataID);
  END (*--with*);
  bagRoutine := intersection;

  leftIndex  := left^.first;
  rightIndex := right^.first;
  WHILE (leftIndex # NullLink) & (rightIndex # NullLink) DO
    order := compareItem(leftIndex^.item, rightIndex^.item);
    IF (order = equal) THEN

	  IF (leftIndex^.count < rightIndex^.count) THEN
	    newCount := leftIndex^.count;
	  ELSE
	    newCount := rightIndex^.count;
	  END (*--if*);

	  tempNode := NewNode(leftIndex^.item, newCount, NullLink);
	  IF (bagError = overflow) THEN
	    RETURN;
	  END (*--if*);
	  TailInsert(tempNode, toBag^.first, toIndex);
	  INC(toBag^.length);
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

PROCEDURE Difference    (    left    : Bag         (*-- in    *);
       	       	             right   : Bag         (*-- in    *);
                         VAR toBag   : Bag         (*-- inout *));

(*-- Compute the bag containing all members of left that are not
  -- members of right. x IN toBag iff (x IN left) & ¬(x IN right) *)

VAR   leftIndex   : Link;        (*-- Loop index over left bag *)
      rightIndex  : Link;        (*-- Loop index over right bag *)
	  toIndex     : Link;        (*-- List of target bag nodes *)
	  tempNode    : Link;        (*-- Temporary for new node *)
	  compareItem : CompareProc; (*-- Item comparison routine *)
	  order       : Relation;    (*-- Ordering relation between items *)

BEGIN
  bagError := noerr;
  IF (left = NullBag) OR (right = NullBag) THEN
    RaiseErrIn(difference, undefined);
	RETURN;
  ELSIF (left^.dataID # right^.dataID) THEN
    RaiseErrIn(difference, typeerror);
	RETURN;
  ELSIF ~Recreate(toBag, left^.dataID) THEN
    RETURN;
  END (*--if*);
  
  WITH toBag^ DO
    compareItem := CompareOf(dataID);
    assignItem  := AssignOf(dataID);
  END (*--with*);
  bagRoutine := difference;
  leftIndex  := left^.first;
  rightIndex := right^.first;
  WHILE (leftIndex # NullLink) & (rightIndex # NullLink) DO
    order := compareItem(leftIndex^.item, rightIndex^.item);

    IF (order = equal) THEN
	  IF (leftIndex^.count > rightIndex^.count) THEN
	    tempNode := NewNode(leftIndex^.item,
		                    leftIndex^.count - rightIndex^.count,
							NullLink);
	    IF (bagError = overflow) THEN
	      RETURN;
	    END (*--if*);

	    TailInsert(tempNode, toBag^.first, toIndex);
	    INC(toBag^.length);
	  END (*--if*);

      leftIndex  := leftIndex^.next;
      rightIndex := rightIndex^.next;

    ELSIF (order = less) THEN
	  WITH leftIndex^ DO
	    tempNode := NewNode(item, count, NullLink);
	  END (*--with*);
	  IF (bagError # noerr) THEN
	    RETURN;
	  END (*--if*);
	  TailInsert(tempNode, toBag^.first, toIndex);
	  INC(toBag^.length);

      leftIndex  := leftIndex^.next;

    ELSE
      rightIndex := rightIndex^.next;
    END (*--if*);
  END (*--while*);

  (*-- Copy remaining items, if any, from the left bag to the
    -- destination bag. *)

  CopySubset(leftIndex, toIndex, toBag);
END Difference;
(*--------------------*)

PROCEDURE SymDifference (    left    : Bag         (*-- in    *);
       	       	             right   : Bag         (*-- in    *);
                         VAR toBag   : Bag         (*-- inout *));

(*-- Compute the bag containing all members of left or right that
  -- are not members of both.
  -- x IN toBag iff (x IN left) # (x IN right) *)

VAR   leftIndex   : Link;        (*-- Loop index over left bag *)
      rightIndex  : Link;        (*-- Loop index over right bag *)
	  toIndex     : Link;        (*-- List of target bag nodes *)
	  tempNode    : Link;        (*-- Temporary node *)
	  compareItem : CompareProc; (*-- Item comparison routine *)
	  order       : Relation;    (*-- Ordering relation between items *)
	  newItem     : Item;        (*-- Either left/right item *)
	  newCount    : CARDINAL;    (*-- Either left/right count *)

BEGIN
  bagError := noerr;
  IF (left = NullBag) OR (right = NullBag) THEN
    RaiseErrIn(symdifference, undefined);
	RETURN;
  ELSIF (left^.dataID # right^.dataID) THEN
    RaiseErrIn(symdifference, typeerror);
	RETURN;
  ELSIF ~Recreate(toBag, left^.dataID) THEN
    RETURN;
  END (*--if*);
  
  WITH toBag^ DO
    compareItem := CompareOf(dataID);
    assignItem  := AssignOf(dataID);
  END (*--with*);

  bagRoutine := symdifference;
  leftIndex  := left^.first;
  rightIndex := right^.first;

  WHILE (leftIndex # NullLink) & (rightIndex # NullLink) DO
    order := compareItem(leftIndex^.item, rightIndex^.item);

    IF (order = equal) THEN
	  IF (leftIndex^.count > rightIndex^.count) THEN
	    WITH leftIndex^ DO
	      tempNode := NewNode(item, count - rightIndex^.count, NullLink);
	    END (*--with*);
	    IF (bagError = overflow) THEN
	      RETURN;
	    END (*--if*);

	    TailInsert(tempNode, toBag^.first, toIndex);
	    INC(toBag^.length);
	  END (*--if*);

      leftIndex  := leftIndex^.next;
      rightIndex := rightIndex^.next;

    ELSE
      IF (order = less) THEN
	    WITH leftIndex^ DO
	      newItem  := item;
		  newCount := count;
		END (*--with*);
        leftIndex := leftIndex^.next;
      ELSE
	    WITH rightIndex^ DO
	      newItem  := item;
		  newCount := count;
		END (*--with*);
        rightIndex := rightIndex^.next;
      END (*--if*);

	  tempNode := NewNode(newItem, newCount, NullLink);
      IF (tempNode = NullLink) THEN
        RETURN;
      END (*--if*);

	  TailInsert(tempNode, toBag^.first, toIndex);
      INC(toBag^.length);
    END (*--if*);
  END (*--while*);

  (*-- Copy remaining items, if any, from either the left bag or
    -- the right bag to the destination bag. *)

  IF (leftIndex = NullLink) THEN
    CopySubset(rightIndex, toIndex, toBag);
  ELSIF (rightIndex = NullLink) THEN
    CopySubset(leftIndex, toIndex, toBag);
  END (*--if*);
END SymDifference;
(*--------------------*)


    (*---------------------------------*)
    (*--         SELECTORS           --*)

PROCEDURE IsDefined     (    theBag  : Bag      (*-- in    *))
                                     : BOOLEAN  (*-- out   *);
BEGIN
  RETURN (theBag # NullBag);
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

VAR   leftIndex : Link; (*-- Loop index over left bag items *)
      rightIndex: Link; (*-- Loop index over right bag items *)
	  compare   : CompareProc;	(*-- item comparison routine *)

BEGIN
  bagError := noerr;
  IF (left # NullBag) & (right # NullBag) THEN
    IF (left^.dataID = right^.dataID) THEN
      IF (left^.length = right^.length) THEN

        (*-- Scan both Bags looking for the first mismatch (inequality)
          -- which indicates that the Bags are unequal.
          -- If the WHILE loop completes then the Bags must be equal.
		  -- Because the lengths are equal, rightIndex will be NIL
		  -- when leftIndex is NIL. *)

		compare   := CompareOf(left^.dataID);
        leftIndex := left^.first;
		rightIndex:= right^.first;
        WHILE (leftIndex # NullLink) DO
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

  (*-- the Bags must be unequal, either by having different
    -- lengths, mismatched types, or an undefined Bag exception. *)

  RETURN FALSE;
END IsEqual;
(*----------------------------*)

PROCEDURE NumMembers    (    theBag  : Bag         (*-- in    *))
                                     : CARDINAL    (*-- out   *);

VAR   current    : Link;
      totalCount : CARDINAL;

BEGIN
  bagError   := noerr;
  totalCount := 0;
  IF (theBag # NullBag) THEN
    current := theBag^.first;
    WHILE (current # NullLink) DO
      INC(totalCount, current^.count);
	  current := current^.next;
    END (*--while*);
  END (*--if*);

  RaiseErrIn(nummembers, undefined);
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

PROCEDURE IsAMember     (    theItem : Item        (*-- in    *);
       	       	             theBag  : Bag         (*-- in    *))
                                     : BOOLEAN     (*-- out   *);

VAR   index       : Link;        (*-- Loop index over items *)
	  compareItem : CompareProc; (*-- Item comparison routine *)
	  order       : Relation;    (*-- Ordering relation between items *)

BEGIN
  bagError := noerr;
  IF (theBag # NullBag) THEN
    WITH theBag^ DO
	  compareItem := CompareOf(dataID);
	  index := first;
	END (*--with*);
    WHILE (index # NullLink) DO
      IF (theItem = index^.item) THEN
        RETURN TRUE;
      ELSE
	    order := compareItem(index^.item, theItem);
        IF (order = greater) THEN
		  RETURN FALSE;
        END (*--if*);
      END (*--if*);
	  index := index^.next;
    END (*--while*);
  ELSE
    RaiseErrIn(ismember, undefined);
  END (*--if*);

  RETURN FALSE;
END IsAMember;
(*----------------------------*)

PROCEDURE NumberOf      (    theItem : Item     (*-- in    *);
       	       	             theBag  : Bag      (*-- in    *))
                                     : CARDINAL (*-- out   *);

VAR   index       : Link;        (*-- Loop index over items *)
	  compareItem : CompareProc; (*-- Item comparison routine *)
	  order       : Relation;    (*-- Ordering relation between items *)

BEGIN
  bagError := noerr;
  IF (theBag # NullBag) THEN
    WITH theBag^ DO
	  compareItem := CompareOf(dataID);
	  index := first;
	END (*--with*);
    WHILE (index # NullLink) DO
      IF (theItem = index^.item) THEN
        RETURN index^.count;
      ELSE
	    order := compareItem(index^.item, theItem);
        IF (order = greater) THEN
		  RETURN 0;
        END (*--if*);
      END (*--if*);
	  index := index^.next;
    END (*--while*);
  ELSE
    RaiseErrIn(numberof, undefined);
  END (*--if*);

  RETURN 0;
END NumberOf;
(*----------------------------*)

PROCEDURE IsSubset      (    left    : Bag         (*-- in    *);
                             right   : Bag         (*-- in    *))
                                     : BOOLEAN     (*-- out   *);

VAR   leftIndex   : Link;    (*-- Loop index over left bag *)
      rightIndex  : Link;    (*-- Loop index over right bag *)
	  compareItem : CompareProc; (*-- Item comparison routine *)
	  order       : Relation;    (*-- Ordering relation between items *)

BEGIN
  bagError := noerr;
  IF (left = NullBag) OR (right = NullBag) THEN
    RaiseErrIn(issubset, undefined);
    RETURN FALSE;
  ELSIF (left^.dataID # right^.dataID) THEN
    RaiseErrIn(issubset, typeerror);
    RETURN FALSE;
  END (*--if*);

  compareItem:= CompareOf(left^.dataID);
  leftIndex  := left^.first;
  rightIndex := right^.first;
  WHILE (leftIndex # NullLink) & (rightIndex # NullLink) DO
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
  RETURN (leftIndex = NullLink);
END IsSubset;
(*----------------------------*)

PROCEDURE IsProperSubset(    left    : Bag         (*-- in    *);
       	       	             right   : Bag         (*-- in    *))
                                     : BOOLEAN     (*-- out   *);
BEGIN
  RETURN IsSubset(left, right) & (left^.length < right^.length);
END IsProperSubset;
(*----------------------------*)


    (*---------------------------------*)
    (*--         ITERATORS           --*)

PROCEDURE LoopOver   (    theBag  : Bag            (*-- in    *);
                          process : LoopAccessProc (*-- in    *));

VAR   theNode : Link; (*-- Loop index over items *)

BEGIN
  bagError := noerr;
  IF (theBag # NullBag) THEN
	theNode := theBag^.first;
    WHILE (theNode # NullLink) DO
	  WITH theNode^ DO
        IF ~process(item, count) THEN
          RETURN;
        END (*--if*);
	  END (*--with*);
      theNode := theNode^.next;
    END (*--while*);
  ELSE
    RaiseErrIn(loopover, undefined);
  END (*--if*);
END LoopOver;
(*----------------------------*)

PROCEDURE Traverse   (    theBag  : Bag            (*-- in    *);
                          process : AccessProc     (*-- in    *));

VAR   theNode : Link; (*-- Loop index over items *)

BEGIN
  bagError := noerr;
  IF (theBag # NullBag) THEN
	theNode := theBag^.first;
    WHILE (theNode # NullLink) DO
	  WITH theNode^ DO
        process(item, count);
	  END (*--with*);
      theNode := theNode^.next;
    END (*--while*);
  ELSE
    RaiseErrIn(traverse, undefined);
  END (*--if*);
END Traverse;
(*----------------------------*)


(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
(*~~ MODULE INITIALIZATION  ~~*)

BEGIN
  FOR bagError := MIN(Exceptions) TO MAX(Exceptions) DO
    handlers[bagError] := ExitOnError;
  END (*--for*);
  handlers[noerr] := NullHandler;
  bagError := noerr;
END BagSUMI.