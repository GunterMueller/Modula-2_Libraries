IMPLEMENTATION MODULE DQPBSBMN;
(*===========================================================
    Version  : 1.00  16 May 1989  C. Lins
    Compiler : TopSpeed Modula-2
    Component: Monolithic Structures - Deque (Opaque version)
      Priority Balking Sequential Bounded Managed Non-Iterator

    REVISION HISTORY
    v1.00  16 May 1989  C. Lins:
	  Initial TopSpeed implementation.
	(C) Copyright 1989 Charles A. Lins
===========================================================*)

FROM SYSTEM IMPORT
    (*--Type*) ADDRESS;

FROM JPIStorage IMPORT
    (*--Proc*) Allocate, Deallocate;

FROM ErrorHandling IMPORT
    (*--Type*) HandlerProc,
    (*--Proc*) Raise, NullHandler, ExitOnError;

FROM Items IMPORT
    (*--Cons*) NullItem,
    (*--Type*) Item, AssignProc, DisposeProc, CompareProc;

FROM QEnum IMPORT
    (*--Type*) Operations, Exceptions, DequeComponentID;

FROM Relations IMPORT
    (*--Type*) Relation;

FROM TypeManager IMPORT
    (*--Cons*) NullType,
    (*--Type*) TypeID,
	(*--Proc*) AssignOf, DisposeOf, CompareOf;

    (*--------------------*)


TYPE  Priority = ADDRESS;

TYPE  ItemsArray   = ARRAY DequeSize OF Item;
TYPE  BoundedDeque = RECORD
	dataID   : TypeID;	(*-- defined data type *)
		priority : PriorityProc;    (*-- retrieve priority *)
		compare  : PriorityCompare; (*-- compare priorities *)
	size     : CARDINAL;	(*-- maximum # of items *)
	rear     : CARDINAL;	(*-- current # of items *)
	items    : ItemsArray;      (*-- array [1..size] of item *)
      END (*-- BoundedDeque *);

TYPE  Deque = POINTER TO BoundedDeque;

    (*--------------------*)


(*---------------------------------*)
(*--	EXCEPTIONS	--*)

VAR   dequeError : Exceptions;
VAR   handler    : ARRAY Exceptions OF HandlerProc;

PROCEDURE DequeError () : Exceptions (*-- out   *);
BEGIN
  RETURN dequeError;
END DequeError;
(*-------------------------*)

PROCEDURE SetHandler (    theError   : Exceptions  (*-- in    *);
	theHandler : HandlerProc (*-- in    *));
BEGIN
  handler[theError] := theHandler;
END SetHandler;
(*-------------------------*)

PROCEDURE GetHandler (    theError   : Exceptions  (*-- in    *))
	: HandlerProc (*-- out   *);
BEGIN
  RETURN handler[theError];
END GetHandler;
(*-------------------------*)

PROCEDURE RaiseErrIn (    theRoutine : Operations (*-- in    *);
	theError   : Exceptions (*-- in    *));
BEGIN
  dequeError := theError;
  Raise(DequeComponentID + ModuleID, theRoutine, theError, handler[theError]);
END RaiseErrIn;
(*-------------------------*)


(*---------------------------------*)
(*--	CONSTRUCTORS	--*)

PROCEDURE Create  (    theType    : TypeID	(*-- in    *);
	theSize    : DequeSize       (*-- in    *);
					   priorityOf : PriorityProc    (*-- in    *);
					   comparison : PriorityCompare (*-- in    *))
	: Deque	(*-- out   *);

CONST staticSize = SIZE(BoundedDeque) - SIZE(ItemsArray);
CONST itemSize   = SIZE(Item);

VAR   newDeque : Deque;

BEGIN
  dequeError := noerr;
  Allocate(newDeque, staticSize + itemSize * theSize);
  IF (newDeque = NIL) THEN
    RaiseErrIn(create, overflow);
  ELSE
    WITH newDeque^ DO
      dataID   := theType;
	  priority := priorityOf;
	  compare  := comparison;
      size     := theSize;
	  rear     := 0;
    END(*--with*);
  END(*--if*);
  RETURN newDeque;
END Create;
(*-------------------------*)

PROCEDURE Destroy (VAR theDeque : Deque     (*-- inout *));

CONST staticSize = SIZE(BoundedDeque) - SIZE(ItemsArray);
CONST itemSize   = SIZE(Item);

BEGIN
  Clear(theDeque);
  IF (dequeError = noerr) THEN
    Deallocate(theDeque, staticSize + itemSize * theDeque^.size);
  END (*--if*);
END Destroy;
(*-------------------------*)

PROCEDURE Clear   (VAR theDeque : Deque     (*-- inout *));

VAR   index : CARDINAL;    (*-- loop index over items *)
      free  : DisposeProc; (*-- item disposal routine *)

BEGIN
  dequeError := noerr;
  IF (theDeque # NIL) THEN
    WITH theDeque^ DO
	  free := DisposeOf(dataID);
      FOR index := MIN(DequeSize) TO rear DO
	free(items[index]);
      END (*--for*);
      rear := 0;
    END (*--with*);
  ELSE
    RaiseErrIn(clear, undefined);
  END (*--if*);
END Clear;
(*-------------------------*)

PROCEDURE Assign  (    theDeque : Deque     (*-- in    *);
	VAR toDeque  : Deque     (*-- inout *));

VAR   index      : CARDINAL;   (*-- loop index over items *)
      assignment : AssignProc; (*-- item assignment routine *)

BEGIN
  dequeError := noerr;
  IF (theDeque = NIL) THEN
    RaiseErrIn(assign, undefined);
  ELSIF (theDeque # toDeque) THEN
    IF (toDeque = NIL) THEN
      WITH theDeque^ DO
	toDeque := Create(dataID, size, priority, compare);
	  END (*--with*);
    ELSIF (theDeque^.rear <= toDeque^.size) THEN
      Clear(toDeque);
      WITH theDeque^ DO
	toDeque^.dataID   := dataID;
		toDeque^.priority := priority;
		toDeque^.compare  := compare;
	  END (*--with*);
	ELSE
      RaiseErrIn(assign, overflow);
    END (*--if*);
    IF (dequeError = noerr) THEN
      WITH theDeque^ DO
	    assignment := AssignOf(dataID);
	FOR index := MIN(DequeSize) TO rear DO
	toDeque^.items[index] := assignment(items[index]);
	END (*--for*);
	toDeque^.rear := rear;
      END (*--with*);
    END (*--if*);
  END (*--if*);
END Assign;
(*-------------------------*)

PROCEDURE Arrive  (VAR theDeque : Deque     (*-- inout *);
       	       	       theItem  : Item      (*-- in    *);
					   theEnd   : Location  (*-- in    *));

VAR   index : CARDINAL; (*-- loop index over items *)
      itemPriority : Priority; (*-- theItem's priority *)
	  priorityOrder: Relation; (*-- ordering between priorities *)

BEGIN
  dequeError := noerr;
  IF (theDeque = NIL) THEN
    RaiseErrIn(arrive, undefined);
  ELSE
    WITH theDeque^ DO
      IF (rear < size) THEN
	    itemPriority := priority(theItem);
	    index := rear;
	INC(rear);
		LOOP
		  IF (index > 0) THEN
		    priorityOrder := compare(priority(items[index]),
				itemPriority);
		IF (theEnd = front) THEN
		      IF (priorityOrder = greater) THEN
			EXIT (*--loop*);
			  ELSE
			    items[index + 1] := items[index];
			    DEC(index);
		      END (*--if*);
			ELSIF (priorityOrder # less) THEN
		      EXIT (*--loop*);
			ELSE
			  items[index + 1] := items[index];
			  DEC(index);
		    END (*--if*);
		  ELSE
		    EXIT (*--loop*);
		  END (*--if*);
		END (*--loop*);
	items[index + 1] := theItem;
      ELSE
	RaiseErrIn(arrive, overflow);
      END (*--if*);
    END (*--with*);
  END (*--if*);
END Arrive;
(*-------------------------*)

PROCEDURE Depart  (VAR theDeque : Deque     (*-- inout *);
					   theEnd   : Location  (*-- in    *));

VAR   index : CARDINAL;    (*-- loop index over items *)
      free  : DisposeProc; (*-- item disposal routine *)

BEGIN
  dequeError := noerr;
  IF (theDeque = NIL) THEN
    RaiseErrIn(depart, undefined);
  ELSE
    WITH theDeque^ DO
      IF (rear = 0) THEN
	RaiseErrIn(depart, underflow);
      ELSE
	    free := DisposeOf(dataID);
	    CASE theEnd OF
		  front :
	free(items[MIN(DequeSize)]);
	FOR index := MIN(DequeSize) + 1 TO rear DO
	items[index - 1] := items[index];
	END (*--for*);
		| back  :
	free(items[rear]);
		END (*--case*);
	DEC(rear);
      END (*--if*);
    END (*--with*);
  END (*--if*);
END Depart;
(*-------------------------*)

PROCEDURE Leave   (VAR theDeque : Deque     (*-- inout *);
       	       	       theItem  : Item      (*-- in    *);
					   theEnd   : Location  (*-- in    *));

VAR   index : CARDINAL;    (*-- loop index over items *)
      free  : DisposeProc; (*-- item disposal routine *)

BEGIN
  dequeError := noerr;
  IF (theDeque = NIL) THEN
    RaiseErrIn(leave, undefined);
  ELSE
    WITH theDeque^ DO
      IF (rear = 0) THEN
	RaiseErrIn(leave, underflow);
      ELSE
	    free := DisposeOf(dataID);
	    CASE theEnd OF
		  front :
		    index := MIN(DequeSize);
		    WHILE (index <= rear) & (items[index] # theItem) DO
		      INC(index);
		    END (*-- while*);
		| back  :
		    index := rear;
		    WHILE (index >= MIN(DequeSize)) & (items[index] # theItem) DO
		      DEC(index);
		    END (*-- while*);
		END (*--case*);
		IF (MIN(DequeSize) <= index) & (index <= rear) THEN
	free(items[index]);
	FOR index := index + 1 TO rear DO
	items[index - 1] := items[index];
	END (*--for*);
	DEC(rear);
		ELSE
	RaiseErrIn(leave, notfound);
	END (*--if*);
      END (*--if*);
    END (*--with*);
  END (*--if*);
END Leave;
(*-------------------------*)


(*---------------------------------*)
(*--	SELECTORS	--*)

PROCEDURE IsDefined (    theDeque : Deque    (*-- in    *))
	: BOOLEAN  (*-- out   *);
BEGIN
  RETURN theDeque # NIL;
END IsDefined;
(*-------------------------*)

PROCEDURE IsEmpty   (    theDeque : Deque    (*-- in    *))
	: BOOLEAN  (*-- out   *);
BEGIN
  dequeError := noerr;
  IF (theDeque # NIL) THEN
    RETURN (theDeque^.rear = 0);
  END (*--if*);
  RaiseErrIn(isempty, undefined);
  RETURN TRUE;
END IsEmpty;
(*-------------------------*)

PROCEDURE IsEqual   (    left     : Deque    (*-- in    *);
	right    : Deque    (*-- in    *))
	: BOOLEAN  (*-- out   *);

VAR   index   : CARDINAL; (*-- loop index over items *)
	  compareItems : CompareProc;	(*-- item comparison routine *)

BEGIN
  dequeError := noerr;
  IF (left = NIL) OR (right = NIL) THEN
    RaiseErrIn(isequal, undefined);
  ELSIF (left^.dataID # right^.dataID) THEN
    RaiseErrIn(isequal, typeerror);
  ELSIF (left^.rear = right^.rear) THEN
    WITH left^ DO
	  compareItems := CompareOf(dataID);
      FOR index := MIN(DequeSize) TO rear DO
	IF compareItems(items[index], right^.items[index]) # equal THEN
	RETURN FALSE;
	END (*--if*);
      END (*--for*);
      RETURN TRUE;
	END (*--with*);
  END (*--if*);
  RETURN FALSE;
END IsEqual;
(*-------------------------*)

PROCEDURE LengthOf  (    theDeque : Deque    (*-- in    *))
	: CARDINAL (*-- out   *);
BEGIN
  dequeError := noerr;
  IF (theDeque # NIL) THEN
    RETURN theDeque^.rear;
  END (*--if*);
  RaiseErrIn(lengthof, undefined);
  RETURN 0;
END LengthOf;
(*-------------------------*)

PROCEDURE SizeOf    (    theDeque : Deque    (*-- in    *))
	: CARDINAL (*-- out   *);
BEGIN
  dequeError := noerr;
  IF (theDeque # NIL) THEN
    RETURN theDeque^.size;
  END (*--if*);
  RaiseErrIn(sizeof, undefined);
  RETURN 0;
END SizeOf;
(*-------------------------*)

PROCEDURE TypeOf    (    theDeque : Deque    (*-- in    *))
	: TypeID   (*-- out   *);
BEGIN
  dequeError := noerr;
  IF (theDeque # NIL) THEN
    RETURN theDeque^.dataID;
  END (*--if*);
  RaiseErrIn(typeof, undefined);
  RETURN NullType;
END TypeOf;
(*-------------------------*)

PROCEDURE FrontOf   (    theDeque : Deque    (*-- in    *))
       	       		: Item     (*-- out   *);
BEGIN
  dequeError := noerr;
  IF (theDeque = NIL) THEN
    RaiseErrIn(frontof, undefined);
  ELSIF (theDeque^.rear = 0) THEN
    RaiseErrIn(frontof, underflow);
  ELSE
    RETURN theDeque^.items[MIN(DequeSize)];
  END (*--if*);
  RETURN NullItem;
END FrontOf;
(*-------------------------*)

PROCEDURE RearOf    (    theDeque : Deque    (*-- in    *))
       	       		: Item     (*-- out   *);
BEGIN
  dequeError := noerr;
  IF (theDeque = NIL) THEN
    RaiseErrIn(rearof, undefined);
  ELSIF (theDeque^.rear = 0) THEN
    RaiseErrIn(rearof, underflow);
  ELSE
    WITH theDeque^ DO
      RETURN items[rear];
	END (*--with*);
  END (*--if*);
  RETURN NullItem;
END RearOf;
(*-------------------------*)

PROCEDURE EndOf     (    theDeque : Deque    (*-- in    *);
	theEnd   : Location (*-- in    *))
       	       		: Item     (*-- out   *);
BEGIN
  dequeError := noerr;
  IF (theDeque = NIL) THEN
    RaiseErrIn(endof, undefined);
  ELSIF (theDeque^.rear = 0) THEN
    RaiseErrIn(endof, underflow);
  ELSE
    WITH theDeque^ DO
      CASE theEnd OF
	    front : RETURN items[MIN(DequeSize)];
	  | back  : RETURN items[rear];
	  END (*--case*);
	END (*--with*);
  END (*--if*);
  RETURN NullItem;
END EndOf;
(*-------------------------*)

PROCEDURE PositionOf (    theDeque: Deque    (*-- in    *);
       	       		theItem : Item     (*-- in    *))
							: CARDINAL (*-- out   *);

VAR   index : CARDINAL; (*-- loop index over items *)

BEGIN
  dequeError := noerr;
  IF (theDeque = NIL) THEN
    RaiseErrIn(positionof, undefined);
  ELSE
    WITH theDeque^ DO
      FOR index := MIN(DequeSize) TO rear DO
	    IF (theItem = items[index]) THEN
		  RETURN index;
		END (*--if*);
      END (*--for*);
    END (*--with*);
  END (*--if*);
  RETURN 0;
END PositionOf;
(*-------------------------*)


BEGIN
  FOR dequeError := MIN(Exceptions) TO MAX(Exceptions) DO
    SetHandler(dequeError, ExitOnError);
  END (*--for*);
  SetHandler(noerr, NullHandler);
  dequeError := noerr;
END DQPBSBMN.