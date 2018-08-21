IMPLEMENTATION MODULE DQPNSUMN;
(*===========================================================
    Version  : 1.00  16 May 1989  C. Lins
    Compiler : TopSpeed Modula-2
    Component: Monolithic Structures - Deque (Opaque version)
      Priority Non-Balking Sequential Unbounded Managed Non-Iterator

    REVISION HISTORY
    v1.00  16 May 1989  C. Lins:
	  Initial TopSpeed Modula-2 implementation.
	(C) Copyright 1989 Charles A. Lins
===========================================================*)

FROM SYSTEM IMPORT
	(*--type*) ADDRESS;

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

TYPE  Link = POINTER TO Node;
TYPE  Node  = RECORD
	prev : Link;  (*-- link to prior deque element *)
	item : Item;  (*-- deque element's data *)
	next : Link;  (*-- link to next deque element *)
      END (*-- Node *);
  
TYPE  Deque = POINTER TO UnboundedDeque;

TYPE  UnboundedDeque = RECORD
	dataID   : TypeID;	(*-- defined data type *)
		priority : PriorityProc;    (*-- retrieve item priority *)
		compare  : PriorityCompare; (*-- compare item priorities *)
	length   : CARDINAL;	(*-- current # of items *)
	head     : Link;	(*-- link to front of deque *)
	tail     : Link;	(*-- link to rear of deque *)
      END (*-- UnboundedDeque *);

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
					   priorityOf : PriorityProc    (*-- in    *);
					   comparison : PriorityCompare (*-- in    *))
	: Deque	(*-- out   *);

VAR   newDeque : Deque;

BEGIN
  Allocate(newDeque, SIZE(UnboundedDeque));
  IF (newDeque = NIL) THEN
    RaiseErrIn(create, overflow);
  ELSE
    WITH newDeque^ DO
      dataID   := theType;
	  priority := priorityOf;
	  compare  := comparison;
	  length   := 0;
	  head     := NIL;
	  tail     := NIL;
    END(*--with*);
  END(*--if*);
  RETURN newDeque;
END Create;
(*-------------------------*)

PROCEDURE Destroy (VAR theDeque : Deque     (*-- inout *));
BEGIN
  Clear(theDeque);
  IF (dequeError = noerr) THEN
    Deallocate(theDeque, SIZE(theDeque^));
  END (*--if*);
END Destroy;
(*-------------------------*)

PROCEDURE Clear   (VAR theDeque : Deque     (*-- inout *));

VAR   oldHead : Link;	(*-- node to be cleared *)
      free    : DisposeProc; (*-- item disposal routine *)

BEGIN
  dequeError := noerr;
  IF (theDeque # NIL) THEN
    WITH theDeque^ DO
	  free := DisposeOf(dataID);
      WHILE (head # NIL) DO
	oldHead := head;
	head    := head^.next;
	free(oldHead^.item);
	Deallocate(oldHead, SIZE(oldHead^));
      END (*--while*);
      tail   := NIL;
      length := 0;
    END (*--with*);
  ELSE
    RaiseErrIn(clear, undefined);
  END (*--if*);
END Clear;
(*-------------------------*)

PROCEDURE TailInsert(    theNode : Link (*-- inout *);
	VAR first   : Link (*-- inout *);
					 VAR last    : Link (*-- inout *));
BEGIN
  IF (first = NIL) THEN
    first := theNode;
  ELSE
    last^.next := theNode;
  END (*--if*);
  theNode^.prev := last;
  last := theNode;
END TailInsert;

PROCEDURE Assign  (    theDeque : Deque     (*-- in    *);
	VAR toDeque  : Deque     (*-- inout *));

VAR   index      : Link;       (*-- loop index over source items *)
	  newNode    : Link;       (*-- new item node for target deque *)
      assignment : AssignProc; (*-- item assignment routine *)

BEGIN
  dequeError := noerr;
  IF (theDeque = NIL) THEN
    RaiseErrIn(assign, undefined);
  ELSIF (theDeque # toDeque) THEN
    IF (toDeque = NIL) THEN
      WITH theDeque^ DO
	toDeque := Create(dataID, priority, compare);
	  END (*--with*);
    ELSE
      Clear(toDeque);
      WITH theDeque^ DO
	toDeque^.dataID   := dataID;
	    toDeque^.priority := priority;
	    toDeque^.compare  := compare;
	  END (*--with*);
    END (*--if*);
    IF (dequeError = noerr) THEN
      WITH theDeque^ DO
	    assignment := AssignOf(dataID);
	index := head;
      END (*--with*);
      WHILE (index # NIL) DO
	Allocate(newNode, SIZE(Node));
	    IF (newNode = NIL) THEN
	      RaiseErrIn(assign, overflow);
	      RETURN;
	    END (*--if*);
	    WITH newNode^ DO
	item := assignment(index^.item);
	next := NIL;
	END (*--with*);
	WITH toDeque^ DO
	      TailInsert(newNode, head, tail);
	END (*--with*);
	index := index^.next;
      END (*--while*);
      toDeque^.length := theDeque^.length;
    END (*--if*);
  END (*--if*);
END Assign;
(*-------------------------*)

PROCEDURE Arrive  (VAR theDeque : Deque     (*-- inout *);
       	       	       theItem  : Item      (*-- in    *);
					   theEnd   : Location  (*-- in    *));

VAR   newNode      : Link;     (*-- new node for theItem *)
      index	: Link;     (*-- loop index over items *)
      itemPriority : Priority; (*-- theItem's priority *)
	  priorityOrder: Relation; (*-- ordering between priorities *)

BEGIN
  dequeError := noerr;
  IF (theDeque = NIL) THEN
    RaiseErrIn(arrive, undefined);
  ELSE
    Allocate(newNode, SIZE(Node));
    IF (newNode = NIL) THEN
      RaiseErrIn(arrive, overflow);
    ELSE
      WITH newNode^ DO
	item := theItem;
	next := NIL;
		prev := NIL;
      END (*--with*);
      WITH theDeque^ DO
	INC(length);
	    itemPriority := priority(theItem);
	IF (head = NIL) THEN
	head := newNode;
		  tail := newNode;
		ELSE
		  index := head;
		  WHILE (index # NIL) &
			(compare(itemPriority, priority(index^.item)) = less) DO
			index := index^.next;
		  END (*--while*);
	IF (theEnd = back) THEN
		    WHILE (index # NIL) &
			(compare(itemPriority, priority(index^.item)) = equal) DO
			  index := index^.next;
		    END (*--while*);
		  END (*--if*);
		  IF (index = NIL) THEN
	tail^.next := newNode;
		    newNode^.prev := tail;
		    tail := newNode;
		  ELSE
		    WITH newNode^ DO
			  next := index;
			  prev := index^.prev;
			END (*--with*);
			IF (index^.prev = NIL) THEN
			  head := newNode;
			ELSE
		      index^.prev^.next := newNode;
			END (*--if*);
			index^.prev := newNode;
		  END (*--if*);
	END (*--if*);
      END (*--with*);
    END (*--if*);
  END (*--if*);
END Arrive;
(*-------------------------*)

PROCEDURE Depart  (VAR theDeque : Deque     (*-- inout *);
					   theEnd   : Location  (*-- in    *));

VAR   oldNode : Link;	(*-- departing node *)
      free    : DisposeProc; (*-- item disposal routine *)

BEGIN
  dequeError := noerr;
  IF (theDeque = NIL) THEN
    RaiseErrIn(depart, undefined);
  ELSE
    WITH theDeque^ DO
      IF (head = NIL) THEN
	RaiseErrIn(depart, underflow);
      ELSE
	    free := DisposeOf(dataID);
	    CASE theEnd OF
		  front :
		oldNode := head;
	head    := head^.next;
			IF (head # NIL) THEN
			  head^.prev := NIL;
			END (*--if*);
		| back  :
		oldNode := tail;
			tail    := tail^.prev;
			IF (tail # NIL) THEN
			  tail^.next := NIL;
			END (*--if*);
		END (*--case*);
	free(oldNode^.item);
	Deallocate(oldNode, SIZE(oldNode^));
	DEC(length);
	IF (length = 0) THEN
		  head := NIL;
	tail := NIL;
	END (*--if*);
      END (*--if*);
    END (*--with*);
  END (*--if*);
END Depart;
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
    RETURN (theDeque^.head = NIL);
  END (*--if*);
  RaiseErrIn(isempty, undefined);
  RETURN TRUE;
END IsEmpty;
(*-------------------------*)

PROCEDURE IsEqual   (    left     : Deque    (*-- in    *);
	right    : Deque    (*-- in    *))
	: BOOLEAN  (*-- out   *);

VAR   leftIndex    : Link;  (*-- loop index of left deque *)
      rightIndex   : Link;  (*-- loop index of right deque *)
	  compareItems : CompareProc;	(*-- item comparison routine *)

BEGIN
  dequeError := noerr;
  IF (left = NIL) OR (right = NIL) THEN
    RaiseErrIn(isequal, undefined);
  ELSIF (left^.dataID # right^.dataID) THEN
    RaiseErrIn(isequal, typeerror);
  ELSIF (left^.length = right^.length) THEN
	compareItems := CompareOf(left^.dataID);
    leftIndex  := left^.head;
    rightIndex := right^.head;
    WHILE (leftIndex # NIL) DO
      IF (rightIndex = NIL) OR
	     (compareItems(leftIndex^.item, rightIndex^.item) # equal) THEN
	RETURN FALSE;
      END (*--if*);
      leftIndex := leftIndex^.next;
      rightIndex:= rightIndex^.next;
    END (*--while*);
    RETURN (rightIndex = NIL);
  END (*--if*);
  RETURN FALSE;
END IsEqual;
(*-------------------------*)

PROCEDURE LengthOf  (    theDeque : Deque    (*-- in    *))
	: CARDINAL (*-- out   *);
BEGIN
  dequeError := noerr;
  IF (theDeque # NIL) THEN
    RETURN theDeque^.length;
  END (*--if*);
  RaiseErrIn(lengthof, undefined);
  RETURN 0;
END LengthOf;
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
  ELSIF (theDeque^.head = NIL) THEN
    RaiseErrIn(frontof, underflow);
  ELSE
    RETURN theDeque^.head^.item;
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
  ELSIF (theDeque^.tail = NIL) THEN
    RaiseErrIn(rearof, underflow);
  ELSE
    RETURN theDeque^.tail^.item;
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
  ELSIF (theDeque^.head = NIL) THEN
    RaiseErrIn(endof, underflow);
  ELSIF (theEnd = front) THEN
	RETURN theDeque^.head^.item;
  ELSE
	RETURN theDeque^.tail^.item;
  END (*--if*);
  RETURN NullItem;
END EndOf;
(*-------------------------*)


BEGIN
  FOR dequeError := MIN(Exceptions) TO MAX(Exceptions) DO
    SetHandler(dequeError, ExitOnError);
  END (*--for*);
  SetHandler(noerr, NullHandler);
  dequeError := noerr;
END DQPNSUMN.

(*
References


[1] A. Aho, J. Hopcroft, and J. Ullman, Data Structures and Algorithms,
    Addison-Wesley, Reading, MA 1983, pp. 56-60.
[2] G. Booch, Software Components in Ada Structures, Tools, and Subsystems,
   	Benjamin/Cummings, Menlo Park, CA 1987, pp. 92-93, 142-153.
[3] K. John Gough, "Writing Generic Utilities in Modula-2", Journal of
    Pascal, Ada, and Modula-2, Vol. 5(3), (May/June 1986), pp 53-62.
[4] T.A. Standish, Data Structure Techniques, Chapter 2: Stacks and Queues,
    Addison-Wesley, Reading, MA 1980, pp. 20-23, 28-32.
[5] R.S. Wiener and G.A. Ford, Modula-2 A Software Development Approach,
    John Wiley & Sons, New York, NY 1985, pp. 247-253
[6] R.S. Wiener and R.F. Sincovec, Data Structures Using Modula-2,
    John Wiley & Sons, New York, NY 1986, pp. 69-71
*)