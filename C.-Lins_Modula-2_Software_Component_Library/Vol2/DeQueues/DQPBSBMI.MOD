IMPLEMENTATION MODULE DQPBSBMI;
(*===========================================================
    Version  : 1.00  16 May 1989  C. Lins
    Compiler : TopSpeed Modula-2
    Component: Monolithic Structures - Deque (Opaque version)
      Priority Balking Sequential Bounded Managed Iterator

    REVISION HISTORY
    v1.00  16 May 1989  C. Lins:
	  Initial TopSpeed Modula-2 implementation.
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
    (*--Type*) Item, AssignProc, DisposeProc, CompareProc,
		AccessProc, LoopAccessProc;

FROM QEnum IMPORT
    (*--Type*) Operations, Exceptions, DequeComponentID;

FROM Relations IMPORT
    (*--Type*) Relation;

FROM TypeManager IMPORT
    (*--Cons*) NullType,
    (*--Type*) TypeID,
	(*--Proc*) AssignOf, DisposeOf, CompareOf;

    (*--------------------*)

(*
13.3.1 Internal Bounded Priority Balking Deque Representation

A dynamically created array of Items is created when a deque
is created. The front of the deque is always accessible by the
index [1].  The header record is used to hold the deque size limit (size),
the current rear index (rear), data type id (dataID), the procedures 
to retrieve and compare item priorities (priority and compare,
respectively), and the dynamic array of items (items).
Items are stored in a linear fashion within the array ordered by priority
with higher priority items closer to the front of the deque. This means
that the items are sorted in descending priority order.
Figure 13.1, below, graphically depicts the internal structure used 
for the bounded deque:

FIGURE 13.1

Representation Invariants:
* MIN(DequeSize) <= size <= MAX(DequeSize)
* 0 <= rear <= size
* the deque is empty when rear is zero
* when not empty, the front of the deque is at items[MIN(DequeSize)]
*)

TYPE  Priority = ADDRESS;  (*-- any 4-byte value will do *)

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


(*
13.3.2 Exceptions

dequeError holds the exception result from the most recently
invoked operation of this module. The Exceptions enumeration
constant noerr indicates successful completion of the operation and
all operations that may raise an exception assign this value to
dequeError before any other processing.

The handlers array holds the current exception handler for the
possible exceptions that may be raised from within this module.

Both are initialized by the module initialization (see section
13.3.6).

DequeError simply returns the current exception result stored
in dequeError and is used to determine whether a deque operation
completed successfully.  SetHandler makes theHandler the current
exception handler for theError, while GetHandler returns the
current exception handler.
*)

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


(*
13.3.3 Constructors

Create attempts to form a new, empty bounded deque object associated
with the given data type ID, maximum size, priority retrieval and
priority comparison routines.  Only the specified
number of entries given in theSize are actually allocated to the
ItemsArray (items).  MacSystem.Allocate returns NIL if it is unable
to successfully complete the allocation request whereupon the overflow
exception is raised and the NullDeque returned.  If successful, the
deque is initialized as follows before returning the new deque:
* theType is saved for later use by IsEqual,
* theSize is saved for later checking for deque overflow by Arrive and Assign, and
* rear is set to the empty deque state.
* the procedure parameters for priorities are saved for use in Arrive
Complexity: O(1).
*)

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

(*
Destroy lets Clear raise the undefined exception and simply releases
dynamically allocated memory resources for theDeque rear to the system.
MacSystem.Deallocate automatically releases the proper amount of space
originally allocated and alters the pointer to NIL (which is also the
value of the NullDeque). Complexity: O(1).
*)

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

(*
Clear retrieves the item disposal routine for theDeque's data type, if
any, and proceeds to free each item in theDeque.  If theDeque is empty
the loop is not executed as rear will be greater than the minimum
DequeSize.  Lastly, rear is set to ensure theDeque is in the empty state.
Complexity: O(n).
*)

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

(*
Assign duplicates the items of theDeque to the target deque, toDeque. An
undefined source deque raised an exception and leaves the target unchanged.
If the target deque is undefined, it is created with the same size and
data type attributes of the source; otherwise the target deque is
cleared of its presnet contents and its data type is altered to reflect
that of the source.  If the target deque is capable of containing all
of the items present in the source, Assign simply copies each item from
the source to the target afterwards updating the target's rear value.
Complexity: O(mn).
*)

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

(*
Arrive adds theItem into its proper position within the items array based
on its priority and theEnd which controls how items of like priority
are ordered relative to one another. 

A precondition is that the array be sorted in descending sequence by
priority and the corresponding postcondition is the same.
Also, there may be more than one item with the same priority.
The algorithm used is a variation on 3.2 Sorted Array Search by Gonnet
[2, pp. 31-32].  This search goes from the back of the deque towards
the front shifting items up into higher index positions within the
items array as it searches for the proper location for the insertion.
When theItem arrives at the rear of theDeque the search/shift must
stop when the next item in the array has a priority greater than or equal
to the given item's priority.  When theItem arrives at the front of
theDeque the search/shift must stop when the next item in the array
has a priority greater than the given item's priority.

When the rear of theDeque is already at its maximum allowed size the
exception overflow is raised and theDeque remains unchanged.  Likewise,
given an undefined deque raises the exception of the same name.
Complexity: O(n).
*)

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

(*
Depart removes theItem at the front or the rear of theDeque depending
on the value of theEnd.  If the item is departing from the rear of the
deque it is sufficient to free the item's value and decrement the rear
index.  Otherwise, if departing from the front of the deque it is
necessary to shift all other items down one position in the items array
and deduct one from the rear index.  Before overwriting the item being
removed, it's value is freed via the disposal routine of theDeque's
data type.  If theDeque is empty on entry to Depart the underflow
exception is raised and theDeque is not changed.  When theDeque is not
defined, the undefined exception is raised.
Complexity: Front O(n), Rear O(1).
*)

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

(*
Leave removes theItem from theDeque regardless of its position within
the deque.  The key to the implementation is the search mechanism for
theItem.  When theEnd is front the search extends from there towards
the back, and the reverse when theEnd is back.  If theItem was found
index will point into the items array at theItem, otherwise index will
be > rear on a search from the front and zero on a search from the back.
Complexity: O(n).
*)

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


(*
13.3.4 Selectors

IsDefined verifies to the best of its ability whether theDeque has been
created and is still an active object. Complexity: O(1).
*)

PROCEDURE IsDefined (    theDeque : Deque    (*-- in    *))
	: BOOLEAN  (*-- out   *);
BEGIN
  RETURN theDeque # NIL;
END IsDefined;
(*-------------------------*)

(*
IsEmpty returns True if theDeque is in the empty state, as indicated by
the rear being zero, and False otherwise.  As per the specification
(section 8.?) undefined deques are considered empty. Complexity: O(1).
*)

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

(*
IsEqual compares the left and right deques for equality, which in this
context means they contain the same items and the same data type ID. The
defined size of the deques is not relevant for the equality test.  Both
deques must be defined and have the same data type ID; if they don not,
then the exceptions undefined and typeerror are raised, respectively.
Obviously, deques of different lengths (indicated by the value of rear)
cannot be equal since there would be at least one item different between
them.  The algorithm simply loops over each of the items in both deques
returning false immediately upon encountering an inequality, and returning
true if and only if every item is the same between them.
Complexity: O(Min(m,n)).
*)

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

(*
LengthOf simply returns the rear index into the items array which is
also the length of theDeque. Complexity: O(1).
*)

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

(*
SizeOf and TypeOf simply return the size and dataID for the given deque.
Undefined deques, as always, raise the undefined exception and return
reasonable values (zero and the NullType, respectively). The complexity
of both routines is O(1).
*)

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

(*
FrontOf returns the value of the item that is at the front of theDeque
or the NullItem if theDeque is undefined or is empty. Complexity: O(1).
*)

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

(*
RearOf returns the value of the item that is at the rear of theDeque
or the NullItem if theDeque is undefined or is empty. Complexity: O(1).
*)

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

(*
EndOf returns the value of the item that is at the given end of theDeque
(front or rear) or the NullItem if theDeque is undefined or is empty.
Complexity: O(1).
*)

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

(*
PositionOf returns the number of positions from the given item from the
front of the deque. If theItem is not present in theDeque then zero
is returned. Complexity O(n).
*)

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


(*
13.3.5 Iterators

Both LoopOver and Traverse simply loop through each of the deque items
passing the item value to theProcess procedure parameter. The direction
of the iteration is controlled by theEnd parameter and is from the front
to the rear when theEnd is front and the reverse when theEnd is rear.
LoopOver may terminate before reaching the opposite end if theProcess
returns False. Complexity: O(n).
*)

PROCEDURE LoopOver (    theDeque  : Deque	(*-- in    *);
	theProcess: LoopAccessProc (*-- in    *);
						theEnd    : Location       (*-- in    *));

VAR   index : CARDINAL; (*-- loop index over items *)

BEGIN
  dequeError := noerr;
  IF (theDeque = NIL) THEN
    RaiseErrIn(loopover, undefined);
  ELSE
    WITH theDeque^ DO
      CASE theEnd OF
	    front :
	FOR index := MIN(DequeSize) TO rear DO
	IF ~theProcess(items[index]) THEN
	RETURN;
	END (*--if*);
	END (*--for*);
	  | back  :
	FOR index := rear TO MIN(DequeSize) BY -1 DO
	IF ~theProcess(items[index]) THEN
	RETURN;
	END (*--if*);
	END (*--for*);
	  END (*--case*);
    END (*--with*);
  END (*--if*);
END LoopOver;
(*-------------------------*)

PROCEDURE Traverse (    theDeque  : Deque	(*-- in    *);
	theProcess: AccessProc     (*-- in    *);
						theEnd    : Location       (*-- in    *));

VAR   index : CARDINAL; (*-- loop index over items *)

BEGIN
  dequeError := noerr;
  IF (theDeque = NIL) THEN
    RaiseErrIn(traverse, undefined);
  ELSE
    WITH theDeque^ DO
      CASE theEnd OF
	    front :
	FOR index := MIN(DequeSize) TO rear DO
	theProcess(items[index]);
	END (*--for*);
	  | back  :
	FOR index := rear TO MIN(DequeSize) BY -1 DO
	theProcess(items[index]);
	END (*--for*);
	  END (*--case*);
    END (*--with*);
  END (*--if*);
END Traverse;
(*-------------------------*)


(*
13.3.6 Module Initialization

The module's local variables are initialized to known states.
dequeError is used to fill the handlers array with a routine
that will exit the program when an exception is raised (saving the
declaration of a special loop control variable for this purpose).
The condition noerr is given the NullHandler which is presumed to
do nothing.  Applying MIN and MAX to cover all exceptions followed
by resetting the handler for noerr ensures that this initialization
will be unaffected by any future changes to the number of Exceptions
or their order of declaration within the enumeration.

Since a FOR loop control variable is undefined following the loop,
dequeError must be set to indicate that an error has not yet occurred.
*)

BEGIN
  FOR dequeError := MIN(Exceptions) TO MAX(Exceptions) DO
    SetHandler(dequeError, ExitOnError);
  END (*--for*);
  SetHandler(noerr, NullHandler);
  dequeError := noerr;
END DQPBSBMI.