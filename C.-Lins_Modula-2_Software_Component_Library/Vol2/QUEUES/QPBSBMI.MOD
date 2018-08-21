IMPLEMENTATION MODULE QPBSBMI;
(*==============================================================
    Version  : 1.00  18 May 1989  C. Lins
    Compiler : TopSpeed Modula-2
    Code Size: R-  bytes
    Component: Monolithic Structures - Queue (Opaque version)
      Priority Balking Sequential Bounded Managed Iterator

    REVISION HISTORY
    v1.00  18 May 1989  C. Lins
      Initial TopSpeed Modula-2 implementation.

    (C) Copyright 1989 Charles A. Lins
==============================================================*)

FROM SYSTEM IMPORT
    (*--type*) ADDRESS;

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
    (*--Type*) Operations, Exceptions, ComponentID;

FROM Relations IMPORT
    (*--Type*) Relation;

FROM TypeManager IMPORT
    (*--Cons*) NullType,
    (*--Type*) TypeID,
    (*--Proc*) AssignOf, DisposeOf, CompareOf;

    (*--------------------*)

(*
12.2.1 Internal Bounded Balking Priority Queue Representation

A dynamically created array of Items is created when a priority queue
is created. The front of the queue is always accessible by the
index [1].  The header record is used to hold the queue size limit (size),
the current back index (rear), data type id (dataID), the procedures to
retrieve and compare item priorities (priority and compare, respectively),
and the dynamic array of items.  Items are stored in a linear fashion
within the array ordered by priority.  Figure 12.1, below,
graphically depicts the internal structure used for the bounded priority
queue:

FIGURE 12.1

Representation Invariants:
¥ MIN(QueueSize) <= size <= MAX(QueueSize)
¥ 0 <= rear <= size
¥ the queue is empty when rear is zero
¥ when not empty, the front of the queue is at items[MIN(QueueSize)]
¥ when not empty, priority(items[1]) ³ priority(items[2]) and so on
*)

TYPE  Priority = ADDRESS; (*-- any 4-byte pointer type will do *)
TYPE  ItemsArray   = ARRAY QueueSize OF Item;
TYPE  BoundedQueue = RECORD
        dataID   : TypeID;          (*-- defined data type *)
        priority : PriorityProc;    (*-- retrieve priority *)
        compare  : PriorityCompare; (*-- compare priorities *)
        size     : QueueSize;       (*-- maximum # of items *)
        rear     : CARDINAL;        (*-- current # of items *)
        items    : ItemsArray;      (*-- array [1..size] of item *)
      END (*-- BoundedQueue *);

TYPE  Queue = POINTER TO BoundedQueue;

    (*--------------------*)


(*
12.2.2 Exceptions

queueError holds the exception result from the most recently
invoked operation of this module. The Exceptions enumeration
constant noerr indicates successful completion of the operation and
all operations that may raise an exception assign this value to
queueError before any other processing.

The handlers array holds the current exception handler for the
possible exceptions that may be raised from within this module.

Both are initialized by the module initialization (see section
12.2.6).

QueueError simply returns the current exception result stored
in queueError and is used to determine whether a queue operation
completed successfully.  SetHandler makes theHandler the current
exception handler for theError, while GetHandler returns the
current exception handler.
*)

VAR   queueError : Exceptions;
VAR   handlers   : ARRAY Exceptions OF HandlerProc;


PROCEDURE QueueError () : Exceptions (*-- out   *);
BEGIN
  RETURN queueError;
END QueueError;
(*-------------------------*)

PROCEDURE SetHandler (    theError   : Exceptions  (*-- in    *);
                          theHandler : HandlerProc (*-- in    *));
BEGIN
  handlers[theError] := theHandler;
END SetHandler;
(*-------------------------*)

PROCEDURE GetHandler (    theError   : Exceptions  (*-- in    *))
                                     : HandlerProc (*-- out   *);
BEGIN
  RETURN handlers[theError];
END GetHandler;
(*-------------------------*)

PROCEDURE RaiseErrIn (    theRoutine : Operations (*-- in    *);
                          theError   : Exceptions (*-- in    *));
BEGIN
  queueError := theError;
  Raise(ComponentID + ModuleID, theRoutine, theError, handlers[theError]);
END RaiseErrIn;
(*-------------------------*)


(*
12.2.3 Constructors

Create attempts to form a new, empty bounded queue object associated
with the given data type ID, maximum size, priority retrieval and
priority comparison routines.  Only the specified
number of entries given in theSize are actually allocated to the
ItemsArray (items).  MacSystem.Allocate returns NIL if it is unable
to successfully complete the allocation request whereupon the overflow
exception is raised and the NullQueue returned.  If successful, the
queue is initialized as follows before returning the new queue:
¥ theType is saved for later use by IsEqual,
¥ theSize is saved for later checking for queue overflow by Arrive and Assign, and
¥ rear is set to the empty queue state.
¥ the procedure parameters for priorities are saved for use in Arrive
Complexity: O(1).
*)

PROCEDURE Create  (    theType    : TypeID          (*-- in    *);
                       theSize    : QueueSize       (*-- in    *);
                       priorityOf : PriorityProc    (*-- in    *);
                       comparison : PriorityCompare (*-- in    *))
                                  : Queue           (*-- out   *);

CONST staticSize = SIZE(BoundedQueue) - SIZE(ItemsArray);
CONST itemSize   = SIZE(Item);

VAR   newQueue : Queue;

BEGIN
  queueError := noerr;
  Allocate(newQueue, staticSize + itemSize * theSize);
  IF (newQueue = NIL) THEN
    RaiseErrIn(create, overflow);
  ELSE
    WITH newQueue^ DO
      dataID   := theType;
      priority := priorityOf;
      compare  := comparison;
      size     := theSize;
      rear     := 0;
    END(*--with*);
  END(*--if*);
  RETURN newQueue;
END Create;
(*-------------------------*)

(*
Destroy lets Clear raise the undefined exception and simply releases
dynamically allocated memory resources for theQueue rear to the system.
MacSystem.Deallocate automatically releases the proper amount of space
originally allocated and alters the pointer to NIL (which is also the
value of the NullQueue). Complexity: O(1).
*)

PROCEDURE Destroy (VAR theQueue : Queue     (*-- inout *));

CONST staticSize = SIZE(BoundedQueue) - SIZE(ItemsArray);
CONST itemSize   = SIZE(Item);

BEGIN
  Clear(theQueue);
  IF (queueError = noerr) THEN
    Deallocate(theQueue, staticSize + itemSize * theQueue^.size);
  END (*--if*);
END Destroy;
(*-------------------------*)

(*
Clear retrieves the item disposal routine for theQueue's data type, if
any, and proceeds to free each item in theQueue.  If theQueue is empty
the loop is not executed as rear will be greater than the minimum
QueueSize.  Lastly, rear is set to ensure theQueue is in the empty state.
Complexity: O(n).
*)

PROCEDURE Clear   (VAR theQueue : Queue     (*-- inout *));

VAR   index : CARDINAL;    (*-- loop index over items *)
      free  : DisposeProc; (*-- item disposal routine *)

BEGIN
  queueError := noerr;
  IF (theQueue # NIL) THEN
    WITH theQueue^ DO
      free := DisposeOf(dataID);
      FOR index := MIN(QueueSize) TO rear DO
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
Assign duplicates the items of theQueue to the target queue, toQueue. An
undefined source queue raised an exception and leaves the target unchanged.
If the target queue is undefined, it is created with the same size and
data type attributes of the source; otherwise the target queue is
cleared of its presnet contents and its data type is altered to reflect
that of the source.  If the target queue is capable of containing all
of the items present in the source, Assign simply copies each item from
the source to the target afterwards updating the target's rear value.
Complexity: O(n).
*)

PROCEDURE Assign  (    theQueue : Queue     (*-- in    *);
                   VAR toQueue  : Queue     (*-- inout *));

VAR   index      : CARDINAL;   (*-- loop index over items *)
      assignment : AssignProc; (*-- item assignment routine *)

BEGIN
  queueError := noerr;
  IF (theQueue = NIL) THEN
    RaiseErrIn(assign, undefined);
  ELSIF (theQueue # toQueue) THEN
    IF (toQueue = NIL) THEN
      WITH theQueue^ DO
        toQueue := Create(dataID, size, priority, compare);
      END (*--with*);
    ELSIF (theQueue^.rear <= toQueue^.size) THEN
      Clear(toQueue);
      WITH theQueue^ DO
        toQueue^.dataID   := dataID;
        toQueue^.priority := priority;
        toQueue^.compare  := compare;
      END (*--with*);
    ELSE
      RaiseErrIn(assign, overflow);
    END (*--if*);
    IF (queueError = noerr) THEN
      WITH theQueue^ DO
        assignment := AssignOf(dataID);
        FOR index := MIN(QueueSize) TO rear DO
          toQueue^.items[index] := assignment(items[index]);
        END (*--for*);
        toQueue^.rear := rear;
      END (*--with*);
    END (*--if*);
  END (*--if*);
END Assign;
(*-------------------------*)

(*
Arrive adds theItem into its proper position within the items array based
on its priority.  A precondition is that the array be sorted in
descending sequence by priority and the corresponding postcondition is
the same.  Also, there may be more than one item with the same priority.
A variant of algorithm 3.2 Sorted Array Search by Gonnet [2, pp. 31-32]
is used.
When the rear of theQueue
is already at its maximum allowed size the exception overflow is raised
and theQueue remains unchanged.  Likewise, given an undefined queue
raises the exception of the same name. Complexity: O(n).
*)

PROCEDURE Arrive  (VAR theQueue : Queue     (*-- inout *);
                       theItem  : Item      (*-- in    *));

VAR   index        : CARDINAL; (*-- loop index over items *)
      itemPriority : Priority; (*-- item's priority - retrieved once *)

BEGIN
  queueError := noerr;
  IF (theQueue = NIL) THEN
    RaiseErrIn(arrive, undefined);
  ELSE
    WITH theQueue^ DO
      IF (rear < size) THEN
        itemPriority := priority(theItem);
        index := rear;
        INC(rear);
        WHILE (index > 0) &
              (compare(priority(items[index]), itemPriority) = less) DO
          items[index + 1] := items[index];
          DEC(index);
        END (*--while*);
        items[index + 1] := theItem;
      ELSE
        RaiseErrIn(arrive, overflow);
      END (*--if*);
    END (*--with*);
  END (*--if*);
END Arrive;
(*-------------------------*)

(*
Depart removes theItem at the front of theQueue by shifting all items
down one position in the items array and deducting one from the rear
index.  Before overwriting the item being removed, it's value is
freed via the disposal routine of theQueue's data type.  If theQueue
is empty on entry to Depart the underflow exception is raised and
theQueue is not changed.  When theQueue is not defined, the undefined
exception is raised. Complexity: O(n).
*)

PROCEDURE Depart  (VAR theQueue : Queue     (*-- inout *));

VAR   index : CARDINAL;    (*-- loop index over items *)
      free  : DisposeProc; (*-- item disposal routine *)

BEGIN
  queueError := noerr;
  IF (theQueue = NIL) THEN
    RaiseErrIn(depart, undefined);
  ELSE
    WITH theQueue^ DO
      IF (rear = 0) THEN
        RaiseErrIn(depart, underflow);
      ELSE
        free := DisposeOf(dataID);
        free(items[MIN(QueueSize)]);
        FOR index := MIN(QueueSize) + 1 TO rear DO
          items[index - 1] := items[index];
        END (*--for*);
        DEC(rear);
      END (*--if*);
    END (*--with*);
  END (*--if*);
END Depart;
(*-------------------------*)

(*
Leave removes theItem from the Queue regardless of its position within
the queue.  Complexity: O(n).
*)

PROCEDURE Leave   (VAR theQueue : Queue     (*-- inout *);
                       theItem  : Item      (*-- in    *));

VAR   index : CARDINAL;    (*-- loop index over items *)
      free  : DisposeProc; (*-- item disposal routine *)

BEGIN
  queueError := noerr;
  IF (theQueue = NIL) THEN
    RaiseErrIn(leave, undefined);
  ELSE
    WITH theQueue^ DO
      IF (rear = 0) THEN
        RaiseErrIn(leave, underflow);
      ELSE
        free := DisposeOf(dataID);
        index := MIN(QueueSize);
        WHILE (index <= rear) & (items[index] # theItem) DO
          INC(index);
        END (*-- while*);
        IF (index <= rear) THEN
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
12.2.4 Selectors

IsDefined verifies to the best of its ability whether theQueue has been
created and is still an active object. Complexity: O(1).
*)

PROCEDURE IsDefined (    theQueue : Queue    (*-- in    *))
                                  : BOOLEAN  (*-- out   *);
BEGIN
  RETURN theQueue # NIL;
END IsDefined;
(*-------------------------*)

(*
IsEmpty returns True if theQueue is in the empty state, as indicated by
the rear being zero, and False otherwise.  As per the specification
(section 8.?) undefined queues are considered empty. Complexity: O(1).
*)

PROCEDURE IsEmpty   (    theQueue : Queue    (*-- in    *))
                                  : BOOLEAN  (*-- out   *);
BEGIN
  queueError := noerr;
  IF (theQueue # NIL) THEN
    RETURN (theQueue^.rear = 0);
  END (*--if*);
  RaiseErrIn(isempty, undefined);
  RETURN TRUE;
END IsEmpty;
(*-------------------------*)

(*
IsEqual compares the left and right queues for equality, which in this
context means they contain the same items and the same data type ID. The
defined size of the queues is not relevant for the equality test.  Both
queues must be defined and have the same data type ID; if they don not,
then the exceptions undefined and typeerror are raised, respectively.
Obviously, queues of different lengths (indicated by the value of rear)
cannot be equal since there would be at least one item different between
them.  The algorithm simply loops over each of the items in both queues
returning false immediately upon encountering an inequality, and returning
true if and only if every item is the same between them. Complexity: O(n).
*)

PROCEDURE IsEqual   (    left     : Queue    (*-- in    *);
                         right    : Queue    (*-- in    *))
                                  : BOOLEAN  (*-- out   *);

VAR   index : CARDINAL; (*-- loop index over items *)
      compareItems : CompareProc;   (*-- item comparison routine *)

BEGIN
  queueError := noerr;
  IF (left = NIL) OR (right = NIL) THEN
    RaiseErrIn(isequal, undefined);
  ELSIF (left^.dataID # right^.dataID) THEN
    RaiseErrIn(isequal, typeerror);
  ELSIF (left^.rear = right^.rear) THEN
    WITH left^ DO
      compareItems := CompareOf(left^.dataID);
      FOR index := MIN(QueueSize) TO rear DO
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
also the length of theQueue. Complexity: O(1).
*)

PROCEDURE LengthOf  (    theQueue : Queue    (*-- in    *))
                                  : CARDINAL (*-- out   *);
BEGIN
  queueError := noerr;
  IF (theQueue # NIL) THEN
    RETURN theQueue^.rear;
  END (*--if*);
  RaiseErrIn(lengthof, undefined);
  RETURN 0;
END LengthOf;
(*-------------------------*)

(*
SizeOf and TypeOf simply return the size and dataID for the given queue.
Undefined queues, as always, raise the undefined exception and return
reasonable values (zero and the NullType, respectively). The complexity
of both routines is O(1).
*)

PROCEDURE SizeOf    (    theQueue : Queue    (*-- in    *))
                                  : CARDINAL (*-- out   *);
BEGIN
  queueError := noerr;
  IF (theQueue # NIL) THEN
    RETURN theQueue^.size;
  END (*--if*);
  RaiseErrIn(sizeof, undefined);
  RETURN 0;
END SizeOf;
(*-------------------------*)

PROCEDURE TypeOf    (    theQueue : Queue    (*-- in    *))
                                  : TypeID   (*-- out   *);
BEGIN
  queueError := noerr;
  IF (theQueue # NIL) THEN
    RETURN theQueue^.dataID;
  END (*--if*);
  RaiseErrIn(typeof, undefined);
  RETURN NullType;
END TypeOf;
(*-------------------------*)

(*
FrontOf returns the value of the item that is at the front of theQueue or
the NullItem if theQueue is undefined or is empty. Complexity: O(1).
*)

PROCEDURE FrontOf   (    theQueue : Queue    (*-- in    *))
                                  : Item     (*-- out   *);
BEGIN
  queueError := noerr;
  IF (theQueue = NIL) THEN
    RaiseErrIn(frontof, undefined);
  ELSIF (theQueue^.rear = 0) THEN
    RaiseErrIn(frontof, underflow);
  ELSE
    RETURN theQueue^.items[MIN(QueueSize)];
  END (*--if*);
  RETURN NullItem;
END FrontOf;
(*-------------------------*)

(*
PositionOf returns the number of positions from the given item to the
front of the queue. If theItem is not present in theQueue then zero
is returned.
*)

PROCEDURE PositionOf (    theQueue: Queue    (*-- in    *);
                          theItem : Item     (*-- in    *))
                                  : CARDINAL (*-- out   *);

VAR   index : CARDINAL; (*-- loop index over items *)

BEGIN
  queueError := noerr;
  IF (theQueue = NIL) THEN
    RaiseErrIn(positionof, undefined);
  ELSE
    WITH theQueue^ DO
      FOR index := MIN(QueueSize) TO rear DO
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
12.2.5 Iterators

Both LoopOver and Traverse simply loop through each of the queue items
from the front to the rear passing the item value to theProcess
procedure parameter.  LoopOver may terminate before reaching the rear
if theProcess returns False. Complexity: O(n).
*)

PROCEDURE LoopOver (    theQueue  : Queue          (*-- in    *);
                        theProcess: LoopAccessProc (*-- in    *));

VAR   index : CARDINAL; (*-- loop index over items *)

BEGIN
  queueError := noerr;
  IF (theQueue = NIL) THEN
    RaiseErrIn(loopover, undefined);
  ELSE
    WITH theQueue^ DO
      FOR index := MIN(QueueSize) TO rear DO
        IF ~theProcess(items[index]) THEN
          RETURN;
        END (*--if*);
      END (*--for*);
    END (*--with*);
  END (*--if*);
END LoopOver;
(*-------------------------*)

PROCEDURE Traverse (    theQueue  : Queue          (*-- in    *);
                        theProcess: AccessProc     (*-- in    *));

VAR   index : CARDINAL; (*-- loop index over items *)

BEGIN
  queueError := noerr;
  IF (theQueue = NIL) THEN
    RaiseErrIn(traverse, undefined);
  ELSE
    WITH theQueue^ DO
      FOR index := MIN(QueueSize) TO rear DO
        theProcess(items[index]);
      END (*--for*);
    END (*--with*);
  END (*--if*);
END Traverse;
(*-------------------------*)


(*
12.2.6 Module Initialization

The module's local variables are initialized to known states.
queueError is used to fill the handlers array with a routine
that will exit the program when an exception is raised (saving the
declaration of a special loop control variable for this purpose).
The condition noerr is given the NullHandler which is presumed to
do nothing.  Applying MIN and MAX to cover all exceptions followed
by resetting the handler for noerr ensures that this initialization
will be unaffected by any future changes to the number of Exceptions
or their order of declaration within the enumeration.

Since a FOR loop control variable is undefined following the loop,
queueError must be set to indicate that an error has not yet occurred.
*)

BEGIN
  FOR queueError := MIN(Exceptions) TO MAX(Exceptions) DO
    SetHandler(queueError, ExitOnError);
  END (*--for*);
  SetHandler(noerr, NullHandler);
  queueError := noerr;
END QPBSBMI.


(*
References

[1] Booch
[2] G.H. Gonnet, Handbook of Algorithms and Data Structures,
    Addison-Wesley, London England, 1984.
*)