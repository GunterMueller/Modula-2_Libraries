(*
11.2 Unbounded Queue Implementation
*)

IMPLEMENTATION MODULE QSUMI;
(*==============================================================
    Version  : 1.00  03 May 1989  C. Lins
    Compiler : TopSpeed Modula-2
    Code Size: R-  bytes
    Component: Monolithic Structures - Queue (Opaque version)
      Non-priority Non-balking Sequential Unbounded Managed Iterator

    REVISION HISTORY
    v1.00  03 May 1989  C. Lins
      Initial TopSpeed Modula-2 implementation.

    (C) Copyright 1989 Charles A. Lins
==============================================================*)

FROM JPIStorage IMPORT
    (*--Proc*) Allocate, Deallocate;

FROM ErrorHandling IMPORT
    (*--Type*) HandlerProc,
    (*--Proc*) Raise, NullHandler, ExitOnError;

FROM Items IMPORT
    (*--Cons*) NullItem,
    (*--Type*) Item, AssignProc, DisposeProc, CompareProc,
               AccessProc, LoopAccessProc;

FROM Relations IMPORT
    (*--Type*) Relation;

FROM QEnum IMPORT
    (*--Type*) Operations, Exceptions, ComponentID;

FROM TypeManager IMPORT
    (*--Cons*) NullType,
    (*--Type*) TypeID,
    (*--Proc*) AssignOf, DisposeOf, CompareOf;

    (*--------------------*)

(*
11.2.1 Internal Unbounded Queue Representation

Uses a linked list of nodes with a header, containing pointers
to the front and the back.  The second pointer isn't absolutely
necessary.  To add an item to the queue, the list could be
traversed from the front until the end is reachedand then the
item added.  For efficiency reasons, the back pointer is used
so items can be added directly, making the traversal unnecessary.
Instead of O(n) time complexity, insertion is O(1).
    
By convention, an empty queue will be represented by both the
front and back pointers being NIL.

Figure 11.1,
below, graphically depicts the internal structure used for the
unbounded queue:

FIGURE 11.1

Representation Invariants:
¥ an empty queue is repesented by length = 0, head = NIL, and tail = NIL
¥ when there is one item in the queue, length = 1, head = tail ­ NIL
*)

TYPE  Link = POINTER TO Node;
TYPE  Node = RECORD
        item : Item; (*-- item data *)
        next : Link; (*-- link to next node *)
      END (*-- Node *);

TYPE  UnboundedQueue = RECORD
        dataID : TypeID;     (*-- defined data type *)
        length : CARDINAL;   (*-- current # of items *)
        head   : Link;       (*-- pointer to first item *)
        tail   : Link;       (*-- pointer to last item *)
      END (*-- UnboundedQueue *);

TYPE  Queue = POINTER TO UnboundedQueue;

    (*--------------------*)


(*
11.2.2 Exceptions

queueError holds the exception result from the most recently
invoked operation of this module. The Exceptions enumeration
constant noerr indicates successful completion of the operation and
all operations that may raise an exception assign this value to
queueError before any other processing.

The handlers array holds the current exception handler for the
possible exceptions that may be raised from within this module.

Both are initialized by the module initialization (see section
11.2.6).

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
11.2.3 Constructors

Create attempts to form a new, empty unbounded queue object associated
with the given data type ID.  MacSystem.Allocate returns NIL if it is
unable to successfully complete the allocation request whereupon the
overflow exception is raised and the NullQueue returned.
If successful, the
queue is initialized as follows before returning the new queue:
¥ theType is saved for later use by IsEqual, Assign, and Clear
¥ the head and tail links are set to the empty queue state (NIL),
¥ the current length is set to the empty queue state (zero).
Complexity: O(1).
*)

PROCEDURE Create  (    theType  : TypeID    (*-- in    *))
                                : Queue     (*-- out   *);

VAR   newQueue : Queue;

BEGIN
  queueError := noerr;
  Allocate(newQueue, SIZE(UnboundedQueue));
  IF (newQueue = NIL) THEN
    RaiseErrIn(create, overflow);
  ELSE
    WITH newQueue^ DO
      dataID := theType;
      length := 0;
      head   := NIL;
      tail   := NIL;
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
value of the NullQueue).  Complexity: O(1).
*)

PROCEDURE Destroy (VAR theQueue : Queue     (*-- inout *));
BEGIN
  Clear(theQueue);
  IF (queueError = noerr) THEN
    Deallocate(theQueue, SIZE(theQueue^));
  END (*--if*);
END Destroy;
(*-------------------------*)

(*
Clear retrieves the item disposal routine for theQueue's data type, if
any, and proceeds to free each item in theQueue.  If theQueue is empty
the loop is not executed as head already be NIL.  Lastly, tail and
length are set to ensure theQueue is in the empty state.
Complexity: O(n).
*)

PROCEDURE Clear   (VAR theQueue : Queue     (*-- inout *));

VAR   oldHead : Link;        (*-- item node to deallocate *)
      free    : DisposeProc; (*-- item disposal routine *)

BEGIN
  queueError := noerr;
  IF (theQueue # NIL) THEN
    WITH theQueue^ DO
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

(*
Assign duplicates the items of theQueue to the target queue, toQueue. An
undefined source queue raises an exception and leaves the target unchanged.
If the target queue is undefined, it is created with the same
data type attribute of the source; otherwise the target queue is
cleared of its present contents and its data type is altered to reflect
that of the source.  Essentially, Assign traverses the links of the
source queue copying items to new nodes created in the target queue.
If a new node cannot be allocated the overflow exception is raised and
the routine is aborted.  Complexity: O(n).
*)

PROCEDURE Assign  (    theQueue : Queue     (*-- in    *);
                   VAR toQueue  : Queue     (*-- inout *));

VAR   fromIndex  : Link;       (*-- loop index over source nodes *)
      assignment : AssignProc; (*-- item assignment routine *)

BEGIN
  queueError := noerr;
  IF (theQueue = NIL) THEN
    RaiseErrIn(assign, undefined);
  ELSIF (theQueue # toQueue) THEN
    IF (toQueue = NIL) THEN
      toQueue := Create(theQueue^.dataID);
    ELSE
      Clear(toQueue);
      toQueue^.dataID := theQueue^.dataID;
    END (*--if*);
    IF (queueError = noerr) & (theQueue^.head # NIL) THEN
      assignment := AssignOf(theQueue^.dataID);
      Allocate(toQueue^.head, SIZE(Node));
      IF (toQueue^.head = NIL) THEN
        RaiseErrIn(assign, overflow);
      ELSE
        WITH toQueue^ DO
          WITH head^ DO
            item := assignment(theQueue^.head^.item);
            next := NIL;
          END (*--with*);
          fromIndex := theQueue^.head;
          tail := head;
          WHILE (fromIndex^.next # NIL) DO
            fromIndex := fromIndex^.next;
            WITH tail^ DO
              Allocate(next, SIZE(Node));
              IF (next = NIL) THEN
                RaiseErrIn(assign, overflow);
                RETURN;
              END (*--if*);
              next^.item := assignment(fromIndex^.item);
              next^.next := NIL;
            END (*--with*);
            tail := tail^.next;
          END (*--while*);
          length := theQueue^.length;
        END (*--with*);
      END (*--if*);
    END (*--if*);
  END (*--if*);
END Assign;
(*-------------------------*)

(*
Arrive adds theItem to the rear of theQueue by first allocating a new
node, storing theItem in that node and then update the links head
and tail, and the queue length. Note that tail will always point to
the new node upon successful completion of the routine.
When the node cannot be allocated the exception overflow is raised
and theQueue remains unchanged.  Likewise, given an undefined queue
raises the exception of the same name.  Complexity: O(1).
*)

PROCEDURE Arrive  (VAR theQueue : Queue     (*-- inout *);
                       theItem  : Item      (*-- in    *));

VAR   newNode : Link; (*-- temporary for new node *)

BEGIN
  queueError := noerr;
  IF (theQueue = NIL) THEN
    RaiseErrIn(arrive, undefined);
  ELSE
    Allocate(newNode, SIZE(Node));
    IF (newNode = NIL) THEN
      RaiseErrIn(arrive, overflow);
    ELSE
      WITH newNode^ DO
        item := theItem;
        next := NIL;
      END (*--with*);
      WITH theQueue^ DO
        INC(length);
        IF (head = NIL) THEN
          head := newNode;
        ELSE
          tail^.next := newNode;
        END (*--if*);
        tail := newNode;
      END (*--with*);
    END (*--if*);
  END (*--if*);
END Arrive;
(*-------------------------*)

(*
Depart removes theItem at the front of theQueue by relinking the
successor to the current queue head as the new head.  The departing
item is freed via the disposal routine of theQueue's data type before
actually releasing the old head node itself.

If theQueue is empty on entry to Depart the underflow exception is
raised and theQueue is not changed.  When theQueue is not defined,
the undefined exception is raised.  Complexity: O(1).
*)

PROCEDURE Depart  (VAR theQueue : Queue     (*-- inout *));

VAR   oldHead : Link;        (*-- original head, departing *)
      free    : DisposeProc; (*-- item disposal routine *)

BEGIN
  queueError := noerr;
  IF (theQueue = NIL) THEN
    RaiseErrIn(depart, undefined);
  ELSE
    WITH theQueue^ DO
      IF (tail = NIL) THEN
        RaiseErrIn(depart, underflow);
      ELSE
        oldHead := head;
        head    := head^.next;
        IF (head = NIL) THEN
          tail := NIL;
        END (*--if*);
        DEC(length);
        free := DisposeOf(dataID);
        free(oldHead^.item);
        Deallocate(oldHead, SIZE(oldHead^));
      END (*--if*);
    END (*--with*);
  END (*--if*);
END Depart;
(*-------------------------*)


(*
11.2.4 Selectors

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
the head being NIL, and False otherwise.  As per the specification
(section 9.?) undefined queues are considered empty.  Complexity: O(1).
*)

PROCEDURE IsEmpty   (    theQueue : Queue    (*-- in    *))
                                  : BOOLEAN  (*-- out   *);
BEGIN
  queueError := noerr;
  IF (theQueue # NIL) THEN
    RETURN (theQueue^.head = NIL);
  END (*--if*);
  RaiseErrIn(isempty, undefined);
  RETURN TRUE;
END IsEmpty;
(*-------------------------*)

(*
IsEqual compares the left and right queues for equality, which in this
context means they contain the same items and the same data type ID.
Both queues must be defined and have the same data type ID; if they 
do not, then the exceptions undefined and typeerror are raised,
respectively.
Obviously, queues of different lengths (indicated by the lengths)
cannot be equal since there would be at least one item different between
them.  The algorithm simply loops over each of the items in both queues
returning false immediately upon encountering an inequality, and returning
true if and only if every item is the same between them. Note that
because the lengths are equal, both index links will become NIL
simultaneously.  Complexity: O(n).
*)

PROCEDURE IsEqual   (    left     : Queue    (*-- in    *);
                         right    : Queue    (*-- in    *))
                                  : BOOLEAN  (*-- out   *);

VAR   leftIndex : Link; (*-- loop index over left items *)
      rightIndex: Link; (*-- loop index over right items *)
      compare : CompareProc;    (*-- item comparison routine *)

BEGIN
  queueError := noerr;
  IF (left = NIL) OR (right = NIL) THEN
    RaiseErrIn(isequal, undefined);
  ELSIF (left^.dataID # right^.dataID) THEN
    RaiseErrIn(isequal, typeerror);
  ELSIF (left^.length = right^.length) THEN
    compare := CompareOf(left^.dataID);
    leftIndex := left^.head;
    rightIndex:= right^.head;
    WHILE (leftIndex # NIL) DO
      IF compare(leftIndex^.item, rightIndex^.item) # equal THEN
        RETURN FALSE;
      END (*--if*);
      leftIndex  := leftIndex^.next;
      rightIndex := rightIndex^.next;
    END (*--while*);
    RETURN TRUE;
  END (*--if*);
  RETURN FALSE;
END IsEqual;
(*-------------------------*)

(*
LengthOf simply returns the length of theQueue.  Complexity: O(1).
*)

PROCEDURE LengthOf  (    theQueue : Queue    (*-- in    *))
                                  : CARDINAL (*-- out   *);
BEGIN
  queueError := noerr;
  IF (theQueue # NIL) THEN
    RETURN theQueue^.length;
  END (*--if*);
  RaiseErrIn(lengthof, undefined);
  RETURN 0;
END LengthOf;
(*-------------------------*)

(*
TypeOf simply returns the dataID for the given queue.
Undefined queues, as always, raise the undefined exception and return
a reasonable value (the NullType).  Complexity: O(1).
*)

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
FrontOf returns the value of the item that is at the head of theQueue or
the NullItem if theQueue is undefined or is empty.  Complexity: O(1).
*)

PROCEDURE FrontOf   (    theQueue : Queue    (*-- in    *))
                                  : Item     (*-- out   *);
BEGIN
  queueError := noerr;
  IF (theQueue = NIL) THEN
    RaiseErrIn(frontof, undefined);
  ELSIF (theQueue^.head = NIL) THEN
    RaiseErrIn(frontof, underflow);
  ELSE
    RETURN theQueue^.head^.item;
  END (*--if*);
  RETURN NullItem;
END FrontOf;
(*-------------------------*)


(*
11.2.5 Iterators

Both LoopOver and Traverse simply loop through each of the queue items
from the head to the tail passing the item value to theProcess
procedure parameter.  LoopOver may terminate before reaching the tail
if theProcess returns False.  Complexity: O(n).
*)

PROCEDURE LoopOver (    theQueue  : Queue          (*-- in    *);
                        theProcess: LoopAccessProc (*-- in    *));

VAR   index : Link; (*-- loop index over items *)

BEGIN
  queueError := noerr;
  IF (theQueue = NIL) THEN
    RaiseErrIn(loopover, undefined);
  ELSE
    WITH theQueue^ DO
      index := head;
      WHILE (index # NIL) DO
        IF ~theProcess(index^.item) THEN
          RETURN;
        END (*--if*);
        index := index^.next;
      END (*--while*);
    END (*--with*);
  END (*--if*);
END LoopOver;
(*-------------------------*)

PROCEDURE Traverse (    theQueue  : Queue          (*-- in    *);
                        theProcess: AccessProc     (*-- in    *));

VAR   index : Link; (*-- loop index over items *)

BEGIN
  queueError := noerr;
  IF (theQueue = NIL) THEN
    RaiseErrIn(traverse, undefined);
  ELSE
    WITH theQueue^ DO
      index := head;
      WHILE (index # NIL) DO
        theProcess(index^.item);
        index := index^.next;
      END (*--while*);
    END (*--with*);
  END (*--if*);
END Traverse;
(*-------------------------*)


(*
11.2.6 Module Initialization

The module's local variables are initialized to known states.
queueError is used to fill the handlers array with a routine
that will exit the program when an exception is raised (saving the
declaration of a special loop control variable for this purpose).
The condition noerr is given the NullHandler which is presumed to
do nothing.  Applying MIN and MAX to cover all exceptions followed
by resetting the handler for noerr ensures that this initialization
will be unaffected by any future changes to the number of Exceptions
or their order of declaration within the enumeration.  Since a FOR
loop control variable is undefined following the loop,
queueError must be set to indicate that an error has not yet occurred.
*)

BEGIN
  FOR queueError := MIN(Exceptions) TO MAX(Exceptions) DO
    SetHandler(queueError, ExitOnError);
  END (*--for*);
  SetHandler(noerr, NullHandler);
  queueError := noerr;
END QSUMI.

(*
References


[1] A. Aho, J. Hopcroft, and J. Ullman, Data Structures and Algorithms,
    Addison-Wesley, Reading, MA 1983, pp. 56-60.
[2] G. Booch, Software Components in Ada Structures, Tools, and Subsystems,
    Benjamin/Cummings, Menlo Park, CA 1987, pp. 92-93, 142-153.
[3] K. John Gough, ÒWriting Generic Utilities in Modula-2Ó, Journal of
    Pascal, Ada, and Modula-2, Vol. 5(3), (May/June 1986), pp 53-62.
[4] T.A. Standish, Data Structure Techniques, Chapter 2: Stacks and Queues,
    Addison-Wesley, Reading, MA 1980, pp. 20-23, 28-32.
[5] R.S. Wiener and G.A. Ford, Modula-2 A Software Development Approach,
    John Wiley & Sons, New York, NY 1985, pp. 247-253
[6] R.S. Wiener and R.F. Sincovec, Data Structures Using Modula-2,
    John Wiley & Sons, New York, NY 1986, pp. 69-71
*)
