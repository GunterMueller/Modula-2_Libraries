(*
15.3 Unbounded Balking Deque Implementation
*)

IMPLEMENTATION MODULE DQNBSUMI;
(*===========================================================
    Version  : 1.00  16 May 1989  C. Lins
    Compiler : TopSpeed Modula-2
    Component: Monolithic Structures - Deque (Opaque version)
      Non-Priority Balking Sequential Unbounded Managed Iterator

    REVISION HISTORY
    v1.00  16 May 1989  C. Lins:
	  Initial TopSpeed implementation.
    (C) Copyright 1989 Charles A. Lins
===========================================================*)

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
    (*--Type*) Operations, Exceptions, DequeComponentID;

FROM TypeManager IMPORT
    (*--Cons*) NullType,
    (*--Type*) TypeID,
	(*--Proc*) AssignOf, DisposeOf, CompareOf;

    (*--------------------*)

(*
15.3.1 Internal Unbounded Deque Representation

For the internal representation of the unbounded deque we shal use a linked list
of nodes with a header.  The header contains pointers to the front and back nodes of the
deque and the current length of the deque.  Each node contains the item value, and
pointers forward and backward to the successor and predecessor nodes respectively.
By employing a doubly-linked list efficient insertion and removal at either end of
the deque is facilitated, as is traversal of the deque in either direction.
By convention, an empty deque will be represented by both the front and back
pointers being NIL.  Figure 15.2 displays the internal structure graphically.

FIGURE 15.2

Representation Invariants:
* when the deque is empty both head and tail are NIL
*)

TYPE  Link = POINTER TO Node;
TYPE  Node  = RECORD
	prev : Link;  (*-- link to prior deque element *)
	item : Item;  (*-- deque element's data *)
	next : Link;  (*-- link to next deque element *)
      END (*-- Node *);
  
TYPE  Deque = POINTER TO UnboundedDeque;

TYPE  UnboundedDeque = RECORD
	dataID : TypeID;    (*-- defined data type *)
	length : CARDINAL;  (*-- current # of items *)
	head   : Link;      (*-- link to front of deque *)
	tail   : Link;      (*-- link to rear of deque *)
      END (*-- UnboundedDeque *);

    (*--------------------*)


(*
15.3.2 Exceptions

dequeError holds the exception result from the most recently
invoked operation of this module. The Exceptions enumeration
constant noerr indicates successful completion of the operation and
all operations that may raise an exception assign this value to
dequeError before any other processing.

The handlers array holds the current exception handler for the
possible exceptions that may be raised from within this module.

Both are initialized by the module initialization (see section
15.3.6).

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
15.3.3 Constructors

Create attempts to form a new, empty bounded deque object associated
with the given data type ID and maximum size.  Only the specified
number of entries given in theSize are actually allocated to the
ItemsArray (items).  MacSystem.Allocate returns NIL if it is unable
to successfully complete the allocation request whereupon the overflow
exception is raised and the NullDeque returned.  If successful, the
deque is initialized as follows before returning the new deque:
* theType is saved for later use by IsEqual, Clear, and Assign
* head, tail and length are set to the empty deque state.
Complexity: O(1).
*)

PROCEDURE Create  (    theType  : TypeID    (*-- in    *))
	: Deque     (*-- out   *);

VAR   newDeque : Deque;

BEGIN
  Allocate(newDeque, SIZE(UnboundedDeque));
  IF (newDeque = NIL) THEN
    RaiseErrIn(create, overflow);
  ELSE
    WITH newDeque^ DO
      dataID := theType;
	  length := 0;
	  head   := NIL;
	  tail   := NIL;
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
BEGIN
  Clear(theDeque);
  IF (dequeError = noerr) THEN
    Deallocate(theDeque, SIZE(theDeque^));
  END (*--if*);
END Destroy;
(*-------------------------*)

(*
Clear retrieves the item disposal routine for theDeque's data type, if
any, and proceeds to free each item in theDeque.  If theDeque is empty
the loop is not executed as head will be NIL.  Lastly, tail and length
are set to ensure theDeque is in the empty state. Complexity: O(n).
*)

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

(*
Assign duplicates the items of theDeque to the target deque, toDeque. An
undefined source deque raises an exception and leaves the target unchanged.
If the target deque is undefined, it is created with the same data type
attribute of the source; otherwise the target deque is cleared of its
present contents and its data type is altered to reflect that of the
source.  If the source deque is not empty, Assign copies each source node
to the target using the TailInsert routine from the doubly-linked unbounded
list module.  TailInsert automatically sets the new node's previous link
to the last node inserted. In order for this to work correctly last must be
initialized to NIL.
Complexity: O(mn), where m is the number of items in the source deque and
n is the number of items in the target deque.
*)

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
      toDeque := Create(theDeque^.dataID);
    ELSE
      Clear(toDeque);
      toDeque^.dataID := theDeque^.dataID;
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

(*
Arrive adds theItem to theEnd of theDeque by advancing the rear index
value and storing theItem at that location.  If the routine is given an
undefined deque the exception of the same name is raised.
If, upon entry, the deque is already empty, a new node is attached to
both the head and tail, regardless of the value of location.  Other-
wise, when theEnd is the front, a new node is added in the same manner
as the Insert operation for a doubly-linked list; when location is the back,
the new node is added to the end of the deque using the TailInsert routine
for a doubly-linked list. Complexity: O(1).
*)

PROCEDURE Arrive  (VAR theDeque : Deque     (*-- inout *);
       	       	       theItem  : Item      (*-- in    *);
					   theEnd   : Location  (*-- in    *));

VAR   newNode : Link;

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
	IF (head = NIL) THEN
	head := newNode;
		  tail := newNode;
	ELSIF (theEnd = front) THEN
		  head^.prev := newNode;
		  newNode^.next := head;
		  head := newNode;
	ELSE
	tail^.next := newNode;
		  newNode^.prev := tail;
		  tail := newNode;
	END (*--if*);
      END (*--with*);
    END (*--if*);
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
Complexity: O(1).
*)

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

(*
Leave removes theItem from theDeque regardless of its position within
the deque.  When theEnd is front, the first matching item is removed
from the deque; conversely, when theEnd is back the last it is the last
matching item that is removed.  At the end of the search for theItem, index
points to the node to be removed from the deque or is NIL if theItem is not
present.
Complexity: O(n).
*)

PROCEDURE Leave   (VAR theDeque : Deque     (*-- inout *);
       	       	       theItem  : Item      (*-- in    *);
					   theEnd   : Location  (*-- in    *));

VAR   index : Link;	(*-- loop index over items *)
      free  : DisposeProc; (*-- item disposal routine *)

BEGIN
  dequeError := noerr;
  IF (theDeque = NIL) THEN
    RaiseErrIn(leave, undefined);
  ELSE
    WITH theDeque^ DO
      IF (head = NIL) THEN
	RaiseErrIn(leave, underflow);
      ELSE
	    free := DisposeOf(dataID);
		CASE theEnd OF
		  front:
		    index := head;
		    WHILE (index # NIL) & (index^.item # theItem) DO
		      index := index^.next;
		    END (*-- while*);
	    | back :
		    index := tail;
		    WHILE (index # NIL) & (index^.item # theItem) DO
		      index := index^.prev;
		    END (*-- while*);
		END (*--case*);
		IF (index # NIL) THEN
		  WITH index^ DO
	free(item);
		    IF (prev # NIL) THEN
		      prev^.next := next;
		    END (*--if*);
		    IF (next # NIL) THEN
		      next^.prev := prev;
		    END (*--if*);
		  END (*--with*);
		  IF (index = head) THEN
		    head := index^.next;
		  END (*--if*);
		  IF (index = tail) THEN
		    tail := index^.prev;
		  END (*--if*);
		  Deallocate(index, SIZE(index^));
	DEC(length);
		ELSE
	RaiseErrIn(leave, notfound);
	END (*--if*);
      END (*--if*);
    END (*--with*);
  END (*--if*);
END Leave;
(*-------------------------*)


(*
15.3.4 Selectors

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
(section 9.4) undefined deques are considered empty. Complexity: O(1).
*)

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

VAR   leftIndex  : Link;  (*-- loop index of left deque *)
      rightIndex : Link;  (*-- loop index of right deque *)
	  compare    : CompareProc;	(*-- item comparison routine *)

BEGIN
  dequeError := noerr;
  IF (left = NIL) OR (right = NIL) THEN
    RaiseErrIn(isequal, undefined);
  ELSIF (left^.dataID # right^.dataID) THEN
    RaiseErrIn(isequal, typeerror);
  ELSIF (left^.length = right^.length) THEN
	compare := CompareOf(left^.dataID);
    leftIndex  := left^.head;
    rightIndex := right^.head;
    WHILE (leftIndex # NIL) DO
      IF (rightIndex = NIL) OR 
	  	 (compare(leftIndex^.item, rightIndex^.item) # equal) THEN
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

(*
LengthOf simply returns the length stored in the deque header. Complexity: O(1).
*)

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

(*
TypeOf simply returns the dataID for the given deque. Undefined deques,
as always, raise the undefined exception and return a reasonable value,
in this case the NullType. Complexity O(1).
*)

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
  ELSIF (theDeque^.head = NIL) THEN
    RaiseErrIn(frontof, underflow);
  ELSE
    RETURN theDeque^.head^.item;
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
  ELSIF (theDeque^.tail = NIL) THEN
    RaiseErrIn(rearof, underflow);
  ELSE
    RETURN theDeque^.tail^.item;
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

(*
PositionOf returns the number of positions from the given item to the
front of the deque where the front is considered position one (1).
If theItem is not present in theDeque then zero is returned.
Complexity O(n).
*)

PROCEDURE PositionOf (    theDeque: Deque    (*-- in    *);
       	       		theItem : Item     (*-- in    *))
							: CARDINAL (*-- out   *);

VAR   index : Link;     (*-- loop index over items *)
      count : CARDINAL; (*-- running value of position *)

BEGIN
  dequeError := noerr;
  IF (theDeque = NIL) THEN
    RaiseErrIn(positionof, undefined);
  ELSE
    count := 1;
	index := theDeque^.head;
    WHILE (index # NIL) DO
	  IF (theItem = index^.item) THEN
		RETURN count;
      END (*--if*);
	  INC(count);
	  index := index^.next;
    END (*--while*);
  END (*--if*);
  RETURN 0;
END PositionOf;
(*-------------------------*)


(*
15.3.5 Iterators

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

VAR   index : Link; (*-- loop index over items *)

BEGIN
  dequeError := noerr;
  IF (theDeque = NIL) THEN
    RaiseErrIn(loopover, undefined);
  ELSIF (theEnd = front) THEN
	index := theDeque^.head;
    WHILE (index # NIL) DO
      IF ~theProcess(index^.item) THEN
		RETURN;
	  END (*--if*);
	  index := index^.next;
    END (*--while*);
  ELSIF (theEnd = back) THEN
	index := theDeque^.tail;
    WHILE (index # NIL) DO
      IF ~theProcess(index^.item) THEN
		RETURN;
	  END (*--if*);
	  index := index^.prev;
    END (*--while*);
  END (*--if*);
END LoopOver;
(*-------------------------*)

PROCEDURE Traverse (    theDeque  : Deque	(*-- in    *);
	theProcess: AccessProc     (*-- in    *);
						theEnd    : Location       (*-- in    *));

VAR   index : Link; (*-- loop index over items *)

BEGIN
  dequeError := noerr;
  IF (theDeque = NIL) THEN
    RaiseErrIn(traverse, undefined);
  ELSIF (theEnd = front) THEN
	index := theDeque^.head;
    WHILE (index # NIL) DO
      theProcess(index^.item);
	  index := index^.next;
    END (*--while*);
  ELSIF (theEnd = back) THEN
	index := theDeque^.tail;
    WHILE (index # NIL) DO
      theProcess(index^.item);
	  index := index^.prev;
    END (*--while*);
  END (*--if*);
END Traverse;
(*-------------------------*)


(*
15.3.6 Module Initialization

The module's local variables are initialized to known states.
dequeError is used to fill the handlers array with a routine
that will exit the program when an exception is raised (saving the
declaration of a special loop control variable for this purpose).
The condition noerr is given the NullHandler which is presumed to
do nothing.  Applying MIN and MAX to cover all exceptions followed
by resetting the handler for noerr ensures that this initialization
will be unaffected by any future changes to the number of Exceptions
or their order of declaration within the enumeration.  Since a FOR loop
control variable is undefined following the loop, dequeError must be
set to indicate that an error has not yet occurred.
*)

BEGIN
  FOR dequeError := MIN(Exceptions) TO MAX(Exceptions) DO
    SetHandler(dequeError, ExitOnError);
  END (*--for*);
  SetHandler(noerr, NullHandler);
  dequeError := noerr;
END DQNBSUMI.

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