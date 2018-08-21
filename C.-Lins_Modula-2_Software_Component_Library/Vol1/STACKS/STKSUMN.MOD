
(*
7.4	StackSUMN Implementation
*)


IMPLEMENTATION MODULE StkSUMN;
(*===========================================================
    Version  : 1.00  28 Apr 1989  C. Lins
    Compiler : TopSpeed Modula-2 Compiler
    Code Size: R-  bytes
    Component: Monolithic Structures - Stack (Opaque)
               Sequential Unbounded Managed Non-Iterator

    INTRODUCTION
    This module provides the implementation for the operations
    of the unbounded stack abstract data type.

    REVISION HISTORY
    v1.00  28 Apr 1989 C. Lins:
      Initial implementation
===========================================================*)

FROM JPIStorage IMPORT
    (*--Proc*) Allocate, Deallocate;

FROM Items IMPORT
    (*--Cons*) NullItem, NoDisposeProc,
    (*--Type*) Item, AssignProc, DisposeProc, CompareProc;

FROM Relations IMPORT
    (*--Type*) Relation;

FROM ErrorHandling IMPORT
    (*--Type*) HandlerProc,
    (*--Proc*) NullHandler, ExitOnError, Raise;

FROM StackEnum IMPORT
    (*--Type*) Exceptions, Operations, ComponentID;

FROM TypeManager IMPORT
    (*--Cons*) NullType,
    (*--Type*) TypeID,
    (*--Proc*) AssignOf, DisposeOf, CompareOf;


    (*--------------------*)

(*
7.4.1	Internal Unbounded Stack Representation

Figure 7.1

7.4.1.1	Stack Header

For the internal representation of an unbounded stack we will use a singly linked list of
nodes, with a header node, called UnboundedStack.  This header contains a pointer to the
topmost node on the stack, a count of the current stack depth, and the data type ID of the
stack.  An empty stack is represented by a UnboundedStack with a Top pointer of NIL
and a Depth of zero.  Where possible, a NIL value for the Stack will be accepted as a legal
value for an empty stack when this will not disrupt the semantics of the invoked
operation.


7.4.1.2	Node

Each node in the list of stack items contains the Item stored on the stack, and a link to
the node immediately beneath it.  The link for the bottommost node on the stack will
contain a value of NIL to represent the end of the stack.


7.4.1.3	Management of Free Stack Nodes

A separate list of free nodes is kept global for all items popped from the stack.  The
FreeList is effectively a FreeStack due to the LIFO management scheme employed.  This
technique is used in order to minimize calls to Allocate and Deallocate system routines,
since both (though especially Allocate) involve a rather large overhead compared to the
bounded form.


7.4.1.4	Stack – Representation of Depth

The cost to maintain the depth of each stack is relatively small, consisting of a field to
store the depth (a CARDINAL) and the execution cost to maintain this field whenever an
item is pushed or popped from the stack. The alternative would be to iterate over every
item in the stack to calculate the depth whenever this operation was requested.  The
implementation used  yields an algorithm of O(1) time complexity for determination of
the stack's depth as opposed to an O(n) algorithm at the expense of a small increase in
space complexity, and a small increase in the proportionality constants for the time
complexity of Push and Pop.


7.4.1.5	Stack – Representation of an Empty Stack

The representation of an opaque type is restricted to POINTER types [Wirth85, pg. 169].
An alternative representation for an empty stack would be to use a value of NIL for the
Stack.  Though this would simplify testing for an empty stack, it would complicate
primitive operations such as Pop and Clear, which would then require the Free operation
as a parameter.


7.4.2	Efficiency of Operations

Within the implementation of a component, we allow ourselves to violate the abstraction
by examining the internal components of the data structure.  Repetitive invocations of
procedures supporting the abstraction may be replaced with the “inline” equivalent of the
operation.  This is most commonly done when testing for the empty stack condition and
in copying items.
*)

TYPE  NodePtr = POINTER TO Node;
TYPE  Node    = RECORD   (*-- a stack node *)
        item  : Item;    (*-- generic data item *)
        next  : NodePtr; (*-- next node beneath this one, if any *)
      END (*-- Node *);

TYPE  UnboundedStack = RECORD
        dataID: TypeID;   (*-- Defined data type for this stack*)
        depth : CARDINAL; (*-- current stack depth, := 0   *)
        top   : NodePtr;  (*-- current stack top    := NIL *)
      END (*-- UnboundedStack *);

TYPE  Stack = POINTER TO UnboundedStack;

    (*-----------------------*)


(*
7.4.3	Exceptions

To support the exception handling mechanism two variables are needed.  The first,
stackError, is used to record the exception code from each operation; while handlers  is an
array of exception handling procedures indexed by the exception code.

The routines StackError, GetHandler, and SetHandler  have been previously described in
the definition module, and their operation should be readily apparent.  RaiseErrIn  is a
local routine used to set stackError and invoke the Raise  routine of the ErrorHandling
module.
*)

VAR   stackError : Exceptions;
VAR   handlers   : ARRAY Exceptions OF HandlerProc;


    (*-----------------------*)


PROCEDURE StackError ()              : Exceptions  (*-- out   *);
BEGIN
  RETURN stackError;
END StackError;
(*----------------------------*)

PROCEDURE GetHandler (    theError   : Exceptions  (*-- in    *))
                                     : HandlerProc (*-- out   *);
BEGIN
  RETURN handlers[theError];
END GetHandler;
(*----------------------------*)

PROCEDURE SetHandler (    theError   : Exceptions  (*-- in    *);
                          theHandler : HandlerProc (*-- in    *));
BEGIN
  handlers[theError] := theHandler;
END SetHandler;
(*----------------------------*)

PROCEDURE RaiseErrIn (    theRoutine : Operations  (*-- in    *);
                          theError   : Exceptions  (*-- in    *));
BEGIN
  stackError := theError;
  Raise(ComponentID + ModuleID, theRoutine, theError, handlers[theError]);
END RaiseErrIn;
(*----------------------------*)


(* ————————————————————————————————————————————————————————————————
    Ê  Local  Ê  Local  Ê  Local  Ê  Local  Ê  Local  Ê  Local  Ê
   ———————————————————————————————————————————————————————————————— *)

    MODULE FreeListMgr;

    (*-- This local module controls access to the Available node list.
      -- Version 1.00  07 Jun. 1987  C. Lins *)

    IMPORT Node, NodePtr, Allocate, Deallocate;
    EXPORT FreeNode, GetNode;

    VAR   FreeList : NodePtr; (*-- Free list of available stack nodes *)

    (*----------------------------*)

(*
FreeNode adds the given node to the front of the free list.  theNode's  next field is linked
to the front of the free list and the node becomes the new front of the free list.
*)

    PROCEDURE FreeNode (VAR theNode: NodePtr);
    BEGIN
      theNode^.next := FreeList;
      FreeList      := theNode;
    END FreeNode;
    (*----------------------------*)


(*
GetNode attempts to generate a new stack node.  If the free list is empty, attempt to
allocate a brand new node.  If the allocation attempt fails, return False and exit, otherwise
return the brand new node, a function result of True and exit.  If the free list is not empty,
remove the first available node from the free list and return the used node to the caller
along with a function result of True.
*)

    PROCEDURE GetNode (VAR theNode: NodePtr (*-- out *)): BOOLEAN;
    BEGIN
      IF (FreeList = NIL) THEN
        Allocate(theNode, SIZE(Node));
        IF (theNode = NIL) THEN
          RETURN FALSE;
        END (*--if*);
      ELSE
        theNode  := FreeList;
        FreeList := FreeList^.next;
      END (*--if*);
      RETURN TRUE;
    END GetNode;
    (*----------------------------*)

    (* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)
    (* ~~~~~  Local Module Initialization  ~~~~~ *)

    BEGIN
      FreeList := NIL; (*-- Initialize the free list to empty. *)
    END FreeListMgr;


(* ————————————————————————————————————————————————————————————————
    †  Local  †  Local  †  Local  †  Local  †  Local  †  Local  †
   ———————————————————————————————————————————————————————————————— *)


(*
7.4.4	Constructors

Create begins by clearing the stackError  field under the assumption of a successful result.

The stack header is then allocated in a local variable since the function result cannot be
manipulated but only returned.  If the unbounded stack could not be allocated, the
overflow exception must be raised, and the NullStack returned.

At this point, all possibility of failure has been avoided and the unbounded stack header
can be initialized to its empty state (top is set to NIL, depth set to zero), and the data type
ID  can be stored for this unbounded stack.  Lastly, the new stack is returned to the caller.
Complexity: O(1).
*)

PROCEDURE Create    (    theType   : TypeID    (*-- in    *))
                                   : Stack     (*-- out   *);

VAR   newStack : Stack;

BEGIN
  stackError := noerr;
  Allocate(newStack, SIZE(UnboundedStack));
  IF (newStack # NIL) THEN
    WITH newStack^ DO
      dataID := theType;
      depth  := 0;
      top    := NIL;
    END (*--with*);
    RETURN newStack;
  END (*--if*);
  RaiseErrIn(create, overflow);
  RETURN NullStack;
END Create;
(*----------------------------*)


(*
Destroy  takes advantage that Clear  sets stackError  to noerr and raises the undefined
stack exception.  So if Clear succeeds, Destroy simply releases the allocated stack header.
Complexity: O(n).
*)

PROCEDURE Destroy  (VAR theStack  : Stack     (*-- inout *));
BEGIN
  Clear(theStack);
  IF (stackError = noerr) THEN
    Deallocate(theStack, SIZE(theStack^));
  END (*--if*);
END Destroy;
(*----------------------------*)


(*
After initializing stackError and verifying that the given stack is a valid object, Clear
retrieves the item disposal routine associated with the stack's data type from the
TypeManager module.  Then each item on the stack is deallocated followed by its stack
node.  To do this safely a local variable, nodeToFree, is used to hold each node.  The loop
invariant takes advantage of the fact that the bottommost stack node contains a next field
of NIL, and this will automatically cause top to contain that value once all stack nodes
have been cleared.  The last step is to reset depth to zero.
Complexity O(n).
*)

PROCEDURE Clear    (VAR theStack  : Stack     (*-- inout *));

VAR   nodeToFree : NodePtr;
      freeItem   : DisposeProc; (*-- item disposal routine *)

BEGIN
  stackError := noerr;
  IF (theStack # NIL) THEN
    WITH theStack^ DO
      freeItem := DisposeOf(dataID);
      WHILE (top # NIL) DO
        nodeToFree := top;
        top := top^.next;
        freeItem(nodeToFree^.item);
        FreeNode(nodeToFree);
      END (*--while*);
      depth := 0;
    END (*--with*);
  ELSE
    RaiseErrIn(clear, undefined);
  END (*--if*);
END Clear;
(*----------------------------*)


(*
Assignment for unbounded objects has the complication that overflow may occur while
adding individual stack nodes.  When the target object is undefined, it must be created
using the data type attribute of the source object.  If overflow does not occur, the actual
assignment can commence, otherwise its suffices to exit (Create has already raised the
exception).  When the target object is defined on entry to the Assign operation, it is
sufficient to Clear the existing contents and update the data type with that of the source
stack.  If the source stack is empty, the routine may simply exit.  Otherwise, the top
node is copied from the source to the target, followed by a loop over the remaining items,
if any.

In order to properly copy the source stack, two node pointers are necessary: one, to loop
through each of the source nodes, and the second to keep track of the last node added to the
target stack.

The assignment operator cannot be used to copy individual items as the data type of those
items is unknown; using assignment for dynamically allocated items would cause
structural sharing of items, which is not desired.
Complexity O(mn).
*)

PROCEDURE Assign   (    theStack  : Stack     (*-- in    *);
                    VAR toStack   : Stack     (*-- inout *));

VAR   fromIndex : NodePtr;    (*-- node to add from source stack *)
      toIndex   : NodePtr;    (*-- last node added to target stack *)
      assignItem: AssignProc; (*-- item assignment routine *)

BEGIN
  IF (theStack # NIL) THEN
    IF (toStack # NIL) THEN
      Clear(toStack);
      toStack^.dataID := theStack^.dataID;
    ELSE
      toStack := Create(theStack^.dataID);
    END (*--if*);

    IF (stackError # noerr) OR (theStack^.top = NIL) THEN
      RETURN;
    END (*--if*);

    assignItem := AssignOf(theStack^.dataID);
    IF GetNode(toStack^.top) THEN
      WITH toStack^.top^ DO
        item := assignItem(theStack^.top^.item);
        next := NIL;
      END (*--with*);
      fromIndex := theStack^.top;
      toIndex   := toStack^.top;
      INC(toStack^.depth);
    ELSE
      RaiseErrIn(assign, overflow);
    END (*--if*);

    WHILE (stackError = noerr) & (fromIndex^.next # NIL) DO
      fromIndex := fromIndex^.next;
      IF GetNode(toIndex^.next) THEN
        toIndex := toIndex^.next;
        WITH toIndex^ DO
          item := assignItem(fromIndex^.item);
          next := NIL;
        END (*--with*);
        INC(toStack^.depth);
      ELSE
        RaiseErrIn(assign, overflow);
      END (*--if*);
    END (*--while*);
  ELSE
    RaiseErrIn(assign, undefined);
  END (*--if*);
END Assign;
(*----------------------------*)


(*
Push  attempts retrieval of a stack node for the new item, which if successful allows us to
store the data for new stack top and a link to the current stack top. Then the stack top can
be updated to point to the new node and the depth can be updated. If a stack node could not
be allocated the overflow exception is raised and the stack is left unchanged.
Complexity O(1)
*)

PROCEDURE Push     (VAR toStack   : Stack     (*-- inout *);
                        theItem   : Item      (*-- in    *));

VAR   newNode: NodePtr;

BEGIN
  stackError := noerr;
  IF (toStack # NIL) THEN
    IF GetNode(newNode) THEN
      WITH toStack^ DO
        newNode^.item := theItem;
        newNode^.next := top;
        top := newNode;
        INC(depth);
      END (*--with*);
    ELSE
      RaiseErrIn(push, overflow);
    END (*--if*);
  ELSE
    RaiseErrIn(push, undefined);
  END (*--if*);
END Push;
(*----------------------------*)


(*
Pop  is the inverse of Push, requiring a test for stack underflow, before effectively
undoing what was done in order to place a node on the stack.

PopTopOf  is similar to Pop except the item is not deallocated, but instead is returned to
the caller.
*)

PROCEDURE Pop       (VAR theStack  : Stack     (*-- inout *));

VAR   nodeToPop: NodePtr;
      freeItem : DisposeProc; (*-- item disposal routine *)

BEGIN
  stackError := noerr;
  IF (theStack # NIL) THEN
    WITH theStack^ DO
      IF (top # NIL) THEN       (*-- Check for stack underflow *)
        nodeToPop := top;       (*-- Remember current top of stack *)
        top       := top^.next; (*-- Update the top of stack *)
        DEC(depth);             (*-- Maintain a correct depth count *)
        freeItem := DisposeOf(dataID);
        freeItem(nodeToPop^.item); (*-- Safely recover the item space *)
        FreeNode(nodeToPop);    (*-- Recover the node space *)
      ELSE
        RaiseErrIn(pop, underflow);
      END (*--if*);
    END (*--with*);
  ELSE
    RaiseErrIn(pop, undefined);
  END (*--if*);
END Pop;
(*----------------------------*)

PROCEDURE PopTopOf (VAR theStack  : Stack     (*-- inout *))
                                  : Item      (*-- out   *);

VAR   oldTop    : NodePtr;
      topItem   : Item;

BEGIN
  stackError := noerr;
  IF (theStack # NIL) THEN
    WITH theStack^ DO
      IF (top # NIL) THEN    (*-- Check for stack underflow *)
        oldTop := top;       (*-- Remember current top of stack *)
        topItem:= top^.item; (*-- Remember current item at stack top *)
        top    := top^.next; (*-- Update the stack top *)
        DEC(depth);          (*-- Maintain a correct depth count *)
        FreeNode(oldTop);    (*-- Recover the node space *)
        RETURN topItem;      (*-- Return the data item *)
      END (*--if*);
    END (*--with*);

    RaiseErrIn(poptopof, underflow);
  ELSE
    RaiseErrIn(poptopof, undefined);
  END (*--if*);
  RETURN NullItem;
END PopTopOf;
(*----------------------------*)


(*
7.4.5	Selectors

IsDefined simply tests for the stack top being not NIL, returning true or false as
appropriate.  This is a simple test for a defined stack object.
*)

PROCEDURE IsDefined (    theStack  : Stack    (*-- in    *))
                                   : BOOLEAN  (*-- out   *);
BEGIN
   RETURN (theStack # NIL);
END IsDefined;
(*----------------------------*)


(*
IsEmpty simply tests the link to the stack top being NIL, returning the appropriate
logical value.  As mentioned in the interface, an undefined stack is considered empty.
*)

PROCEDURE IsEmpty  (    theStack   : Stack    (*-- in    *))
                                   : BOOLEAN  (*-- out   *);
BEGIN
  stackError := noerr;
  IF (theStack # NIL) THEN
    RETURN theStack^.top = NIL;
  END (*--if*);

  RaiseErrIn(isempty, undefined);
  RETURN TRUE;
END IsEmpty;
(*----------------------------*)


(*
IsEqual scans both stacks looking for the first inequality between items which indicates
the stacks are unequal, otherwise if the loop completes the stacks must be equal.  This
assumes that both stacks (1) have been defined, (2) have been given the same data type
ID, and (3) have the same depth.  The first two of these assumptions, if unfounded, cause
the exceptions, undefined and typeerror, respectively, to be raised.  Invariant: Since the
stacks have the same depth, when leftIndex is NIL, by definition rightIndex will also be
NIL.
*)

PROCEDURE IsEqual  (    left      : Stack     (*-- in    *);
                        right     : Stack     (*-- in    *))
                                  : BOOLEAN   (*-- out   *);

VAR   leftIndex : NodePtr;
      rightIndex: NodePtr;
      compare   : CompareProc;	(*-- item comparison routine *)

BEGIN
  stackError := noerr;
  IF (left # NIL) & (right # NIL) THEN
    IF (left^.dataID = right^.dataID) THEN
      IF (left^.depth = right^.depth) THEN
	compare   := CompareOf(left^.dataID);
        leftIndex := left^.top;
        rightIndex:= right^.top;
        WHILE (leftIndex # NIL) &
              (compare(leftIndex^.item, rightIndex^.item) = equal) DO
          leftIndex := leftIndex^.next;
          rightIndex:= rightIndex^.next;
        END (*--while*);
        RETURN (leftIndex = NIL);
      END (*--if*);
    ELSE
      RaiseErrIn(isequal, typeerror);
    END (*--if*);
  ELSE
    RaiseErrIn(isequal, undefined);
  END (*--if*);
  RETURN FALSE;
END IsEqual;
(*----------------------------*)


PROCEDURE TypeOf    (    theStack  : Stack     (*-- in    *))
                                   : TypeID    (*-- out   *);
BEGIN
  stackError := noerr;
  IF (theStack # NIL) THEN
    RETURN theStack^.dataID;
  END (*--if*);
  RaiseErrIn(typeof, undefined);
  RETURN NullType;
END TypeOf;
(*----------------------------*)

PROCEDURE DepthOf  (    theStack   : Stack         (*-- in    *))
                                   : CARDINAL      (*-- out   *);
BEGIN
  stackError := noerr;
  IF (theStack # NIL) THEN
    RETURN theStack^.depth;
  END (*--if*);
  RaiseErrIn(depthof, undefined);
  RETURN 0;
END DepthOf;
(*----------------------------*)

PROCEDURE TopOf    (    theStack   : Stack         (*-- in    *))
                                   : Item          (*-- out   *);
BEGIN
  stackError := noerr;
  IF (theStack # NIL) THEN
    WITH theStack^ DO
      IF (top # NIL) THEN
        RETURN top^.item;
      END (*--if*);
    END (*--with*);
    RaiseErrIn(topof, underflow);
  ELSE
    RaiseErrIn(topof, undefined);
  END (*--if*);

  RETURN NullItem;
END TopOf;
(*----------------------------*)


(*
7.4.6	Module Initialization

In the module initialization the local exception handlers array variables are set to default
handlers (ExitOnError) except for the noerr handler which is given the null handler.
stackError is given the value noerr  avoiding an undefined state.
*)

BEGIN
  FOR stackError := initfailed TO MAX(Exceptions) DO
    SetHandler(stackError, ExitOnError);
  END (*--for*);
  SetHandler(noerr, NullHandler);
  stackError := noerr;
END StkSUMN.


(*
References

[1]	A. Aho, J. Hopcroft, and J. Ullman, Data Structures and Algorithms, Addison-Wesley,
	Reading, MA 1983, pp. 37-53.
[2]	G. Booch, Software Components With Ada Structures, Tools, and Subsystems,
	Benjamin/Cummings, Menlo Park, CA 1987.
[3]	D. Knuth, The Art of Computer Programming, Vol. 1, Fundamental Algorithms,
	Addison-Wesley, Reading, MA 1973.
[4]	R. Wiener and R. Sincovec, Data Structures Using Modula-2, John Wiley & Sons, New
	York, NY 1986.
[5]	N. Wirth, Algorithms and Data Structures, Prentice-Hall, Englewood Cliffs, NJ 1986.
[6]	N. Wirth, Algorithms + Data Structures = Programs, Prentice-Hall, Englewood Cliffs,
	NJ 1976.
[7]	N. Wirth, Programming in Modula-2, Springer-Verlag, New York, NY 1983.
*)
