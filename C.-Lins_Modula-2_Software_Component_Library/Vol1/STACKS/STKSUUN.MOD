
(*
7.4	StackSUUN Implementation
*)


IMPLEMENTATION MODULE StkSUUN;
(*===========================================================
    Version  : 1.00  28 Apr 1989  C. Lins
    Compiler : TopSpeed Modula-2
    Code Size: R-  bytes
    Component: Monolithic Structures - Stack (Opaque)
               Sequential Unbounded Unmanaged Non-Iterator

    REVISION HISTORY
    v1.00  28 Apr 1989 C. Lins:
      Initial implementation
===========================================================*)

FROM JPIStorage IMPORT
    (*--Proc*) Allocate, Deallocate;

FROM Items IMPORT
    (*--Cons*) NullItem,
    (*--Type*) Item;

FROM ErrorHandling IMPORT
    (*--Type*) HandlerProc,
    (*--Proc*) NullHandler, ExitOnError, Raise;

FROM StackEnum IMPORT
    (*--Type*) Exceptions, Operations, ComponentID;

    (*--------------------*)

TYPE  NodePtr = POINTER TO Node;
TYPE  Node    = RECORD   (*-- a stack node *)
        item  : Item;    (*-- generic data item *)
        next  : NodePtr; (*-- next node beneath this one, if any *)
      END (*-- Node *);

TYPE  UnboundedStack = RECORD
        depth : CARDINAL; (*-- current stack depth, := 0   *)
        top   : NodePtr;  (*-- current stack top    := NIL *)
      END (*-- UnboundedStack *);

TYPE  Stack = POINTER TO UnboundedStack;

    (*-----------------------*)

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


(* ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
    ¥  Local  ¥  Local  ¥  Local  ¥  Local  ¥  Local  ¥  Local  ¥
   ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ *)

    MODULE FreeListMgr;

    (*-- This local module controls access to the Available node list.
      -- Version 1.00  07 Jun. 1987  C. Lins *)

    IMPORT Node, NodePtr, Allocate, Deallocate;
    EXPORT FreeNode, GetNode;

    VAR   FreeList : NodePtr; (*-- Free list of available stack nodes *)

    (*----------------------------*)

    PROCEDURE FreeNode (VAR theNode: NodePtr);
    BEGIN
      theNode^.next := FreeList;
      FreeList      := theNode;
    END FreeNode;
    (*----------------------------*)

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


(* ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
    ¥  Local  ¥  Local  ¥  Local  ¥  Local  ¥  Local  ¥  Local  ¥
   ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ *)


PROCEDURE Create    ()             : Stack     (*-- out   *);

VAR   newStack : Stack;

BEGIN
  stackError := noerr;
  Allocate(newStack, SIZE(UnboundedStack));
  IF (newStack # NIL) THEN
    WITH newStack^ DO
      depth  := 0;
      top    := NIL;
    END (*--with*);
    RETURN newStack;
  END (*--if*);
  RaiseErrIn(create, overflow);
  RETURN NullStack;
END Create;
(*----------------------------*)

PROCEDURE Destroy  (VAR theStack  : Stack     (*-- inout *));
BEGIN
  Clear(theStack);
  IF (stackError = noerr) THEN
    Deallocate(theStack, SIZE(theStack^));
  END (*--if*);
END Destroy;
(*----------------------------*)

PROCEDURE Clear    (VAR theStack  : Stack     (*-- inout *));

VAR   nodeToFree : NodePtr;

BEGIN
  stackError := noerr;
  IF (theStack # NIL) THEN
    WITH theStack^ DO
      WHILE (top # NIL) DO
        nodeToFree := top;
        top := top^.next;
        FreeNode(nodeToFree);
      END (*--while*);
      depth := 0;
    END (*--with*);
  ELSE
    RaiseErrIn(clear, undefined);
  END (*--if*);
END Clear;
(*----------------------------*)

PROCEDURE Assign   (    theStack  : Stack     (*-- in    *);
                    VAR toStack   : Stack     (*-- inout *));

VAR   fromIndex : NodePtr;    (*-- node to add from source stack *)
      toIndex   : NodePtr;    (*-- last node added to target stack *)

BEGIN
  IF (theStack # NIL) THEN
    IF (toStack # NIL) THEN
      Clear(toStack);
    ELSE
      toStack := Create();
    END (*--if*);

    IF (stackError # noerr) OR (theStack^.top = NIL) THEN
      RETURN;
    END (*--if*);

    IF GetNode(toStack^.top) THEN
      WITH toStack^.top^ DO
        item := theStack^.top^.item;
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
          item := fromIndex^.item;
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

PROCEDURE Pop       (VAR theStack  : Stack     (*-- inout *));

VAR   nodeToPop: NodePtr;

BEGIN
  stackError := noerr;
  IF (theStack # NIL) THEN
    WITH theStack^ DO
      IF (top # NIL) THEN       (*-- Check for stack underflow *)
        nodeToPop := top;       (*-- Remember current top of stack *)
        top       := top^.next; (*-- Update the top of stack *)
        DEC(depth);             (*-- Maintain a correct depth count *)
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

PROCEDURE IsDefined (    theStack  : Stack    (*-- in    *))
                                   : BOOLEAN  (*-- out   *);
BEGIN
   RETURN (theStack # NIL);
END IsDefined;
(*----------------------------*)

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

PROCEDURE IsEqual  (    left      : Stack     (*-- in    *);
                        right     : Stack     (*-- in    *))
                                  : BOOLEAN   (*-- out   *);

VAR   leftIndex : NodePtr;
      rightIndex: NodePtr;

BEGIN
  stackError := noerr;
  IF (left # NIL) & (right # NIL) THEN
    IF (left^.depth = right^.depth) THEN
	  leftIndex := left^.top;
	  rightIndex:= right^.top;
	  WHILE (leftIndex # NIL) &
		    (leftIndex^.item = rightIndex^.item) DO
	    leftIndex := leftIndex^.next;
	    rightIndex:= rightIndex^.next;
	  END (*--while*);
	  RETURN (leftIndex = NIL);
    END (*--if*);
  ELSE
    RaiseErrIn(isequal, undefined);
  END (*--if*);
  RETURN FALSE;
END IsEqual;
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

BEGIN
  FOR stackError := initfailed TO MAX(Exceptions) DO
    SetHandler(stackError, ExitOnError);
  END (*--for*);
  SetHandler(noerr, NullHandler);
  stackError := noerr;
END StkSUUN.
