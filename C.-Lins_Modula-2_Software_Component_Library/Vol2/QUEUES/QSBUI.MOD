IMPLEMENTATION MODULE QSBUI;
(*==============================================================
    Version  : 1.00  18 May 1989  C. Lins
    Compiler : TopSpeed Modula-2
    Component: Monolithic Structures - Queue (Opaque version)
      Non-priority Non-balking Sequential Bounded Unmanaged Iterator
    Code Size: R-  bytes

    This module is designed for use with the basic data types
    (e.g., INTEGERs, POINTERs to other structures) as data items.
    You must type cast the generic Items to/from your basic data type.
    Refer to the QueueSBMI module for all other commentary.

    REVISION HISTORY
    v1.00  18 May 1989  C. Lins:
      Initial implementation derived from QueueSBMI module.

    (C) Copyright 1989 Charles A. Lins
==============================================================*)

FROM JPIStorage IMPORT
    (*--Proc*) Allocate, Deallocate;

FROM ErrorHandling IMPORT
    (*--Type*) HandlerProc,
    (*--Proc*) Raise, NullHandler, ExitOnError;

FROM Items IMPORT
    (*--Cons*) NullItem,
    (*--Type*) Item, AccessProc, LoopAccessProc;

FROM QEnum IMPORT
    (*--Type*) Operations, Exceptions, ComponentID;

    (*--------------------*)

TYPE  ItemsArray   = ARRAY QueueSize OF Item;
TYPE  BoundedQueue = RECORD
        size   : QueueSize;  (*-- maximum # of items *)
        rear   : CARDINAL;   (*-- current # of items *)
        items  : ItemsArray; (*-- array [1..size] of item *)
      END (*-- BoundedQueue *);

TYPE  Queue = POINTER TO BoundedQueue;

    (*--------------------*)


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


(*-- Constructors --*)

PROCEDURE Create  (    theSize  : QueueSize (*-- in    *))
                                : Queue     (*-- out   *);

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
      size   := theSize;
      rear   := 0;
    END(*--with*);
  END(*--if*);
  RETURN newQueue;
END Create;
(*-------------------------*)

PROCEDURE Destroy (VAR theQueue : Queue     (*-- inout *));

CONST staticSize = SIZE(BoundedQueue) - SIZE(ItemsArray);
CONST itemSize   = SIZE(Item);

BEGIN
  queueError := noerr;
  IF (theQueue # NIL) THEN
    Deallocate(theQueue, staticSize + itemSize * theQueue^.size);
  ELSE
    RaiseErrIn(destroy, undefined);
  END (*--if*);
END Destroy;
(*-------------------------*)

PROCEDURE Clear   (VAR theQueue : Queue     (*-- inout *));
BEGIN
  queueError := noerr;
  IF (theQueue # NIL) THEN
    theQueue^.rear := 0;
  ELSE
    RaiseErrIn(clear, undefined);
  END (*--if*);
END Clear;
(*-------------------------*)

PROCEDURE Assign  (    theQueue : Queue     (*-- in    *);
                   VAR toQueue  : Queue     (*-- inout *));

VAR   index : CARDINAL;   (*-- loop index over items *)

BEGIN
  queueError := noerr;
  IF (theQueue = NIL) THEN
    RaiseErrIn(assign, undefined);
  ELSIF (theQueue # toQueue) THEN
    IF (toQueue = NIL) THEN
      WITH theQueue^ DO
        toQueue := Create(size);
      END (*--with*);
    ELSIF (theQueue^.rear <= toQueue^.size) THEN
      Clear(toQueue);
    ELSE
      RaiseErrIn(assign, overflow);
    END (*--if*);
    IF (queueError = noerr) THEN
      WITH theQueue^ DO
        FOR index := MIN(QueueSize) TO rear DO
          toQueue^.items[index] := items[index];
        END (*--for*);
        toQueue^.rear := rear;
      END (*--with*);
    END (*--if*);
  END (*--if*);
END Assign;
(*-------------------------*)

PROCEDURE Arrive  (VAR theQueue : Queue     (*-- inout *);
                       theItem  : Item      (*-- in    *));
BEGIN
  queueError := noerr;
  IF (theQueue = NIL) THEN
    RaiseErrIn(arrive, undefined);
  ELSE
    WITH theQueue^ DO
      IF (rear < size) THEN
        INC(rear);
        items[rear] := theItem;
      ELSE
        RaiseErrIn(arrive, overflow);
      END (*--if*);
    END (*--with*);
  END (*--if*);
END Arrive;
(*-------------------------*)

PROCEDURE Depart  (VAR theQueue : Queue     (*-- inout *));

VAR   index : CARDINAL;    (*-- loop index over items *)

BEGIN
  queueError := noerr;
  IF (theQueue = NIL) THEN
    RaiseErrIn(depart, undefined);
  ELSE
    WITH theQueue^ DO
      IF (rear = 0) THEN
        RaiseErrIn(depart, underflow);
      ELSE
        FOR index := MIN(QueueSize) + 1 TO rear DO
          items[index - 1] := items[index];
        END (*--for*);
        DEC(rear);
      END (*--if*);
    END (*--with*);
  END (*--if*);
END Depart;
(*-------------------------*)


(*-- Selectors --*)

PROCEDURE IsDefined (    theQueue : Queue    (*-- in    *))
                                  : BOOLEAN  (*-- out   *);
BEGIN
  RETURN theQueue # NIL;
END IsDefined;
(*-------------------------*)

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

PROCEDURE IsEqual   (    left     : Queue    (*-- in    *);
                         right    : Queue    (*-- in    *))
                                  : BOOLEAN  (*-- out   *);

VAR   index : CARDINAL; (*-- loop index over items *)

BEGIN
  queueError := noerr;
  IF (left = NIL) OR (right = NIL) THEN
    RaiseErrIn(isequal, undefined);
  ELSIF (left^.rear = right^.rear) THEN
    WITH left^ DO
      FOR index := MIN(QueueSize) TO rear DO
        IF (items[index] # right^.items[index]) THEN
          RETURN FALSE;
        END (*--if*);
      END (*--for*);
      RETURN TRUE;
    END (*--with*);
  END (*--if*);
  RETURN FALSE;
END IsEqual;
(*-------------------------*)

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


(*-- Iterators --*)

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


(*-- Module Initialization --*)

BEGIN
  FOR queueError := MIN(Exceptions) TO MAX(Exceptions) DO
    SetHandler(queueError, ExitOnError);
  END (*--for*);
  SetHandler(noerr, NullHandler);
  queueError := noerr;
END QSBUI.
