IMPLEMENTATION MODULE QSUUN;
(*==============================================================
    Version  : 1.00  03 May 1989  C. Lins
    Compiler : TopSpeed Modula-2
    Code Size: R-  bytes
    Component: Monolithic Structures - Queue (Opaque version)
      Non-Priority Non-Balking Sequential Unbounded Unmanaged Non-Iterator

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
    (*--Type*) Item;

FROM QEnum IMPORT
    (*--Type*) Operations, Exceptions, ComponentID;

    (*--------------------*)

TYPE  Link = POINTER TO Node;
TYPE  Node = RECORD
        item : Item; (*-- item data *)
        next : Link; (*-- link to next node *)
      END (*-- Node *);

TYPE  UnboundedQueue = RECORD
        length : CARDINAL;   (*-- current # of items *)
        head   : Link;       (*-- pointer to first item *)
        tail   : Link;       (*-- pointer to last item *)
      END (*-- UnboundedQueue *);

TYPE  Queue = POINTER TO UnboundedQueue;

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

PROCEDURE Create  ()            : Queue     (*-- out   *);

VAR   newQueue : Queue;

BEGIN
  queueError := noerr;
  Allocate(newQueue, SIZE(UnboundedQueue));
  IF (newQueue = NIL) THEN
    RaiseErrIn(create, overflow);
  ELSE
    WITH newQueue^ DO
      length := 0;
      head   := NIL;
      tail   := NIL;
    END(*--with*);
  END(*--if*);
  RETURN newQueue;
END Create;
(*-------------------------*)

PROCEDURE Destroy (VAR theQueue : Queue     (*-- inout *));
BEGIN
  Clear(theQueue);
  IF (queueError = noerr) THEN
    Deallocate(theQueue, SIZE(theQueue^));
  END (*--if*);
END Destroy;
(*-------------------------*)

PROCEDURE Clear   (VAR theQueue : Queue     (*-- inout *));

VAR   oldHead : Link;        (*-- item node to deallocate *)

BEGIN
  queueError := noerr;
  IF (theQueue # NIL) THEN
    WITH theQueue^ DO
      WHILE (head # NIL) DO
        oldHead := head;
        head    := head^.next;
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

PROCEDURE Assign  (    theQueue : Queue     (*-- in    *);
                   VAR toQueue  : Queue     (*-- inout *));

VAR   fromIndex  : Link;       (*-- loop index over source nodes *)

BEGIN
  queueError := noerr;
  IF (theQueue = NIL) THEN
    RaiseErrIn(assign, undefined);
  ELSIF (theQueue # toQueue) THEN
    IF (toQueue = NIL) THEN
      toQueue := Create();
    ELSE
      Clear(toQueue);
    END (*--if*);
    IF (queueError = noerr) & (theQueue^.head # NIL) THEN
      Allocate(toQueue^.head, SIZE(Node));
      IF (toQueue^.head = NIL) THEN
        RaiseErrIn(assign, overflow);
      ELSE
        WITH toQueue^ DO
          WITH head^ DO
            item := theQueue^.head^.item;
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
              next^.item := fromIndex^.item;
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

PROCEDURE Depart  (VAR theQueue : Queue     (*-- inout *));

VAR   oldHead : Link;        (*-- original head, departing *)

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
        Deallocate(oldHead, SIZE(oldHead^));
      END (*--if*);
    END (*--with*);
  END (*--if*);
END Depart;
(*-------------------------*)

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
    RETURN (theQueue^.head = NIL);
  END (*--if*);
  RaiseErrIn(isempty, undefined);
  RETURN TRUE;
END IsEmpty;
(*-------------------------*)

PROCEDURE IsEqual   (    left     : Queue    (*-- in    *);
                         right    : Queue    (*-- in    *))
                                  : BOOLEAN  (*-- out   *);

VAR   leftIndex : Link; (*-- loop index over left items *)
      rightIndex: Link; (*-- loop index over right items *)

BEGIN
  queueError := noerr;
  IF (left = NIL) OR (right = NIL) THEN
    RaiseErrIn(isequal, undefined);
  ELSIF (left^.length = right^.length) THEN
    leftIndex := left^.head;
    rightIndex:= right^.head;
    WHILE (leftIndex # NIL) DO
      IF (leftIndex^.item # rightIndex^.item) THEN
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

BEGIN
  FOR queueError := MIN(Exceptions) TO MAX(Exceptions) DO
    SetHandler(queueError, ExitOnError);
  END (*--for*);
  SetHandler(noerr, NullHandler);
  queueError := noerr;
END QSUUN.
