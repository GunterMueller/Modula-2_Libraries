IMPLEMENTATION MODULE QSCUI;
(*==============================================================
    Version  : 1.00  19 May 1989  C. Lins
    Compiler : TopSpeed Modula-2 Compiler
    Component: Monolithic Structures - Queue (Opaque version)
      Non-priority Non-balking Sequential Circular Unmanaged Iterator

    REVISION HISTORY
    v1.00  19 May 1989  C. Lins:
      Implemented based on QueueSCMI module.

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

FROM TypeManager IMPORT
    (*--Cons*) NullType,
    (*--Type*) TypeID;

    (*--------------------*)


TYPE  ItemsArray    = ARRAY QueueSize OF Item;
TYPE  CircularQueue = RECORD
        dataID : TypeID;     (*-- defined data type *)
        size   : QueueSize;  (*-- maximum # of items *)
        length : CARDINAL;   (*-- current # of items *)
        front  : CARDINAL;   (*-- current head of the queue *)
        items  : ItemsArray; (*-- array [1..size] of item *)
      END (*-- CircularQueue *);

TYPE  Queue = POINTER TO CircularQueue;

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


PROCEDURE Create  (    theType  : TypeID    (*-- in    *);
                       theSize  : QueueSize (*-- in    *))
                                : Queue     (*-- out   *);

CONST staticSize = SIZE(CircularQueue) - SIZE(ItemsArray);
CONST itemSize   = SIZE(Item);

VAR   newQueue : Queue;

BEGIN
  queueError := noerr;
  Allocate(newQueue, staticSize + itemSize * theSize);
  IF (newQueue = NIL) THEN
    RaiseErrIn(create, overflow);
  ELSE
    WITH newQueue^ DO
      dataID := theType;
      size   := theSize;
      length := 0;
      front  := MIN(QueueSize);
    END(*--with*);
  END(*--if*);
  RETURN newQueue;
END Create;
(*-------------------------*)

PROCEDURE Destroy (VAR theQueue : Queue     (*-- inout *));

CONST staticSize = SIZE(CircularQueue) - SIZE(ItemsArray);
CONST itemSize   = SIZE(Item);

BEGIN
  Clear(theQueue);
  IF (queueError = noerr) THEN
    Deallocate(theQueue, staticSize + itemSize * theQueue^.size);
  END (*--if*);
END Destroy;
(*-------------------------*)

PROCEDURE Clear   (    theQueue : Queue     (*-- inout *));
BEGIN
  queueError := noerr;
  IF (theQueue # NIL) THEN
    WITH theQueue^ DO
      length := 0;
      front  := MIN(QueueSize);
    END (*--with*);
  ELSE
    RaiseErrIn(clear, undefined);
  END (*--if*);
END Clear;
(*-------------------------*)

PROCEDURE Assign  (    theQueue : Queue     (*-- in    *);
                   VAR toQueue  : Queue     (*-- inout *));

VAR   index      : CARDINAL;   (*-- loop index over items *)
      fromIndex  : CARDINAL;   (*-- index into source queue *)
      toIndex    : CARDINAL;   (*-- index into target queue *)

BEGIN
  queueError := noerr;
  IF (theQueue = NIL) THEN
    RaiseErrIn(assign, undefined);
  ELSIF (theQueue # toQueue) THEN
    IF (toQueue = NIL) THEN
      WITH theQueue^ DO
        toQueue := Create(dataID, size);
      END (*--with*);
    ELSIF (theQueue^.length <= theQueue^.size) THEN
      Clear(toQueue);
      toQueue^.dataID := theQueue^.dataID;
    ELSE
      RaiseErrIn(assign, overflow);
    END (*--if*);
    IF (queueError = noerr) THEN
      WITH theQueue^ DO
        fromIndex  := front;
        toIndex    := toQueue^.front;
        FOR index := MIN(QueueSize) TO length DO
          toQueue^.items[toIndex] := items[fromIndex];
          fromIndex := (fromIndex+1) MOD size;
          toIndex   := (toIndex+1) MOD toQueue^.size;
        END (*--for*);
        toQueue^.length := length;
      END (*--with*);
    END (*--if*);
  END (*--if*);
END Assign;
(*-------------------------*)

PROCEDURE Arrive  (    theQueue : Queue     (*-- inout *);
                       theItem  : Item      (*-- in    *));
BEGIN
  queueError := noerr;
  IF (theQueue = NIL) THEN
    RaiseErrIn(arrive, undefined);
  ELSE
    WITH theQueue^ DO
      IF (length < size) THEN
        items[(front+length) MOD size] := theItem;
        INC(length);
      ELSE
        RaiseErrIn(arrive, overflow);
      END (*--if*);
    END (*--with*);
  END (*--if*);
END Arrive;
(*-------------------------*)

PROCEDURE Depart  (    theQueue : Queue     (*-- inout *));

VAR   index : CARDINAL;    (*-- loop index over items *)

BEGIN
  queueError := noerr;
  IF (theQueue = NIL) THEN
    RaiseErrIn(depart, undefined);
  ELSE
    WITH theQueue^ DO
      IF (length = 0) THEN
        RaiseErrIn(depart, underflow);
      ELSE
        front := (front+1) MOD size;
        DEC(length);
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
    RETURN (theQueue^.length = 0);
  END (*--if*);
  RaiseErrIn(isempty, undefined);
  RETURN TRUE;
END IsEmpty;
(*-------------------------*)

PROCEDURE IsEqual   (    left     : Queue    (*-- in    *);
                         right    : Queue    (*-- in    *))
                                  : BOOLEAN  (*-- out   *);

VAR   leftIndex : CARDINAL; (*-- loop index left queue items *)
      rightIndex: CARDINAL; (*-- loop index right queue items *)
      leftBack  : CARDINAL; (*-- where left queue ends *)
      rightBack : CARDINAL; (*-- where right queue ends *)

BEGIN
  queueError := noerr;
  IF (left = NIL) OR (right = NIL) THEN
    RaiseErrIn(isequal, undefined);
  ELSIF (left^.dataID # right^.dataID) THEN
    RaiseErrIn(isequal, typeerror);
  ELSIF (left^.length = right^.length) THEN

    WITH left^ DO
      leftIndex  := front;
      leftBack   := (front + length) MOD size;
    END (*--with*);

    WITH right^ DO
      rightIndex := front;
      rightBack  := (front + length) MOD size;
    END (*--with*);

    WITH left^ DO
      WHILE (leftIndex # leftBack) & (rightIndex # rightBack) DO
        IF (items[leftIndex] # right^.items[rightIndex]) THEN
          RETURN FALSE;
        END (*--if*);
        leftIndex  := (leftIndex + 1) MOD size;
        rightIndex := (rightIndex + 1) MOD right^.size;
      END (*--while*);
      RETURN (leftIndex = leftBack) & (rightIndex = rightBack);
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
    RETURN theQueue^.length;
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

PROCEDURE FrontOf   (    theQueue : Queue    (*-- in    *))
                                  : Item     (*-- out   *);
BEGIN
  queueError := noerr;
  IF (theQueue = NIL) THEN
    RaiseErrIn(frontof, undefined);
  ELSIF (theQueue^.length = 0) THEN
    RaiseErrIn(frontof, underflow);
  ELSE
    RETURN theQueue^.items[theQueue^.front];
  END (*--if*);
  RETURN NullItem;
END FrontOf;
(*-------------------------*)


PROCEDURE LoopOver (    theQueue  : Queue          (*-- in    *);
                        theProcess: LoopAccessProc (*-- in    *));

VAR   index : CARDINAL; (*-- loop index over items *)
      back  : CARDINAL; (*-- where the queue ends *)

BEGIN
  queueError := noerr;
  IF (theQueue = NIL) THEN
    RaiseErrIn(loopover, undefined);
  ELSE
    WITH theQueue^ DO
      index:= front;
      back := (front+length) MOD size;
      WHILE (index # back) DO
        IF ~theProcess(items[index]) THEN
          RETURN;
        END (*--if*);
        index := (index+1) MOD size;
      END (*--while*);
    END (*--with*);
  END (*--if*);
END LoopOver;
(*-------------------------*)

PROCEDURE Traverse (    theQueue  : Queue          (*-- in    *);
                        theProcess: AccessProc     (*-- in    *));

VAR   index : CARDINAL; (*-- loop index over items *)
      back  : CARDINAL; (*-- where the queue ends *)

BEGIN
  queueError := noerr;
  IF (theQueue = NIL) THEN
    RaiseErrIn(traverse, undefined);
  ELSE
    WITH theQueue^ DO
      index:= front;
      back := (front+length) MOD size;
      WHILE (index # back) DO
        theProcess(items[index]);
        index := (index+1) MOD size;
      END (*--while*);
    END (*--with*);
  END (*--if*);
END Traverse;
(*-------------------------*)


BEGIN
  FOR queueError := MIN(Exceptions) TO MAX(Exceptions) DO
    SetHandler(queueError, ExitOnError);
  END (*--for*);
  SetHandler(noerr, NullHandler);
  queueError := noerr;
END QSCUI.
