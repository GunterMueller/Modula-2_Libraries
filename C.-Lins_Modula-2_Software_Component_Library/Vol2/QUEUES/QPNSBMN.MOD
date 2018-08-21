IMPLEMENTATION MODULE QPNSBMN;
(*==============================================================
    Version  : 1.00  18 May 1989  C. Lins
    Compiler : TopSpeed Modula-2
    Code Size: R-  bytes
    Component: Monolithic Structures - Queue (Opaque version)
      Priority Non-Balking Sequential Bounded Managed Non-Iterator

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
    (*--Type*) Item, AssignProc, DisposeProc, CompareProc;

FROM QEnum IMPORT
    (*--Type*) Operations, Exceptions, ComponentID;

FROM Relations IMPORT
    (*--Type*) Relation;

FROM TypeManager IMPORT
    (*--Cons*) NullType,
    (*--Type*) TypeID,
    (*--Proc*) AssignOf, DisposeOf, CompareOf;

    (*--------------------*)


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

PROCEDURE Arrive  (VAR theQueue : Queue     (*-- inout *);
                       theItem  : Item      (*-- in    *));

VAR   index : CARDINAL; (*-- loop index over items *)
      itemPriority : Priority;

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
      compareItems : CompareProc;   (*-- item comparison routine *)

BEGIN
  queueError := noerr;
  IF (left = NIL) OR (right = NIL) THEN
    RaiseErrIn(isequal, undefined);
  ELSIF (left^.dataID # right^.dataID) THEN
    RaiseErrIn(isequal, typeerror);
  ELSIF (left^.rear = right^.rear) THEN
    WITH left^ DO
      compareItems := CompareOf(dataID);
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
  ELSIF (theQueue^.rear = 0) THEN
    RaiseErrIn(frontof, underflow);
  ELSE
    RETURN theQueue^.items[MIN(QueueSize)];
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
END QPNSBMN.
