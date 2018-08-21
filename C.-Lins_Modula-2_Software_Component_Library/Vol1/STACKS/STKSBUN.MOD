IMPLEMENTATION MODULE StkSBUN;
(*==========================================================
    Version  : 1.00  29 Apr 1989  C. Lins
    Compiler : TopSpeed Modula-2
    Code Size: R-  bytes
    Component: Monolithic Structures - Stack (Opaque)
               Sequential Bounded Managed Non-Iterator

    REVISION HISTORY
    v1.00  29 Apr 1989  C. Lins
        Initial implementation from StackSBUI module.
==========================================================*)

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

    (*-----------------------*)


TYPE  ItemsArray   = ARRAY SizeRange OF Item;

TYPE  BoundedStack = RECORD
        size  : SizeRange;  (*-- Maximum # of items on this stack *)
        top   : CARDINAL;   (*-- Current stack top := 0 *)
        items : ItemsArray; (*-- Dynamic array [1..size] of item  *)
      END (*-- BoundedStack *);

TYPE  Stack = POINTER TO BoundedStack;

    (*-----------------------*)

VAR   stackError : Exceptions;
VAR   handlers   : ARRAY Exceptions OF HandlerProc;

    (*-----------------------*)

PROCEDURE StackError ()              : Exceptions    (*-- out   *);
BEGIN
  RETURN stackError;
END StackError;
(*----------------------------*)

PROCEDURE GetHandler (    theError   : Exceptions    (*-- in    *))
                                     : HandlerProc   (*-- out   *);
BEGIN
  RETURN handlers[theError];
END GetHandler;
(*----------------------------*)

PROCEDURE SetHandler (    theError   : Exceptions    (*-- in    *);
                          theHandler : HandlerProc   (*-- in    *));
BEGIN
  handlers[theError] := theHandler;
END SetHandler;
(*----------------------------*)

PROCEDURE RaiseErrIn (    theRoutine : Operations    (*-- in    *);
                          theError   : Exceptions    (*-- in    *));
BEGIN
  stackError := theError;
  Raise(ComponentID + ModuleID, theRoutine, theError, handlers[theError]);
END RaiseErrIn;
(*----------------------------*)

    (*-----------------------*)

PROCEDURE Create    (    theSize   : SizeRange (*-- in    *))
                                   : Stack     (*-- out   *);

CONST minStackSize = SIZE(BoundedStack) - SIZE(ItemsArray);

VAR   newStack : Stack;

BEGIN
  stackError := noerr;
  Allocate(newStack, minStackSize + SIZE(Item) * theSize);
  IF (newStack # NIL) THEN
    WITH newStack^ DO
      size   := theSize;
      top    := 0;
    END (*--with*);
    RETURN newStack;
  END (*--if*);
  RaiseErrIn(create, overflow);
  RETURN NullStack;
END Create;
(*----------------------------*)

PROCEDURE Destroy   (VAR theStack  : Stack     (*-- inout *));

CONST minStackSize = SIZE(BoundedStack) - SIZE(ItemsArray);

BEGIN
  Clear(theStack);
  IF (stackError = noerr) THEN
    Deallocate(theStack, minStackSize + SIZE(Item) * theStack^.size);
  END (*--if*);
END Destroy;
(*----------------------------*)

PROCEDURE Clear     (VAR theStack  : Stack     (*-- inout *));

VAR   itemIndex : CARDINAL;    (*-- loop index over items *)

BEGIN
  stackError := noerr;
  IF (theStack # NIL) THEN
    WITH theStack^ DO
      top := 0;
    END (*--with*);
  ELSE
    RaiseErrIn(clear, undefined);
  END (*--if*);
END Clear;
(*----------------------------*)

PROCEDURE Assign    (    theStack  : Stack     (*-- in    *);
                     VAR toStack   : Stack     (*-- inout *));

VAR   itemIndex : CARDINAL;   (*-- loop index over items *)

BEGIN
  stackError := noerr;
  IF (theStack # NIL) THEN

    IF (toStack # NIL) THEN
      IF (theStack^.top <= toStack^.size) THEN
        Clear(toStack);
      ELSE
        RaiseErrIn(assign, overflow);
      END (*--if*);
    ELSE
      WITH theStack^ DO
        toStack := Create(size);
      END (*--with*);
    END (*--if*);
    IF (stackError # noerr) THEN
      RETURN;
    END (*--if*);

    WITH theStack^ DO
      FOR itemIndex := MIN(SizeRange) TO top DO
        toStack^.items[itemIndex] := items[itemIndex];
      END (*--for*);
      toStack^.top := top;
    END (*--with*);
  ELSE
    RaiseErrIn(assign, undefined);
  END (*--if*);
END Assign;
(*----------------------------*)


PROCEDURE Push      (VAR toStack   : Stack     (*-- inout *);
                         theItem   : Item      (*-- in    *));
BEGIN
  stackError := noerr;
  IF (toStack # NIL) THEN
    WITH toStack^ DO
      IF (top < size) THEN
        INC(top);
        items[top] := theItem;
      ELSE
        RaiseErrIn(push, overflow);
      END (*--if*);
    END (*--with*);
  ELSE
    RaiseErrIn(push, undefined);
  END (*--if*);
END Push;
(*----------------------------*)

PROCEDURE Pop       (VAR theStack  : Stack     (*-- inout *));
BEGIN
  stackError := noerr;
  IF (theStack # NIL) THEN
    WITH theStack^ DO
      IF (top # 0) THEN
        DEC(top);
      ELSE
        RaiseErrIn(pop, underflow);
      END (*--if*);
    END (*--with*);
  ELSE
    RaiseErrIn(pop, undefined);
  END (*--if*);
END Pop;
(*----------------------------*)

PROCEDURE PopTopOf  (VAR theStack  : Stack     (*-- inout *))
                                   : Item      (*-- out   *);

VAR   theItem : Item; (*-- item to be returned *)

BEGIN
  stackError := noerr;
  IF (theStack # NIL) THEN
    WITH theStack^ DO
      IF (top # 0) THEN
        theItem := items[top];
        DEC(top);
        RETURN theItem;
      END (*--if*);
    END (*--with*);

    RaiseErrIn(poptopof, underflow);
  ELSE
    RaiseErrIn(poptopof, undefined);
  END (*--if*);

  (*-- Return the empty item if an exception occurred. *)

  RETURN NullItem;

END PopTopOf;
(*----------------------------*)


PROCEDURE IsDefined (    theStack  : Stack     (*-- in    *))
                                   : BOOLEAN   (*-- out   *);
BEGIN
  RETURN (theStack # NIL);
END IsDefined;
(*----------------------------*)

PROCEDURE IsEmpty   (    theStack  : Stack     (*-- in    *))
                                   : BOOLEAN   (*-- out   *);
BEGIN
  stackError := noerr;
  IF (theStack # NIL) THEN
    RETURN theStack^.top = 0;
  END (*--if*);
  RaiseErrIn(isempty, undefined);
  RETURN TRUE;
END IsEmpty;
(*----------------------------*)

PROCEDURE IsEqual   (    left      : Stack     (*-- in    *);
                         right     : Stack     (*-- in    *))
                                   : BOOLEAN   (*-- out   *);

VAR   index : CARDINAL; (*-- loop index over items *)

BEGIN
  stackError := noerr;
  IF (left # NIL) & (right # NIL) THEN
    IF (left^.top = right^.top) THEN

      FOR index := MIN(SizeRange) TO left^.top DO
        IF (left^.items[index] # right^.items[index]) THEN
          RETURN FALSE;
        END (*--if*);
      END (*--for*);
      RETURN TRUE;

    END (*--if*);
  ELSE
    RaiseErrIn(isequal, undefined);
  END (*--if*);
  RETURN FALSE;
END IsEqual;
(*----------------------------*)

PROCEDURE SizeOf   (    theStack   : Stack     (*-- in    *))
                                   : CARDINAL  (*-- out   *);
BEGIN
  stackError := noerr;
  IF (theStack # NIL) THEN
    RETURN theStack^.size;
  END (*--if*);
  RaiseErrIn(sizeof, undefined);
  RETURN 0;
END SizeOf;
(*----------------------------*)

PROCEDURE DepthOf   (    theStack  : Stack     (*-- in    *))
                                   : CARDINAL  (*-- out   *);
BEGIN
  stackError := noerr;
  IF (theStack # NIL) THEN
    RETURN theStack^.top;
  END (*--if*);
  RaiseErrIn(depthof, undefined);
  RETURN 0;
END DepthOf;
(*----------------------------*)

PROCEDURE TopOf     (    theStack  : Stack     (*-- in    *))
                                   : Item      (*-- out   *);
BEGIN
  stackError := noerr;
  IF (theStack # NIL) THEN
    WITH theStack^ DO
      IF (top # 0) THEN
        RETURN items[top];
      END (*--if*);
    END (*--with*);
    RaiseErrIn(topof, underflow);
  ELSE
    RaiseErrIn(topof, undefined);
  END (*--if*);

  (*-- Return the empty item if an exception occurred *)

  RETURN NullItem;
END TopOf;
(*----------------------------*)

BEGIN
  FOR stackError := initfailed TO MAX(Exceptions) DO
    SetHandler(stackError, ExitOnError);
  END (*--for*);
  SetHandler(noerr, NullHandler);
  stackError := noerr;
END StkSBUN.
