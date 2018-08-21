(*
6.3	StackSBMI Implementation
*)


IMPLEMENTATION MODULE StkSBMI;
(*==========================================================
    Version  : 1.00  29 Apr 1989  C. Lins
    Compiler : TopSpeed Modula-2 Compiler
    Code Size: R- 2356 bytes
    Component: Monolithic Structures - Stack (Opaque)
               Sequential Bounded Managed Iterator

    INTRODUCTION
    This module provides the implementation of the bounded stack
    composed of generic Items.

    REVISION HISTORY
    v1.00  29 Apr 1989  C. Lins
        Initial implementation for TML Modula-2.
==========================================================*)

FROM JPIStorage IMPORT
    (*--Proc*) Allocate, Deallocate;

FROM Items IMPORT
    (*--Cons*) NullItem, NoDisposeProc,
    (*--Type*) Item, AssignProc, DisposeProc, AccessProc,
               ChangeProc, LoopAccessProc, LoopChangeProc,
			   CompareProc;

FROM ErrorHandling IMPORT
    (*--Type*) HandlerProc,
    (*--Proc*) NullHandler, ExitOnError, Raise;

FROM Relations IMPORT
    (*--Type*) Relation;

FROM StackEnum IMPORT
    (*--Type*) Exceptions, Operations, ComponentID;

FROM TypeManager IMPORT
    (*--Cons*) NullType,
    (*--Type*) TypeID,
    (*--Proc*) AssignOf, DisposeOf, CompareOf;


(*
6.3.1	Internal Representation

Figure 6.1

For the internal representation of a bounded stack a record is dynamically allocated on the
heap.  This record will be made just large enough to hold the declared maximum size of
the stack.  Though the items array type declaration covers the maximum allowed size
range of a bounded stack, only size entries are actually allocated.  This technique allows
great savings in the amount of space actually used for each bounded stack.

top is initialized to zero when a stack is created, and this value is used to represent an
empty stack.  Furthermore, top may never exceed the value of size. Encountering this
condition is an indication of stack overflow.
*)


TYPE  ItemsArray   = ARRAY SizeRange OF Item;

TYPE  BoundedStack = RECORD
        dataID: TypeID;     (*-- Defined data type for this stack*)
        size  : SizeRange;  (*-- Maximum # of items on this stack *)
        top   : CARDINAL;   (*-- Current stack top := 0 *)
        items : ItemsArray; (*-- Dynamic array [1..size] of item  *)
      END (*-- BoundedStack *);

TYPE  Stack = POINTER TO BoundedStack;


(*
6.3.2	Exceptions

To support the exception handling mechanism two variables are needed.  The first,
stackError, is used to record the exception code from each operation; while handlers  is an
array of exception handling procedures indexed by the exception code.

The routines StackError, GetHandler, and SetHandler  have been previously described in
the definition module, and their operation should be readily apparent.

RaiseErrIn  is a local routine used to set the stackError variable and invoke the Raise
routine of the ErrorHandling module.
*)

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


(*
6.3.3	Constructors

Create begins by clearing the stackError  field under the assumption of a successful result.

The header for the stack must then be allocated in a local variable since the function result
cannot be manipulated but only returned.  The key to this allocation step is the
calculation of the number of bytes necessary based on the size of an individual item and
the number of items requested.  We must not forget the space for storing theSize, the
Type, and the stack top.  The constant minStackSize  accomplishes this regardless of the
number and size of these “static” fields.  The calculation is unaffected by changes in the
number or size of these fields that may come about due to future maintenance.  If the
bounded stack could not be allocated, the overflow exception must be raised, and the
NullStack returned.

At this point, all possibility of failure has been avoided and the bounded stack header can
be initialized to its empty state (top set to zero), and the size limit and data type ID  can
be stored for this bounded stack.  Lastly, the new stack can be returned to the caller.
Complexity O(1).
*)

PROCEDURE Create    (    theSize   : SizeRange (*-- in    *);
                         theType   : TypeID    (*-- in    *))
                                   : Stack     (*-- out   *);

CONST minStackSize = SIZE(BoundedStack) - SIZE(ItemsArray);

VAR   newStack : Stack;

BEGIN
  stackError := noerr;
  Allocate(newStack, minStackSize + SIZE(Item) * theSize);
  IF (newStack # NIL) THEN
    WITH newStack^ DO
      dataID := theType;
      size   := theSize;
      top    := 0;
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
Complexity O(n).
*)

PROCEDURE Destroy   (VAR theStack  : Stack     (*-- inout *));
BEGIN
  Clear(theStack);
  IF (stackError = noerr) THEN
    Deallocate(theStack, (SIZE(BoundedStack) - SIZE(ItemsArray)) +
                          SIZE(Item) * theStack^.size);
  END (*--if*);
END Destroy;
(*----------------------------*)


(*
After initializing stackError and verifying that the given stack is a valid object, Clear
retrieves the item disposal routine associated with the stack's data type from the
TypeManager module.  Though that module ensures that the disposal routine is not a NIL
procedure, it would be inefficient to repeatedly invoke a procedure that did nothing (most
compilers are not smart enough to avoid calling a NIL procedure nor one that does not
contain any executable statements).  So to clear the stack of items it may be necessary to
dispose of the individual items followed by setting top to zero. Complexity O(n).
*)

PROCEDURE Clear     (VAR theStack  : Stack     (*-- inout *));

VAR   itemIndex : CARDINAL;    (*-- loop index over items *)
      freeItem  : DisposeProc; (*-- item disposal routine *)

BEGIN
  stackError := noerr;
  IF (theStack # NIL) THEN
    WITH theStack^ DO
      freeItem := DisposeOf(dataID);
      IF (freeItem # NoDisposeProc) THEN
        FOR itemIndex := MIN(SizeRange) TO top DO
          freeItem(items[itemIndex]);
        END (*--for*);
      END (*--if*);
      top := 0;
    END (*--with*);
  ELSE
    RaiseErrIn(clear, undefined);
  END (*--if*);
END Clear;
(*----------------------------*)


(*
Assignment for bounded objects is simpler to implement than their unbounded
counterparts since the opportunity for overflow is restricted to when the target object is
(re-)created.  If the target object exists and is capable of holding all of the source object's
items the target can be safely cleared and its data type updated appropriately.  Otherwise,
the overflow exception is raised and the assignment operation is aborted.  When the target
object is undefined it must be created using the data type and size attributes of the source
object.  If overflow does not occur, the actual assignment can commence, otherwise its
suffices to exit (Create has already raised the exception).

The assignment operator cannot be used to copy the whole items array as only a slice of
the array's index range was actually allocated and who knows what other dynamically
allocated objects follow it in memory.  Nor can assignment be used to copy individual
items as the data type of those items is unknown; using assignment for dynamically
allocated items would cause structural sharing of items, which is not desired.
Complexity O(n).
*)

PROCEDURE Assign    (    theStack  : Stack     (*-- in    *);
                     VAR toStack   : Stack     (*-- inout *));

VAR   itemIndex : CARDINAL;   (*-- loop index over items *)
      assignItem: AssignProc; (*-- item assignment routine *)

BEGIN
  stackError := noerr;
  IF (theStack # NIL) THEN

    IF (toStack # NIL) THEN
      IF (theStack^.top <= toStack^.size) THEN
        Clear(toStack);
        toStack^.dataID := theStack^.dataID;
      ELSE
        RaiseErrIn(assign, overflow);
      END (*--if*);
    ELSE
      WITH theStack^ DO
        toStack := Create(size, dataID);
      END (*--with*);
    END (*--if*);
    IF (stackError # noerr) THEN
      RETURN;
    END (*--if*);

    WITH theStack^ DO
      assignItem := AssignOf(dataID);
      FOR itemIndex := MIN(SizeRange) TO top DO
        toStack^.items[itemIndex] := assignItem(items[itemIndex]);
      END (*--for*);
      toStack^.top := top;
    END (*--with*);
  ELSE
    RaiseErrIn(assign, undefined);
  END (*--if*);
END Assign;
(*----------------------------*)


(*
Push  is quite simple to implement: if the stack is not already full, the array index to the
stack top is advanced and theItem is stored there; otherwise the overflow exception is
raised. Of course, the undefined exception is raised if the source stack is NIL.
Complexity O(1).
*)

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


(*
Pop  is the inverse of Push, requiring a test for stack underflow, deallocation of the item,
and decrementing the stack top. Complexity O(1).

PopTopOf  is similar to Pop except the item is not deallocated, but instead is returned to
the caller. Complexity O(1).
*)

PROCEDURE Pop       (VAR theStack  : Stack     (*-- inout *));

VAR   freeItem : DisposeProc; (*-- item disposal routine *)

BEGIN
  stackError := noerr;
  IF (theStack # NIL) THEN
    WITH theStack^ DO
      IF (top # 0) THEN
        freeItem := DisposeOf(dataID);
        freeItem(items[top]);
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


(*
6.3.4	Selectors

IsDefined checks that the given stack object has been bound to a dynamic entity by
testing for a non-NIL pointer. Complexity O(1).
*)

PROCEDURE IsDefined (    theStack  : Stack     (*-- in    *))
                                   : BOOLEAN   (*-- out   *);
BEGIN
  RETURN (theStack # NIL);
END IsDefined;
(*----------------------------*)


(*
IsEmpty simply tests the index to the stack top being equal to zero, returning the
appropriate logical value.  As mentioned in the interface, an undefined stack is considered
empty. Complexity O(1).
*)

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


(*
IsEqual essentially scans both stacks looking for the first mismatch (inequality) which
indicates the stacks are unequal, otherwise if the FOR loop completes the stacks must be
equal.  This assumes that: (1) the stacks have been defined, (2) have been given the same
data type ID, and (3) have the same depth.  The first two of these assumptions, if
unfounded, raise the exceptions undefined and typeerror, respectively.  If the stacks do not
have the same depth then at least one item differs between them, and therefore they are
unequal. Complexity O(Min(m,n)).
*)

PROCEDURE IsEqual   (    left      : Stack     (*-- in    *);
                         right     : Stack     (*-- in    *))
                                   : BOOLEAN   (*-- out   *);

VAR   index : CARDINAL; (*-- loop index over items *)
      compare : CompareProc;	(*-- item comparison routine *)

BEGIN
  stackError := noerr;
  IF (left # NIL) & (right # NIL) THEN
    IF (left^.dataID = right^.dataID) THEN
      IF (left^.top = right^.top) THEN

	compare := CompareOf(left^.dataID);
        FOR index := MIN(SizeRange) TO left^.top DO
          IF (compare(left^.items[index], right^.items[index]) # equal) THEN
            RETURN FALSE;
          END (*--if*);
        END (*--for*);
        RETURN TRUE;

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


(*
Both SizeOf  and TypeOf  return the current values of size and dataID, respectively, for
the given stack.  Undefined stacks raise an exception and return so-called “null” values.
Complexity O(1).
*)

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


(*
6.3.5	Iterators

The two “looping” iterator routines, LoopOver and LoopChange, utilize the same
algorithm; the essential difference is the type of procedure parameter that is invoked for
each item processed.  A similar statement can be made about the two traversal routines,
Traverse and TravChange.  In all cases, the iteration begins with the top stack item and
ends with the bottom stack item.
*)

PROCEDURE LoopOver   (    theStack  : Stack          (*-- in    *);
                          theProcess: LoopAccessProc (*-- in    *));

VAR   index : CARDINAL; (*-- loop index over items *)

BEGIN
  stackError := noerr;
  IF (theStack # NIL) THEN
    WITH theStack^ DO
      FOR index := top TO MIN(SizeRange) BY -1 DO
        IF ~theProcess(items[index]) THEN
          RETURN;
        END (*--if*);
      END (*--for*);
    END (*--with*);
  ELSE
    RaiseErrIn(loopover, undefined);
  END (*--if*);
END LoopOver;
(*----------------------------*)

PROCEDURE LoopChange (    theStack  : Stack          (*-- in    *);
                          theProcess: LoopChangeProc (*-- in    *));

VAR   index : CARDINAL; (*-- loop index over items *)

BEGIN
  stackError := noerr;
  IF (theStack # NIL) THEN
    WITH theStack^ DO
      FOR index := top TO MIN(SizeRange) BY -1 DO
        IF ~theProcess(items[index]) THEN
          RETURN;
        END (*--if*);
      END (*--for*);
    END (*--with*);
  ELSE
    RaiseErrIn(loopchange, undefined);
  END (*--if*);
END LoopChange;
(*----------------------------*)

PROCEDURE Traverse   (    theStack  : Stack          (*-- in    *);
                          theProcess: AccessProc     (*-- in    *));

VAR   index : CARDINAL; (*-- loop index over items *)

BEGIN
  stackError := noerr;
  IF (theStack # NIL) THEN
    WITH theStack^ DO
      FOR index := top TO MIN(SizeRange) BY -1 DO
        theProcess(items[index]);
      END (*--for*);
    END (*--with*);
  ELSE
    RaiseErrIn(traverse, undefined);
  END (*--if*);
END Traverse;
(*----------------------------*)

PROCEDURE TravChange (    theStack  : Stack          (*-- in    *);
                          theProcess: ChangeProc     (*-- in    *));

VAR   index : CARDINAL; (*-- loop index over items *)

BEGIN
  stackError := noerr;
  IF (theStack # NIL) THEN
    WITH theStack^ DO
      FOR index := top TO MIN(SizeRange) BY -1 DO
        theProcess(items[index]);
      END (*--for*);
    END (*--with*);
  ELSE
    RaiseErrIn(traverse, undefined);
  END (*--if*);
END TravChange;
(*----------------------------*)


(*
6.3.6	Module Initialization

In the module initialization the local exception handlers array variables are set to default
handlers (ExitOnError) except for the noerr handler which is given the NullHandler.
stackError is given the value noerr avoiding an undefined state.
*)

BEGIN
  FOR stackError := initfailed TO MAX(Exceptions) DO
    SetHandler(stackError, ExitOnError);
  END (*--for*);
  SetHandler(noerr, NullHandler);
  stackError := noerr;
END StkSBMI.


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
[5]	N. Wirth, Algorithms and Data Structures, Prentice-Hall, Englewood Cliffs, NJ 1986,
	pp. 180-195.
[6]	N. Wirth, Algorithms + Data Structures = Programs, Prentice-Hall, Englewood Cliffs,
	NJ 1976.
[7]	N. Wirth, Programming in Modula-2, Springer-Verlag, New York, NY 1983.
*)