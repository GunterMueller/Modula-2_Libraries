(*
5.2 Doubly-Linked Unbounded List Implementation

Below, the implementation of the doubly-linked unbounded list
is proffered.
*)

IMPLEMENTATION MODULE ListDUM;
(*==========================================================
    Version  : 1.00  18 May 1989  C. Lins
    Compiler : TopSpeed Modula-2
    Component: Polylithic Structures - List
	Doubly-Linked Unbounded Managed

    The Abstraction
    This module provides the List ADT for generic Items.

    Revision History
    v1.00  18 May 1989  C. Lins
      Initial implementation for TopSpeed Modula-2.

    (C) Copyright 1989 Charles A. Lins
==========================================================*)

FROM JPIStorage IMPORT
    (*--Proc*) Allocate, Deallocate;

FROM Items IMPORT
    (*--Cons*) NullItem,
    (*--Type*) Item;

FROM ErrorHandling IMPORT
    (*--Type*) HandlerProc,
    (*--Proc*) NullHandler, Raise, ExitOnError;

FROM ListEnum IMPORT
    (*--Type*) Exceptions, Operations, ComponentID;

    (*-----------------------*)

(*
5.2.1 Internal Doubly-linked Unbounded List Representation

A list is a recursive data structure defined as either:

* an empty list (e.g., NIL), or
* the chain of an item and a list.

We shall model this structure directly with the chain of items
being represented by a Node containing the item itself and a
link to the next and previous items in the list.  Then a list
becomes a pointer to a Node.

FIGURE 5.1

Representation Invariants:
* an empty list is represented by NIL
* the first non-empty node has a prev pointer of NIL
* the last non-empty node has a next pointer of NIL

Unfortunately, this does not allow us to manage the structure's
data type as was the case for all of Volume 1 and the non-list
modules in this volume.  The consequence is that the programmer
must exercise greater discipline when using the module.
*)

TYPE  List = POINTER TO Node;
TYPE  Node = RECORD
	prev : List; (*-- Next node backward *)
	item : Item; (*-- Data for the node *)
	next : List; (*-- Next node forward *)
      END (*-- Node *);

    (*--------------------*)

(*
5.2.2 Exceptions

The listError variable holds the exception result from each
operation and ListError simply returns this value.  It is
initialized to noerr in section 5.2.5 Module Initialization.
Each list operation reinitializes listError to noerr as its
first statement.  RaiseErrIn is used to assign an actual
exception value to listError when a list operation raises
an exception.

handlers is an array of exception handling routines invoked
when an exception is raised.  Each exception is initialized to
ExitOnError other than noerr which is set to the NullHandler
in section 5.2.5 Module Initialization.  GetHandler and
SetHandler allow assignment and retrieval, respectively, of
the exception handlers for specific exceptions.
*)

VAR   listError : Exceptions;
VAR   handlers  : ARRAY Exceptions OF HandlerProc;


PROCEDURE ListError   ()	: Exceptions  (*-- out   *);
BEGIN
  RETURN listError;
END ListError;
(*--------------------*)

PROCEDURE GetHandler  (    ofError   : Exceptions  (*-- in    *))
	: HandlerProc (*-- out   *);
BEGIN
  RETURN handlers[ofError];
END GetHandler;
(*--------------------*)

PROCEDURE SetHandler  (    ofError   : Exceptions  (*-- in    *);
	theHandler: HandlerProc (*-- in    *));
BEGIN
  handlers[ofError] := theHandler;
END SetHandler;
(*--------------------*)

PROCEDURE RaiseErrIn  (    theRoutine: Operations  (*-- in    *);
	theError  : Exceptions  (*-- in    *));
BEGIN
  listError := theError;
  Raise(ComponentID + ModuleID, theRoutine, theError, handlers[theError]);
END RaiseErrIn;
(*--------------------*)


(*
5.2.3 Constructors

Create simply returns the NullList which is our representation
of an empty list.  Data type is not managed by this list
implementation in the interests of simplicity. Complexity: O(1).
*)

PROCEDURE Create    () : List	(*-- out   *);
BEGIN
  listError := noerr;
  RETURN NullList;
END Create;
(*--------------------*)

(*
Destroy simply clears the list which also destroys it.
Complexity: O(n).
*)

PROCEDURE Destroy   (VAR theList : List (*-- inout *));
BEGIN
  Clear(theList);
END Destroy;
(*--------------------*)

(*
Clear proceeds over each node and simply deallocates it.
Complexity: O(n).
*)

PROCEDURE Clear     (VAR theList : List (*-- inout *));

VAR   theNode : List; (*-- List node to be deallocated *)

BEGIN
  listError := noerr;
  WHILE (theList # NullList) DO
    theNode := theList;
    theList := theList^.next;
    Deallocate(theNode, SIZE(theNode^));
  END (*--while*);
END Clear;
(*--------------------*)

(*
Assign copies the source list to the target list.  If the
two lists are the same, in other words point to the same
node, the routine simply exits as the target already is
equal to the source.  Otherwise, the target list is cleared
of its present contents.  If the source list is not empty
the routine copies each node to the target using the TailInsert
routine which is a variation on that given in algorithm
4.2.2 in Gonnet [2, pg. 137].  TailInsert automatically sets
the new node's previous link to the last node inserted. In
order for this to work correctly last must be initialized to
the NullList (NIL).
Complexity: O(n).
*)

PROCEDURE TailInsert(VAR theNode : List (*-- inout *);
	VAR first   : List (*-- inout *);
					 VAR last    : List (*-- inout *));
BEGIN
  IF (first = NullList) THEN
    first := theNode;
  ELSE
    last^.next := theNode;
  END (*--if*);
  theNode^.prev := last;
  last := theNode;
END TailInsert;

PROCEDURE Assign    (    theList : List (*-- in    *);
	VAR toList  : List (*-- inout *));

VAR   newNode : List; (*-- new node from source to target *)
      last    : List; (*-- last node inserted into target *)

BEGIN
  IF (theList # toList) THEN
    Clear(toList);
    last := NullList;
    WHILE (theList # NullList) DO
      Allocate(newNode, SIZE(Node));
	  IF (newNode = NullList) THEN
	    RaiseErrIn(assign, overflow);
	    RETURN;
	  END (*--if*);

	  WITH newNode^ DO
	    item := theList^.item;
	    next := NullList;
	  END (*--with*);

	  TailInsert(newNode, toList, last);
	  theList := theList^.next;
    END (*--while*);
  END (*--if*);
END Assign;
(*--------------------*)

(*
SetItem takes the given list node and assigns theItem as its
data item value.  SetNext simply links the given node to the
next node given, while SetPrev similarly links the node to the
previous node given.
In all three routines, if theList is empty an exception is raised.
Similarly, all three routines have a time complexity of O(1).
*)

PROCEDURE SetItem   (    theList : List (*-- inout *);
	theItem : Item (*-- in    *));
BEGIN
  listError := noerr;
  IF (theList = NullList) THEN
    RaiseErrIn(setitem, listisnull);
  ELSE
    theList^.item := theItem;
  END (*--if*);
END SetItem;
(*--------------------*)

PROCEDURE SetNext   (    theList : List (*-- inout *);
	newNext : List (*-- in    *));
BEGIN
  listError := noerr;
  IF (theList = NullList) THEN
    RaiseErrIn(setnext, listisnull);
  ELSE
    theList^.next := newNext;
  END (*--if*);
END SetNext;
(*--------------------*)

PROCEDURE SetPrev   (    theList : List (*-- inout *);
	newPrev : List (*-- in    *));
BEGIN
  listError := noerr;
  IF (theList = NullList) THEN
    RaiseErrIn(setprev, listisnull);
  ELSE
    theList^.prev := newPrev;
  END (*--if*);
END SetPrev;
(*--------------------*)

(*
Insert simply adds theItem to the front of theList by allocating
a new list node and linking its next pointer to theList (which
may be the NullList).  Before theList's previous pointer is linked to
the new node, if a node preceeds theList then its next link
must also be updated to account for the new list node as well.
Complexity: O(1).
*)

PROCEDURE Insert    (    theItem : Item (*-- in    *);
	VAR theList : List (*-- inout *));

VAR   newList : List; (*-- new list node for theItem *)

BEGIN
  listError := noerr;
  Allocate(newList, SIZE(Node));
  IF (newList = NullList) THEN
    RaiseErrIn(insert, overflow);
  ELSE
    WITH newList^ DO
	  IF (theList = NullList) THEN
	    prev := NullList;
	  ELSE
	    prev := theList^.prev;
	  END (*--if*);
	  item := theItem;
	  next := theList;
	END (*--with*);
	IF (theList # NullList) & (theList^.prev # NullList) THEN
	  theList^.prev^.next := newList;
    END (*--if*);
	IF (theList # NullList) THEN
	  theList^.prev := newList;
    END (*--if*);
    theList := newList;
  END (*--if*);
END Insert;
(*--------------------*)

(*
SetList constructs a list of length one out of the given item.
If a list node cannot be allocated from the heap the overflow
exception is raised and the NullList returned.
Complexity O(1).
*)

PROCEDURE SetList   (    theItem : Item (*-- in    *))
	: List (*-- out   *);

VAR   newList : List;

BEGIN
  listError := noerr;
  Allocate(newList, SIZE(Node));
  IF (newList = NullList) THEN
    RaiseErrIn(setlist, overflow);
  ELSE
    WITH newList^ DO
	  item := theItem;
	  next := NullList;
	  prev := NullList;
	END (*--with*);
  END (*--if*);
  RETURN newList;
END SetList;
(*--------------------*)


(*
5.2.4 Selectors

IsEmpty returns True if theList is in the empty state, as indicated by
the list being equal to the null list, and False otherwise.  
Complexity: O(1).
*)

PROCEDURE IsEmpty   (    theList : List     (*-- in    *))
	: BOOLEAN  (*-- out   *);
BEGIN
  listError := noerr;
  RETURN (theList = NullList);
END IsEmpty;
(*--------------------*)

(*
IsEqual compares the left and right list for equality, which in this
context means they contain the same items.  The algorithm simply loops
over each of the items in both lists returning false immediately upon
encountering an inequality, and returning true if and only if every
item is the same between them. This condition is detected by
both list pointers being the null list at the end of the loop.
Complexity: O(Min(m,n)).
*)

PROCEDURE IsEqual   (    left    : List     (*-- in    *);
	right   : List     (*-- in    *))
	: BOOLEAN  (*-- out   *);
BEGIN
  listError := noerr;
  WHILE (left # NullList) & (right # NullList) DO
    IF (left^.item # right^.item) THEN
	  RETURN FALSE;
	END (*--if*);
	left  := left^.next;
	right := right^.next;
  END (*--while*);
  RETURN (left = right) & (right = NullList);
END IsEqual;
(*--------------------*)

(*
LengthOf simply loops over the links of the list counting
the number of list nodes it finds. Complexity: O(n).
*)

PROCEDURE LengthOf  (    theList : List     (*-- in    *))
	: CARDINAL (*-- out   *);

VAR   length : CARDINAL; (*-- Running count of items *)

BEGIN
  listError := noerr;
  length    := 0;
  WHILE (theList # NullList) DO
	INC(length);
	theList := theList^.next;
  END (*--while*);
  RETURN length;
END LengthOf;
(*--------------------*)

(*
GetNext simply returns the link to the next list node, or if
given an empty list the listisnull exception is raised and the
null list is returned.  Complexity: O(1).
*)

PROCEDURE GetNext   (    theList : List     (*-- in    *))
	: List     (*-- out   *);
BEGIN
  listError := noerr;
  IF (theList = NullList) THEN
    RaiseErrIn(getnext, listisnull);
    RETURN NullList;
  END (*--if*);
  RETURN theList^.next;
END GetNext;
(*--------------------*)

(*
GetPrev simply returns the link to the previous list node, or if
given an empty list the listisnull exception is raised and the
null list is returned.  Complexity: O(1).
*)

PROCEDURE GetPrev   (    theList : List     (*-- in    *))
	: List     (*-- out   *);
BEGIN
  listError := noerr;
  IF (theList = NullList) THEN
    RaiseErrIn(getprev, listisnull);
    RETURN NullList;
  END (*--if*);
  RETURN theList^.prev;
END GetPrev;
(*--------------------*)

(*
GetItem simply returns the item of the given list node, or if
given an empty list the listisnull exception is raised and the
null item is returned.  Complexity: O(1).
*)

PROCEDURE GetItem   (    theList : List     (*-- in    *))
	: Item     (*-- out   *);
BEGIN
  listError := noerr;
  IF (theList = NullList) THEN
    RaiseErrIn(getitem, listisnull);
    RETURN NullItem;
  END (*--if*);
  RETURN theList^.item;
END GetItem;
(*--------------------*)


(*
5.2.5 Module Initialization

The module's local variables are initialized to known states.
listError is used to fill the handlers array with a routine
that will exit the program when an exception is raised (saving the
declaration of a special loop control variable for this purpose).
The condition noerr is given the NullHandler which is presumed to
do nothing.  Applying MIN and MAX to cover all exceptions followed
by resetting the handler for noerr ensures that this initialization
will be unaffected by any future changes to the number of Exceptions
or their order of declaration within the enumeration.

Since a FOR loop control variable is undefined following the loop,
listError must be set to indicate that an error has not yet occurred.
*)

BEGIN
  FOR listError := MIN(Exceptions) TO MAX(Exceptions) DO
    SetHandler(listError, ExitOnError);
  END (*--for*);
  SetHandler(noerr, NullHandler);
  listError := noerr;
END ListDUM.

(*

References
[1] G. Booch, Software Components with Ada, Structures, Tools, and Subsystems,
    Benjamin/Cummings, Menlo Park, CA 1987.
[2] G.H. Gonnet, Handbook of Algorithms and Data Structures,
    Addison-Wesley, London England 1984.
[3] D. Knuth, The Art of Computer Programming, Volume 1, Fundamental
    Algorithms, Addison-Wesley, Reading, MA 1973.
[4] R. Wiener and R. Sincovec, Data Structures Using Modula-2,
    John Wiley & Sons, New York, NY 1986, pg. 198.
[5] N. Wirth, Algorithms and Data Structures, Prentice-Hall,
    Englewood Cliffs, NJ 1986.
*)