(*
6.2 Singly-Linked Bounded List Implementation

Now that the interface to the bounded list and the rationale
behind it has been covered we can move on to discuss the actual;
implementation details.
*)

IMPLEMENTATION MODULE ListSBM;
(*==========================================================
    Version  : 1.00  18 May 1989  C. Lins
    Compiler : TopSpeed Modula-2
    Component: Polylithic Structures - List
	Singly-Linked Bounded Managed

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
6.2.1 Internal Singly-linked Bounded List Representation

FIGURE 6.1

A list is a recursive data structure defined as either:

* an empty list (e.g., NIL), or
* the chain of an item and a list.

We shall model this structure directly with the chain of items
being represented by a Node containing the item itself and a
link to the next item in the list.  Then a list becomes a
pointer to a Node.  Since we are drawing nodes from a pool, the
pointers become indexes into the pool of nodes.  The pool itself,
shown above in figure 6.1, uses a singly-linked list as there
is no need to traverse the nodes backward.

Representation Invariants:
* an empty list is represented by NIL
* the last non-empty node has a next pointer of NIL

Unfortunately, this does not allow us to manage the structure's
data type as was the case for all of Volume 1 and the non-list
modules in this volume.  The consequence is that the programmer
must exercise greater discipline when using the module.
*)

TYPE  Node = RECORD
	item : Item; (*-- Data for the node *)
	next : List; (*-- Next node forward *)
      END (*-- Node *);

TYPE  NodeHeap = ARRAY PoolSize OF Node;
TYPE  NodePool = RECORD
	free  : List;
		size  : PoolSize;
		nodes : NodeHeap;
      END (*-- NodePool *);

TYPE  Pool = POINTER TO NodePool;

    (*--------------------*)

(*
6.2.2 Exceptions

The listError variable holds the exception result from each
operation and ListError simply returns this value.  It is
initialized to noerr in section 6.2.6 Module Initialization.
Each list operation, other than IsDefined, reinitializes
listError to noerr as its first statement.  RaiseErrIn is
used to assign an actual exception value to listError when
a list operation raises an exception.

handlers is an array of exception handling routines invoked
when an exception is raised.  Each exception is initialized to
ExitOnError other than noerr which is set to the NullHandler
in section 6.2.6 Module Initialization.  GetHandler and
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
6.2.3 Pool Constructors


InitPool attempts to allocate a node pool of the given size and
link all of its nodes together into the free list, (which is
singly-linked).  We use the variable sized array allocation
technique presented in the bounded component modules of Volume
1.  Complexity: O(s) where s is the size of the node pool.
*)

PROCEDURE InitPool  (    theSize : PoolSize (*-- in    *))
	: Pool     (*-- out   *);

CONST baseSize = SIZE(NodePool) - SIZE(NodeHeap);
CONST nodeSize = SIZE(Node);

VAR   index   : List; (*-- loop index for building free list *)
      newPool : Pool; (*-- new node pool being initialized *)

BEGIN
  listError := noerr;
  Allocate(newPool, baseSize + nodeSize * theSize);
  IF (newPool = NIL) THEN
    RaiseErrIn(modinit, initfailed);
  ELSE
	WITH newPool^ DO
	  FOR index := theSize - 1 TO MIN(PoolSize) BY -1 DO
		nodes[index].next := index + 1;
	  END (*--while*);
	  nodes[theSize].next := 0;
	  free := MIN(PoolSize);
	  size := theSize;
	 END (*--with*);
  END (*--if*);
  RETURN newPool;
END InitPool;
(*--------------------*)

(*
FreePool simply needs to return the storage originally
allocated to the pool back to the heap.  Complexity: O(1).
*)

PROCEDURE FreePool  (    thePool : Pool     (*-- inout *));

CONST baseSize = SIZE(NodePool) - SIZE(NodeHeap);
CONST nodeSize = SIZE(Node);

BEGIN
  listError := noerr;
  IF (thePool = NIL) THEN
    RaiseErrIn(modinit, undefined);
  ELSE
    Deallocate(thePool, baseSize + nodeSize * thePool^.size);
  END (*--if*);
END FreePool;
(*--------------------*)


(*
6.2.4 List Constructors

Create retrieves a node from the given pool, raising overflow and
returning the NullList if the node pool is already full.  It is
equivalent to the routine GetNode seen in Pascal texts.
Complexity: O(1).
*)

PROCEDURE Create    (    thePool : Pool     (*-- inout *))
	: List     (*-- out   *);

VAR   newNode : List;

BEGIN
  listError := noerr;
  IF (thePool = NIL) THEN
    RaiseErrIn(create, undefined);
    RETURN NullList;
  END (*--if*);
  WITH thePool^ DO
    newNode := free;
	IF (free = NullList) THEN
	  RaiseErrIn(create, overflow);
	ELSE
	  free := nodes[free].next;
	END (*--if*);
  END (*--with*);
  RETURN newNode;
END Create;
(*--------------------*)

(*
Destroy returns theList node to the node pool and is equivalent
to the routine FreeNode seen in Pascal texts.  The given list
node is simply added to the front of the available free list.
Complexity: O(1).
*)

PROCEDURE Destroy   (    thePool : Pool     (*-- inout *);
	VAR theList : List     (*-- inout *));
BEGIN
  listError := noerr;
  IF (thePool = NIL) THEN
    RaiseErrIn(destroy, undefined);
  ELSE
    WITH thePool^ DO
	  nodes[theList].next := free;
	  free := theList;
    END (*--with*);
  END (*--if*);
END Destroy;
(*--------------------*)

(*
Clear proceeds over each node and simply returns it to the pool's
free list. Complexity O(n).
*)

PROCEDURE Clear     (    thePool : Pool     (*-- inout *);
	VAR theList : List     (*-- inout *));

VAR   theNode : List; (*-- List node to be deallocated *)

BEGIN
  listError := noerr;
  IF (thePool = NIL) THEN
    RaiseErrIn(clear, undefined);
  ELSE
    WITH thePool^ DO
      WHILE (theList # NullList) DO
	theNode := theList;
	theList := nodes[theList].next;
	    nodes[theNode].next := free;
	    free := theNode;
      END (*--while*);
    END (*--with*);
  END (*--if*);
END Clear;
(*--------------------*)

(*
Assign copies the source list to the target list.  If the
two lists are the same, in other words point to the sam
node, the routine simply exits as the target already is
equal to the source.  Otherwise, the target list is cleared
of its present contents.  If the source list is not empty
the routine copies each node to the target using the TailInsert
routine from algorithm 4.2.2 in Gonnet [3, pg. 137].
Complexity: O(n).
*)

PROCEDURE Assign    (    thePool : Pool     (*-- inout *);
	theList : List     (*-- in    *);
	VAR toList  : List     (*-- inout *));

VAR   newNode : List; (*-- new node from source to target *)
      last    : List; (*-- last node inserted into target *)

  PROCEDURE TailInsert (    theNode : List (*-- in    *);
	                    VAR first   : List (*-- inout *);
                        VAR last    : List (*-- inout *));
  BEGIN
    IF (first = NullList) THEN
      first := theNode;
    ELSE
      thePool^.nodes[last].next := theNode;
    END (*--if*);
    last := theNode;
  END TailInsert;

BEGIN
  listError := noerr;
  IF (thePool = NIL) THEN
    RaiseErrIn(assign, undefined);
  ELSIF (theList # toList) THEN
    Clear(thePool, toList);
    last := NullList;
    WITH thePool^ DO
      WHILE (theList # NullList) DO
	    IF (free = NullList) THEN
	      RETURN;
	    END (*--if*);
	newNode := free;
	    free := nodes[free].next;

	    WITH nodes[newNode] DO
	      item := nodes[theList].item;
	      next := NullList;
	    END (*--with*);

	    TailInsert(newNode, toList, last);
	    theList := nodes[theList].next;
      END (*--while*);
    END (*--with*);
  END (*--if*);
END Assign;
(*--------------------*)

(*
SetItem takes the given list node and assigns theItem as its
data item value.  SetNext simply links the given node to the
next node given.  An exception is raised in both routines if
theList is empty.
Complexity: O(1)
*)

PROCEDURE SetItem   (    thePool : Pool     (*-- inout *);
	theList : List     (*-- inout *);
	theItem : Item     (*-- in    *));
BEGIN
  listError := noerr;
  IF (thePool = NIL) THEN
    RaiseErrIn(setitem, undefined);
  ELSIF (theList = NullList) THEN
    RaiseErrIn(setitem, listisnull);
  ELSE
    thePool^.nodes[theList].item := theItem;
  END (*--if*);
END SetItem;
(*--------------------*)

PROCEDURE SetNext   (    thePool : Pool     (*-- inout *);
	theList : List     (*-- inout *);
	newNext : List     (*-- in    *));
BEGIN
  listError := noerr;
  IF (thePool = NIL) THEN
    RaiseErrIn(setnext, undefined);
  ELSIF (theList = NullList) THEN
    RaiseErrIn(setnext, listisnull);
  ELSE
    thePool^.nodes[theList].next := newNext;
  END (*--if*);
END SetNext;
(*--------------------*)

(*
SetList constructs a list of length one out of the given item.
If the free list is empty the overflow exception is raised
and the NullList returned,
otherwise the top node is taken off the free list and the new
list node is updated with the item value and null previous and
next links.
Complexity: O(1).
*)

PROCEDURE SetList   (    thePool : Pool     (*-- inout *);
	theItem : Item     (*-- in    *))
	: List     (*-- out   *);

VAR   newList : List;

BEGIN
  listError := noerr;
  IF (thePool = NIL) THEN
    RaiseErrIn(setlist, undefined);
  ELSIF (thePool^.free = NullList) THEN
    RaiseErrIn(setlist, overflow);
  ELSE
    WITH thePool^ DO
      newList := free;
	  free    := nodes[free].next;
      WITH nodes[newList] DO
	    item := theItem;
	    next := NullList;
	  END (*--with*);
	END (*--with*);
	RETURN newList;
  END (*--if*);
  RETURN NullList;
END SetList;
(*--------------------*)

(*
Insert simply adds theItem to the front of theList by allocating
a new list node from the given node pool and linking its next
pointer to theList (which may be the NullList).
Complexity: O(1).
*)

PROCEDURE Insert    (    thePool : Pool     (*-- inout *);
	theItem : Item     (*-- in    *);
	VAR theList : List     (*-- inout *));

VAR   newList : List; (*-- new list node for theItem *)

BEGIN
  listError := noerr;
  IF (thePool = NIL) THEN
    RaiseErrIn(insert, undefined);
  ELSIF (thePool^.free = NullList) THEN
    RaiseErrIn(insert, overflow);
  ELSE
    WITH thePool^ DO
      newList := free;
	  free    := nodes[free].next;
      WITH nodes[newList] DO
	    item := theItem;
	    next := theList;
	  END (*--with*);
	  theList := newList;
	END (*--with*);
  END (*--if*);
END Insert;
(*--------------------*)


(*
6.2.5 Selectors

IsDefined returns true if thePool appears to point to something,
(i.e., not be NIL) and false otherwise.
Complexity: O(1).
*)

PROCEDURE IsDefined (    thePool : Pool     (*-- in    *))
	: BOOLEAN  (*-- out   *);
BEGIN
  RETURN (thePool # NIL);
END IsDefined;
(*--------------------*)

(*
IsEmpty returns True if theList is in the empty state, as indicated by
the list being equal to the null list, and False otherwise.  
Complexity: O(1).
*)

PROCEDURE IsEmpty   (    thePool : Pool     (*-- in    *);
	theList : List     (*-- in    *))
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

PROCEDURE IsEqual   (    thePool : Pool     (*-- in    *);
	left    : List     (*-- in    *);
	right   : List     (*-- in    *))
	: BOOLEAN  (*-- out   *);
BEGIN
  listError := noerr;
  IF (thePool = NIL) THEN
    RaiseErrIn(isequal, undefined);
	RETURN FALSE;
  ELSE
    WITH thePool^ DO
      WHILE (left # NullList) & (right # NullList) DO
	IF (nodes[left].item # nodes[right].item) THEN
	      RETURN FALSE;
	    END (*--if*);
	    left  := nodes[left].next;
	    right := nodes[right].next;
      END (*--while*);
      RETURN (left = right) & (right = NullList);
    END (*--with*);
  END (*--if*);
END IsEqual;
(*--------------------*)

(*
LengthOf simply loops over the links of the list counting
the number of list nodes it finds. Complexity: O(n).
*)

PROCEDURE LengthOf  (    thePool : Pool     (*-- in    *);
	theList : List     (*-- in    *))
	: CARDINAL (*-- out   *);

VAR   length : CARDINAL; (*-- Running count of items *)

BEGIN
  listError := noerr;
  length    := 0;
  IF (thePool = NIL) THEN
    RaiseErrIn(lengthof, undefined);
  ELSE
    WITH thePool^ DO
      WHILE (theList # NullList) DO
	    INC(length);
	    theList := nodes[theList].next;
      END (*--while*);
    END (*--with*);
  END (*--if*);
  RETURN length;
END LengthOf;
(*--------------------*)

(*
GetNext simply returns the link to the next list node, or if
given an empty list the listisnull exception is raised and the
null list is returned.  If thePool itself is invalid the undefined
exception is raised and the null item is returned.
Complexity: O(1).
*)

PROCEDURE GetNext   (    thePool : Pool     (*-- in    *);
	theList : List     (*-- in    *))
	: List     (*-- out   *);
BEGIN
  listError := noerr;
  IF (thePool = NIL) THEN
    RaiseErrIn(getnext, undefined);
    RETURN NullList;
  ELSIF (theList = NullList) THEN
    RaiseErrIn(getnext, listisnull);
    RETURN NullList;
  END (*--if*);
  RETURN thePool^.nodes[theList].next;
END GetNext;
(*--------------------*)

(*
GetItem simply returns the item of the given list node, or if
given an empty list the listisnull exception is raised and the
null item is returned.  If thePool itself is invalid the undefined
exception is raised and the null item is returned.
Complexity: O(1).
*)

PROCEDURE GetItem   (    thePool : Pool     (*-- in    *);
	theList : List     (*-- in    *))
	: Item     (*-- out   *);
BEGIN
  listError := noerr;
  IF (thePool = NIL) THEN
    RaiseErrIn(getitem, undefined);
    RETURN NullItem;
  ELSIF (theList = NullList) THEN
    RaiseErrIn(getitem, listisnull);
    RETURN NullItem;
  END (*--if*);
  RETURN thePool^.nodes[theList].item;
END GetItem;
(*--------------------*)


(*
6.2.6 Module Initialization

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
END ListSBM.

(*

References

[1] Aho, Hopcroft, and Ullman, Data Structures and Algorithms,
    Addison-Wesley, Reading MA 1983.
[2] G. Booch, Software Components with Ada, Structures, Tools, and Subsystems,
    Benjamin/Cummings, Menlo Park, CA 1987.
[3] G.H. Gonnet, Handbook of Algorithms and Data Structures,
    Addison-Wesley, London England 1984.
[4] D. Knuth, The Art of Computer Programming, Volume 1, Fundamental
    Algorithms, Addison-Wesley, Reading MA 1973.
[5] R. Wiener and R. Sincovec, Data Structures Using Modula-2,
    John Wiley & Sons, New York, NY, 1986, pg. 198.
[6] N. Wirth, Algorithms and Data Structures, Prentice-Hall,
    Englewood Cliffs, NJ 1986.
*)