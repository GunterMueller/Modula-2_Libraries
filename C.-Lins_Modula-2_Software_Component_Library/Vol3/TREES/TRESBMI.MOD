(*
5.2 Bounded Binary Search Tree Implementation

The internal structures used in representing an unbalanced, bounded
binary search tree are described in this section along with the
algorithms implementing the operations defined in the interface. All
routines in this module are simply tranformations of their unbounded
counterparts to use a bounded tree representation.

This section is broken down as follows:
        * 5.2.1 Internal Representation
        * 5.2.2 Exception Handling
        * 5.2.3 Free List Management
        * 5.2.4 Local Operations
        * 5.2.5 Tree Constructors
        * 5.2.6 Tree Selectors
        * 5.2.7 Passive Iterators
        * 5.2.8 Active Iterators
        * 5.2.9 Module Initialization
*)

IMPLEMENTATION MODULE TreSBMI;
(*==============================================================
    Version  : V2.01  08 December 1989.
    Compiler : JPI TopSpeed Modula-2
    Code Size: OBJ file has 13187 bytes.
    Component: Monolithic Structures - Tree (Opaque version)
               Sequential Bounded Managed Iterator

    REVISION HISTORY
    v1.01  18 Mar 1988  C. Lins
      Initial implementation for TML Modula-2.
    v1.02  01 Oct 1988  C. Lins
      Updated and improved comments.
    v1.03  29 Jan 1989  C. Lins
      Added aliases for generic Items as Keys and Data to enhance
          readability.
    v1.04  06 Feb 1989  C. Lins
          Added use of InsertProc instead of FoundProc in Insert.
    v1.05  09 Feb 1989  C. Lins
          Removed VAR from Clear, Insert, & Remove (the tree itself
          does not change).
    v1.06  21 Feb 1989  C. Lins
          Corrected Inorder, Preorder, & Postorder routine names
          being passed to RaiseErrIn when an exception was raised.
    v1.07  17 Apr 1989  C. Lins:
          Corrected IsEqual to use comparison routine derived
          from the CompareOf routine associated with the stack's
          TypeID.
    v1.04  18 Apr 1989   C. Lins:
          Added component id constant.
    v2.00  08 Oct 1989  C. Lins
          Created generic pc version
    v2.01  08 Dec 1989  I.S.C. Houston.
          Adapted to JPI Compiler:
          Used type transfer functions instead of VAL.
          Used shortened library module names for DOS and OS/2.

        (C) Copyright 1989 Charles A. Lins
==============================================================*)

FROM JPIStorage IMPORT
    (*--Proc*) Allocate, Deallocate;

FROM Relations IMPORT
        (*--Type*) Relation;

FROM Items IMPORT
        (*--Cons*) NullItem,
    (*--Type*) Item, AssignProc, CompareProc, DisposeProc;

FROM TreeTypes IMPORT
    (*--Type*) Operations, Exceptions, AccessProc, FoundProc,
                           InsertProc, NotFoundProc, Key, Data, ComponentID;

FROM ErrorHandling IMPORT
    (*--Type*) HandlerProc,
    (*--Proc*) Raise, NullHandler, ExitOnError;

FROM TypeManager IMPORT
    (*--Cons*) NullType,
    (*--Type*) TypeID,
        (*--Proc*) AssignOf, DisposeOf, CompareOf;


    (*-----------------------*)

(*
5.2.1 Internal Bounded Binary Search Tree Representation

To represent a bounded tree we use a pool of nodes for each bounded
tree, shown graphically in Figure 5.1, below.

_Figure 5.1_

Link    defines an index into the node pool and is used to maintain the
                relationships between nodes of the tree. The constant nil is
                used to represent an empty subtree.

Node    defines an entry in the tree consisting of a key value, a data
                value, and links to the left and right subtrees, if any.

NodePool        defines a pool of nodes of the largest size supported by
                the module.

BoundedTree     defines a descriptor for a bounded tree. The nodes array is
                allocated dynamically in size when the bounded tree descriptor
                is created and is only as large as needed for the specified
                size of the node pool.

Tree    defines a reference to the tree descriptor and its node pool as
                required by Modula-2.
*)

TYPE  Link = [0 .. MAX(TreeSize)];
CONST nil  = MIN(Link);
TYPE  Node = RECORD
                key   : Key ; (*--key value for this node *)
                data  : Data; (*--data value for this node *)
                left  : Link; (*--link to left subtree *)
                right : Link; (*--link to right subtree*)
          END (*--Node *);

TYPE  NodePool = ARRAY TreeSize OF Node;

TYPE  BoundedTree = RECORD
                keyID  : TypeID;   (*--data type for the tree's keys *)
        dataID : TypeID;   (*--data type for this tree's items *)
                size   : TreeSize; (*--maximum number of nodes *)
                root   : Link;     (*--link to root of this tree *)
                avail  : Link;     (*--link to available list of nodes *)
                nodes  : NodePool; (*--dynamic array [1..size] of node *)
      END (*--BoundedTree *);

TYPE  Tree = POINTER TO BoundedTree;


(*
5.2.2 Exception Handling

treeError holds the exception result from the most recently
invoked operation of this module. The Exceptions enumeration
constant noerr indicates successful completion of the operation and
all operations that may raise an exception assign this value to
treeError before any other processing.

The handler array holds the current exception handler for the
possible exceptions that may be raised from within this module.

Both are initialized by the module initialization (see section
5.2.9).

TreeError       simply returns the current exception result stored
                        in treeError and is used to determine whether a tree
                        operation completed successfully.

SetHandler      makes theHandler the current exception handler for theError
                        by storing theHandler in the handler array.

GetHandler      returns the current exception handler for theError from the
                        handler array.
*)

VAR   treeError : Exceptions;
VAR   handler   : ARRAY Exceptions OF HandlerProc;

PROCEDURE TreeError  ()              : Exceptions  (*--out  *);
BEGIN
  RETURN treeError;
END TreeError;
(*-------------------------*)

PROCEDURE GetHandler (    theError   : Exceptions  (*--in   *))
                                     : HandlerProc (*--out  *);
BEGIN
  RETURN handler[theError];
END GetHandler;
(*-------------------------*)

PROCEDURE SetHandler (    theError   : Exceptions  (*--in   *);
                          theHandler : HandlerProc (*--in   *));
BEGIN
  handler[theError] := theHandler;
END SetHandler;
(*-------------------------*)

PROCEDURE RaiseErrIn (    theRoutine : Operations (*--in   *);
                          theError   : Exceptions (*--in   *));
BEGIN
  treeError := theError;
  Raise(ComponentID + ModuleID, theRoutine, theError, handler[theError]);
END RaiseErrIn;
(*-------------------------*)


(*
5.2.3 Free List Management

The following routines manage a tree's free list of available nodes.
The head of the free list is kept in the avail field of the tree's
descriptor record. None of the routines check for invalid (i.e., NIL)
trees as they are all local of the module; nor do they raise any exceptions.
The algorithms used are all quite old (in computer science terms) and thus
should not require much explanation.

InitFreeList    initializes a tree's free list by linking together all of
                        the nodes of the pool.

GetNode         attempts to retrieve a node from the free list if one is available,
                        otherwise nil is returned.

FreeNode        returns a node to a tree's free list.
*)

PROCEDURE InitFreeList (VAR theTree : Tree   (*--inout*));

VAR   index : TreeSize; (*--loop index over nodes in the free list *)

BEGIN
  WITH theTree^ DO
    FOR index := MIN(TreeSize) TO size-1 DO
          nodes[index].left := index + 1;
        END (*--for*);
        nodes[size].left := nil;
        avail := MIN(TreeSize);
  END (*--with*);
END InitFreeList;
(*-------------------------*)

PROCEDURE GetNode (    theTree : Tree       (*--in   *)): Link  (*--out  *);

VAR   oldAvail : Link; (*--link to node from free list *)

BEGIN
  WITH theTree^ DO
    IF (avail = nil) THEN
          RETURN nil;
        ELSE
          oldAvail := avail;
          avail    := nodes[oldAvail].left;
          RETURN oldAvail;
        END (*--if*);
  END (*--with*);
END GetNode;
(*-------------------------*)

PROCEDURE FreeNode ( theTree : Tree (*--in   *); theNode : Link (*--in   *));
BEGIN
  WITH theTree^ DO
    nodes[theNode].left := avail;
    avail := theNode;
  END (*--with*);
END FreeNode;
(*-------------------------*)


(*
5.2.4 Local Operations

NewNode allocates and initializes a new leaf node for a tree.
Complexity O(1).
*)

PROCEDURE NewNode   (    theTree  : Tree     (*--in   *);
                                                 theKey   : Key      (*--in   *);
                                                 theData  : Data     (*--in   *))
                                                          : Link     (*--out  *);

VAR   theNode : Link; (*--link to new leaf node being created *)

BEGIN
  theNode := GetNode(theTree);
  IF (theNode # nil) THEN
    WITH theTree^.nodes[theNode] DO
      key   := theKey;
      data  := theData;
      left  := nil;
      right := nil;
    END (*--with*);
  END (*--if*);
  RETURN theNode;
END NewNode;
(*-------------------------*)


(*
5.2.5 Constructors

Create attempts to build a new empty tree of the given type.
First, the tree header is allocated and the key and data type IDs are
stored in the header. The pointer to the root node is initialized
to the empty state (0). If the header allocation fails the
overflow exception is raised and the NullTree is returned.
Complexity O(1).
*)

PROCEDURE Create    (    keyType  : TypeID    (*--in   *);
                         dataType : TypeID    (*--in   *);
                         theSize  : TreeSize  (*--in   *))
                                  : Tree      (*--out  *);

CONST minSize  = SIZE(BoundedTree) - SIZE(NodePool);
CONST nodeSize = SIZE(Node);

VAR   newTree : Tree;

BEGIN
  treeError := noerr;
  Allocate(newTree, minSize + nodeSize * VAL(INTEGER, theSize));
  IF (newTree = NIL) THEN
    RaiseErrIn(create, overflow);
  ELSE
    WITH newTree^ DO
          keyID  := keyType;
          dataID := dataType;
          size   := theSize;
          root   := nil;
          InitFreeList(newTree);
    END(*--with*);
  END(*--if*);
  RETURN newTree;
END Create;
(*-------------------------*)

(*
MakeTree is a combination of Create(keyType, dataType, theSize)
immediately followed by Insert(theKey, theData). Complexity O(1).
*)

PROCEDURE MakeTree  (    keyType  : TypeID    (*--in   *);
                                                 dataType : TypeID    (*--in   *);
                                                 theSize  : TreeSize  (*--in   *);
                         theKey   : Key       (*--in   *);
                                                 theData  : Data      (*--in   *))
                                  : Tree      (*--out  *);

VAR   newTree : Tree;

BEGIN
  newTree := Create(keyType, dataType, theSize);
  IF (treeError = noerr) THEN
    newTree^.root := NewNode(newTree, theKey, theData);
  ELSE
    RaiseErrIn(maketree, overflow);
  END (*--if*);
  RETURN newTree;
END MakeTree;
(*-------------------------*)


(*
Destroy lets Clear raise the undefined exception and simply releases
dynamically allocated memory resources for theTree back to the system.
The amount of space originally allocated is released back to the system
and the pointer is altered by JPIStorage.Deallocate to NIL
(which is also the value of the NullTree). Complexity: O(n).
*)

PROCEDURE Destroy   (VAR theTree  : Tree      (*--inout*));

CONST minSize  = SIZE(BoundedTree) - SIZE(NodePool);
CONST nodeSize = SIZE(Node);

BEGIN
  Clear(theTree);
  IF (treeError = noerr) THEN
    Deallocate(theTree, minSize + nodeSize * theTree^.size);
  END (*--if*);
END Destroy;
(*-------------------------*)


(*
Clear uses a postorder traversal of theTree, clearing the nodes of
both subtrees before clearing the tree itself. After disposing the
subtrees the key and data values can be disposed followed by the node.
Complexity O(n).
*)

PROCEDURE Clear     (    theTree  : Tree      (*--inout*));

VAR   freeData : DisposeProc; (*--data value disposal routine *)
          freeKey  : DisposeProc; (*--key value disposal routine *)

  PROCEDURE ClearNodes (VAR theSubtree : Link (*--inout*));
  BEGIN
    IF (theSubtree # nil) THEN
          WITH theTree^.nodes[theSubtree] DO
            ClearNodes(left);
            ClearNodes(right);
                freeKey(key);
                freeData(data);
                FreeNode(theTree, theSubtree);
          END (*--with*);
        END (*--if*);
  END ClearNodes;

BEGIN
  treeError := noerr;
  IF (theTree = NIL) THEN
    RaiseErrIn(clear, undefined);
  ELSE
    WITH theTree^ DO
      freeKey  := DisposeOf(keyID);
      freeData := DisposeOf(dataID);
      ClearNodes(root);
          root := nil;
        END (*--with*);
  END (*--if*);
END Clear;
(*-------------------------*)


(*
Assign uses a preorder traversal of the source tree to generate a
copy in the destination tree. Preliminary to the actual copying,
we must ensure that the source tree is defined and clear or create
the destination tree as necessary. This step is accomplished by the
RecreateTarget routine which must accomodate the following cases:
        * the source tree is undefined, and thus, the target tree must be
          left unchanged;
        * the source tree and target tree are the same and therefore the
          postcondition of the Assign operation is already met;
        * the source tree is defined but the target tree is undefined, so
          the target tree must be created with the same key and data type
          id's as the source tree; and
        * both the source and target trees are defined, and thus the target
          tree must be cleared of its contents followed by its key and data
          key id's being set to the same as the source tree.

In the second case, we automatically return FALSE so that Assign will
bypass the node copying operation. While in the other three instances,
success depends on whether treeError remains set to noerr.

The main body of Assign uses the result from RecreateTarget to determine
whether to continue with the copy operation after recreating the target tree.
Complexity O(m+n) where m is the number of nodes in the destination
tree and n is the number of nodes in the source tree.
*)

PROCEDURE Assign    (    theTree  : Tree      (*--in   *);
                     VAR toTree   : Tree      (*--inout*));

VAR   assignKey  : AssignProc; (*--key item assignment routine *)
          assignItem : AssignProc; (*--data item assignment routine *)

  PROCEDURE RecreateTarget () : BOOLEAN (*--out *);
  BEGIN
    IF (theTree = NIL) THEN
      RaiseErrIn(assign, undefined);
    ELSIF (toTree = NIL) THEN
      WITH theTree^ DO
        toTree := Create(keyID, dataID, size);
          END (*--with*);
    ELSIF (toTree = theTree) THEN
      RETURN FALSE;
    ELSE
      Clear(toTree);
      WITH theTree^ DO
            toTree^.keyID  := keyID;
            toTree^.dataID := dataID;
          END (*--with*);
    END (*--if*);
        RETURN treeError = noerr;
  END RecreateTarget;

  PROCEDURE DoAssign (    theSubtree : Link (*--in   *);
                                          VAR toSubtree  : Link (*--out  *));
  BEGIN
    IF (theSubtree = nil) THEN
          toSubtree := nil;
        ELSE
          toSubtree := GetNode(toTree);
          IF (toSubtree = nil) THEN
            RaiseErrIn(assign, overflow);
          ELSE
            WITH toTree^.nodes[toSubtree] DO
                  key   := assignKey(theTree^.nodes[theSubtree].key);
                  data  := assignItem(theTree^.nodes[theSubtree].data);
                  left  := nil;
                  right := nil;
                END (*--with*);
            DoAssign(theTree^.nodes[theSubtree].left,
                                 toTree^.nodes[toSubtree].left);
            DoAssign(theTree^.nodes[theSubtree].right,
                                 toTree^.nodes[toSubtree].right);
          END (*--if*);
        END (*--if*);
  END DoAssign;

BEGIN
  treeError := noerr;
  IF RecreateTarget() THEN
    WITH theTree^ DO
      assignKey  := AssignOf(keyID);
      assignItem := AssignOf(dataID);
      DoAssign(root, toTree^.root);
        END (*--with*);
  END (*--if*);
END Assign;
(*-------------------------*)

(*
Insert adds a node with theKey and theData to theTree and places the
node within its proper position maintaining the search tree property.
This algorithm is the standard recursive one for binary tree insertion
converted to use the pool of nodes instead of the heap.
Complexity O(log2 n).
*)

PROCEDURE Insert    (    theTree  : Tree       (*--inout*);
                         theKey   : Key        (*--in   *);
                         theData  : Data       (*--in   *);
                                                 found    : InsertProc (*--in   *));

VAR   compare : CompareProc; (*--key comparison routine *)

  PROCEDURE DoInsert (VAR theSubtree : Link (*--inout*));
  BEGIN
    IF (theSubtree = nil) THEN
          theSubtree := NewNode(theTree, theKey, theData);
          IF (theSubtree = nil) THEN
            RaiseErrIn(insert, overflow);
          END (*--if*);
        ELSE
          WITH theTree^.nodes[theSubtree] DO
            CASE compare(key, theKey) OF
              less    : DoInsert(right);
            | greater : DoInsert(left);
            ELSE
              found(key, data, theData);
            END (*--case*);
          END (*--with*);
        END (*--if*);
  END DoInsert;

BEGIN (*--Insert --*)
  treeError := noerr;
  IF (theTree = NIL) THEN
    RaiseErrIn(insert, undefined);
  ELSE
    WITH theTree^ DO
      compare := CompareOf(keyID);
      DoInsert(root);
        END (*--with*);
  END (*--if*);
END Insert;
(*-------------------------*)


(*
Remove searches theTree for the first node containing theKey. If no such
node exists the notFound procedure parameter is called. Otherwise, we
use the standard binary tree deletion algorithm as given by Wirth [8] and
many others (see references).
Complexity O(log2 n).
*)

PROCEDURE Remove    (    theTree  : Tree         (*--inout*);
                         theKey   : Key          (*--in   *);
                                                 notFound : NotFoundProc (*--in   *));

VAR   compare  : CompareProc; (*--key comparison routine *)
          freeKey  : DisposeProc; (*--key disposal routine *)
          freeData : DisposeProc; (*--data disposal routine *)

  PROCEDURE DoRemove (VAR subTree : Link (*--inout*));

  VAR   oldTree : Link; (*--link to subtree to be disposed *)

    PROCEDURE SwapRemove (VAR subTree : Link (*--inout*));
        BEGIN
          WITH theTree^ DO
            WITH nodes[subTree] DO
              IF (right # nil) THEN
                SwapRemove(right);
              ELSE
                nodes[oldTree].key  := key;
                    nodes[oldTree].data := data;
                    oldTree := subTree;
                    subTree := left;
              END (*--if*);
            END (*--with*);
          END (*--with*);
        END SwapRemove;

  BEGIN (*--DoRemove --*)
    IF (subTree = nil) THEN

          notFound(theKey); (*--ERROR key not found in the tree *)

        ELSE
          WITH theTree^ DO
            CASE compare(theKey, nodes[subTree].key) OF
              less    : DoRemove(nodes[subTree].left);
            | greater : DoRemove(nodes[subTree].right);
            ELSE
                  (*--key found, delete it *)
                  oldTree := subTree;
                  IF (nodes[oldTree].right = nil) THEN
                    subTree := nodes[oldTree].left;
                  ELSIF (nodes[oldTree].left = nil) THEN
                    subTree := nodes[oldTree].right;
                  ELSE
                    SwapRemove(nodes[oldTree].left);
                  END (*--if*);
                  WITH nodes[oldTree] DO
                    freeKey(key);
                    freeData(data);
                  END (*--with*);
                  FreeNode(theTree, oldTree);
            END (*--case*);
          END (*--with*);
        END (*--if*);
  END DoRemove;

BEGIN (*--Remove --*)
  treeError := noerr;
  IF (theTree = NIL) THEN
    RaiseErrIn(remove, undefined);
  ELSE
    WITH theTree^ DO
      compare := CompareOf(keyID);
          freeKey := DisposeOf(keyID);
          freeData:= DisposeOf(dataID);
      DoRemove(root);
        END (*--with*);
  END (*--if*);
END Remove;
(*-------------------------*)


(*
5.2.6 Selectors

IsDefined verifies to the best of its ability whether theTree has been
created and is still an active object. Complexity O(1).
*)

PROCEDURE IsDefined  (    theTree  : Tree         (*--in   *))
                                   : BOOLEAN      (*--out  *);
BEGIN
  RETURN theTree # NullTree;
END IsDefined;
(*-------------------------*)

(*
IsEmpty returns True if theTree is in the empty state, as indicated by
the root being nil (0), and False otherwise.  As per the specification
(section 3.3) undefined trees are considered empty. Complexity O(1).
*)

PROCEDURE IsEmpty    (    theTree  : Tree         (*--in   *))
                                   : BOOLEAN      (*--out  *);
BEGIN
  treeError := noerr;
  IF (theTree # NIL) THEN
    RETURN (theTree^.root = nil);
  END (*--if*);
  RaiseErrIn(isempty, undefined);
  RETURN TRUE;
END IsEmpty;
(*-------------------------*)

(*
IsEqual uses a preorder traversal of both left and right trees. As soon
as an inequality between keys is found we can return false as the trees
cannot be equal. Complexity O(Min(m,n)).
*)

PROCEDURE IsEqual    (    left     : Tree         (*--in   *);
                          right    : Tree         (*--in   *))
                                   : BOOLEAN      (*--out  *);

VAR       compare : CompareProc;        (*-- item comparison routine *)

  PROCEDURE DoIsEqual (    leftSubtree  : Link    (*--in   *);
                                                   rightSubtree : Link    (*--in   *))
                                                                                : BOOLEAN (*--out  *);
  BEGIN
    IF (leftSubtree = nil) OR (rightSubtree = nil) THEN
          RETURN (leftSubtree = nil) & (rightSubtree = nil);
    ELSIF compare(left^.nodes[leftSubtree].key,
                                  right^.nodes[rightSubtree].key) # equal THEN
          RETURN FALSE;
        ELSE
          RETURN (DoIsEqual(left^.nodes[leftSubtree].left,
                                                right^.nodes[rightSubtree].left) &
                          DoIsEqual(left^.nodes[leftSubtree].right,
                                                right^.nodes[rightSubtree].right));
        END (*--if*);
  END DoIsEqual;

BEGIN
  treeError := noerr;
  IF (left = NIL) OR (right = NIL) THEN
    RaiseErrIn(isequal, undefined);
  ELSIF (left^.dataID # right^.dataID) OR
                (left^.keyID # right^.keyID) THEN
    RaiseErrIn(isequal, typeerror);
  ELSE
        compare := CompareOf(left^.keyID);
    RETURN DoIsEqual(left^.root, right^.root);
  END (*--if*);
  RETURN FALSE;
END IsEqual;
(*-------------------------*)

(*
SizeOf simply returns the defined size for the given tree. KeyTypeOf and
DataTypeOf return the key type ID and data type ID, respectively, for the
given tree. Undefined trees, as always, raise the undefined exception and
return a reasonable value, in this case either zero for SizeOf or the
NullType for the TypeOf routines. All three routines have complexity O(1).
*)

PROCEDURE SizeOf     (    theTree  : Tree         (*--in   *))
                                   : CARDINAL     (*--out  *);
BEGIN
  treeError := noerr;
  IF (theTree # NIL) THEN
    RETURN theTree^.size;
  END (*--if*);
  RaiseErrIn(sizeof, undefined);
  RETURN 0;
END SizeOf;
(*-------------------------*)

PROCEDURE KeyTypeOf  (    theTree  : Tree         (*--in   *))
                                   : TypeID       (*--out  *);
BEGIN
  treeError := noerr;
  IF (theTree # NIL) THEN
    RETURN theTree^.keyID;
  END (*--if*);
  RaiseErrIn(typeof, undefined);
  RETURN NullType;
END KeyTypeOf;
(*-------------------------*)

PROCEDURE DataTypeOf (    theTree  : Tree         (*--in   *))
                                   : TypeID       (*--out  *);
BEGIN
  treeError := noerr;
  IF (theTree # NIL) THEN
    RETURN theTree^.dataID;
  END (*--if*);
  RaiseErrIn(typeof, undefined);
  RETURN NullType;
END DataTypeOf;
(*-------------------------*)

(*
ExtentOf returns the number of nodes present in the given tree or zero
for an undefined tree. We simply employ an inorder traversal of the tree
counting the nodes along the way. Complexity O(n).
*)

PROCEDURE ExtentOf   (    theTree  : Tree         (*--in   *))
                                   : CARDINAL     (*--out  *);

VAR   count : CARDINAL; (*--running count of nodes in tree *)

  PROCEDURE CountNodes (    theSubtree : Link (*--in   *));
  BEGIN
    IF (theSubtree # nil) THEN
          WITH theTree^.nodes[theSubtree] DO
            CountNodes(left);
            INC(count);
            CountNodes(right);
          END (*--with*);
        END (*--if*);
  END CountNodes;

BEGIN
  treeError := noerr;
  count     := 0;
  IF (theTree = NIL) THEN
    RaiseErrIn(extentof, undefined);
  ELSE
    CountNodes(theTree^.root);
  END (*--if*);
  RETURN count;
END ExtentOf;
(*-------------------------*)

(*
IsPresent uses an iterative traversal of the given tree attempting
to find node in theTree containing theKey value. The search path
begins at the root switching to the left or right subtree based on
examination of each node's key. As noted by Wirth [9] and others, as
few as log2 n comparisons may be needed to find theKey if theTree is
perfectly balanced. The algorithmic complexity of the search is
therefore O(log2 n) where n is the number of nodes in the tree.
It is assumed that
all keys are comparable and the compare procedure is not NIL.
*)

PROCEDURE IsPresent  (    theTree  : Tree         (*--in   *);
                          theKey   : Key          (*--in   *);
                                                  found    : FoundProc    (*--in   *);
                                                  notFound : NotFoundProc (*--in   *));

VAR   treeIndex : Link;
      compare   : CompareProc; (*--key comparison routine *)

BEGIN
  treeError := noerr;
  IF (theTree # NIL) THEN
    WITH theTree^ DO
      treeIndex := root;
          compare   := CompareOf(keyID);
          LOOP
            IF (treeIndex = nil) THEN
              notFound(theKey);
                  EXIT (*--loop*);
            END (*--if*);
            WITH nodes[treeIndex] DO
              CASE compare(key, theKey) OF
                equal   : found(theKey, data);
                                      EXIT (*--loop*);
              | less    : treeIndex := right;
              | greater : treeIndex := left;
              END (*--case*);
            END (*--with*);
          END (*--loop*);
        END (*--with*);

  ELSE
    RaiseErrIn(ispresent, undefined);
  END (*--if*);
END IsPresent;
(*-------------------------*)


(*
5.2.7 Passive Iterators

Each of the three iterator routines accomplish recursively Preorder,
Inorder, and Postorder traversals of the given tree. If the tree is
not defined, the undefined exception is raised and the traversal is
aborted. Otherwise, traversal begins with the root of the tree following
the specifications given in section 3.1.6.2. The complexity is O(n) for
all three traversals.
*)

PROCEDURE Preorder  (    theTree   : Tree       (*--in   *);
                         theProcess: AccessProc (*--in   *));

  PROCEDURE DoPreorder (    theSubtree : Link (*--in   *));
  BEGIN
    IF (theSubtree # nil) THEN
          WITH theTree^.nodes[theSubtree] DO
            theProcess(key, data);
            DoPreorder(left);
            DoPreorder(right);
          END (*--with*);
        END (*--if*);
  END DoPreorder;

BEGIN
  treeError := noerr;
  IF (theTree = NIL) THEN
    RaiseErrIn(preorder, undefined);
  ELSE
    DoPreorder(theTree^.root);
  END (*--if*);
END Preorder;
(*-------------------------*)

PROCEDURE Inorder   (    theTree   : Tree       (*--in   *);
                         theProcess: AccessProc (*--in   *));

  PROCEDURE DoInorder (    theSubtree : Link (*--in   *));
  BEGIN
    IF (theSubtree # nil) THEN
          WITH theTree^.nodes[theSubtree] DO
            DoInorder(left);
            theProcess(key, data);
            DoInorder(right);
          END (*--with*);
        END (*--if*);
  END DoInorder;

BEGIN
  treeError := noerr;
  IF (theTree = NIL) THEN
    RaiseErrIn(inorder, undefined);
  ELSE
    DoInorder(theTree^.root);
  END (*--if*);
END Inorder;
(*-------------------------*)

PROCEDURE Postorder (    theTree   : Tree       (*--in   *);
                         theProcess: AccessProc (*--in   *));

  PROCEDURE DoPostorder (    theSubtree : Link (*--in   *));
  BEGIN
    IF (theSubtree # nil) THEN
          WITH theTree^.nodes[theSubtree] DO
            DoPostorder(left);
            DoPostorder(right);
            theProcess(key, data);
          END (*--with*);
        END (*--if*);
  END DoPostorder;

BEGIN
  treeError := noerr;
  IF (theTree = NIL) THEN
    RaiseErrIn(postorder, undefined);
  ELSE
    DoPostorder(theTree^.root);
  END (*--if*);
END Postorder;
(*-------------------------*)


(*
5.2.8 Active Iterators

The active iterators given below simply return eomponents of tree nodes
and are thus self-explanatory.
*)

PROCEDURE RootOf  (    theTree : Tree    (*--in   *))
                                                           : NodePtr (*--out  *);
BEGIN
  IF (theTree = NIL) THEN
    RETURN nil;
  END (*--if*);
  RETURN theTree^.root;
END RootOf;
(*-------------------------*)

PROCEDURE LeftOf  (    theTree : Tree    (*--in   *);
                                           theNode : NodePtr (*--in   *))
                                                           : NodePtr (*--out  *);
BEGIN
  IF (theTree = NIL) OR (theNode = nil) THEN
    RETURN nil;
  END (*--if*);
  RETURN theTree^.nodes[theNode].left;
END LeftOf;
(*-------------------------*)

PROCEDURE RightOf (    theTree : Tree    (*--in   *);
                                           theNode : NodePtr (*--in   *))
                                                           : NodePtr (*--out  *);
BEGIN
  IF (theTree = NIL) OR (theNode = nil) THEN
    RETURN nil;
  END (*--if*);
  RETURN theTree^.nodes[theNode].right;
END RightOf;
(*-------------------------*)

PROCEDURE IsNull  (    theNode : NodePtr (*--in   *))
                                                           : BOOLEAN (*--out  *);
BEGIN
  RETURN theNode = nil;
END IsNull;
(*-------------------------*)

PROCEDURE KeyOf   (    theTree : Tree    (*--in   *);
                                           theNode : NodePtr (*--in   *))
                                                           : Key     (*--out  *);
BEGIN
  IF (theTree = NIL) OR (theNode = nil) THEN
    RETURN NullItem;
  END (*--if*);
  RETURN theTree^.nodes[theNode].key;
END KeyOf;
(*-------------------------*)

PROCEDURE DataOf  (    theTree : Tree    (*--in   *);
                                           theNode : NodePtr (*--in   *))
                                                           : Data    (*--out  *);
BEGIN
  IF (theTree = NIL) OR (theNode = nil) THEN
    RETURN NullItem;
  END (*--if*);
  RETURN theTree^.nodes[theNode].data;
END DataOf;
(*-------------------------*)


(*
5.2.9 Module Initialization

The module's local variables are initialized to known states.
treeError is used to fill the handlers array with a routine
that will exit the program when an exception is raised (saving the
declaration of a special loop control variable for this purpose).
The condition noerr is given the NullHandler which is presumed to
do nothing.  Applying MIN and MAX to cover all exceptions followed
by resetting the handler for noerr ensures that this initialization
will be unaffected by any future changes to the number of Exceptions
or their order of declaration within the enumeration.  Since a FOR loop
control variable is undefined following the loop, treeError must be
set to indicate that an error has not yet occurred.
*)

BEGIN
  FOR treeError := MIN(Exceptions) TO MAX(Exceptions) DO
    SetHandler(treeError, ExitOnError);
  END (*--for*);
  SetHandler(noerr, NullHandler);
  treeError := noerr;
        NullTree := NIL;
END TreSBMI.

(*
References


[1] A. Aho, J. Hopcroft, and J. Ullman, Data Structures and Algorithms,
    Addison-Wesley, Reading, MA 1983.
[2] G. Booch, Software Components in Ada Structures, Tools, and Subsystems,
        Benjamin/Cummings, Menlo Park, CA 1987.
[3]     G.H. Gonnet, Handbook of Algorithms and Data Structures, Addison-Wesley,
        Reading, MA 1984.
[4] K. John Gough, ðWriting Generic Utilities in Modula-2ð, Journal of
    Pascal, Ada, and Modula-2, Vol. 5(3), (May/June 1986), pp 53-62.
[5] T.A. Standish, Data Structure Techniques, Addison-Wesley, Reading, MA 1980.
[6] A. Tenenbaum and M.J. Augenstein, Data Structures Using Pascal, Prentice-Hall,
        Englewood Cliffs, NJ 1981.
[7] R.S. Wiener and G.A. Ford, Modula-2 A Software Development Approach,
    John Wiley & Sons, New York, NY 1985.
[8] R.S. Wiener and R.F. Sincovec, Data Structures Using Modula-2,
    John Wiley & Sons, New York, NY 1986.
[9]     N. Wirth, Algorithms and Data Structures, Prentice-Hall, Englewood Cliffs,
        NJ 1986.
*)
