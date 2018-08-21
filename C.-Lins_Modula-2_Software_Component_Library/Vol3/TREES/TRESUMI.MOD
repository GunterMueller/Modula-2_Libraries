(*
4.3 Unbounded Binary Search Tree Implementation

The internal structures used in representing an unbalanced, unbounded
binary search tree are described in this section along with the
algorithms implementing the operations defined in the interface.

This section is broken down as follows:
        * 4.3.1 Internal Representation
        * 4.3.2 Exception Handling
        * 4.3.3 Local Operations
        * 4.3.4 Tree Constructors
        * 4.3.5 Tree Selectors
        * 4.3.6 Passive Iterators
        * 4.3.7 Active Iterators
        * 4.3.8 Module Initialization
*)

IMPLEMENTATION MODULE TreSUMI;
(*==============================================================
    Version  : V2.01  08 December 1989.
    Compiler : JPI TopSpeed Modula-2
    Code size: OBJ file has 12075 bytes
    Component: Monolithic Structures - Tree (Opaque version)
               Sequential Unbounded Managed Iterator

    REVISION HISTORY
    v1.01  18 Mar 1988  C. Lins
      Initial implementation for TML Modula-2.
    v1.02  01 Oct 1988  C. Lins
      Updated and improved comments.
        v1.03  29 Jan 1989  C. Lins
          Changed to use Key and Data aliases for generic Items.
        v1.04  02 Feb 1989  C. Lins
          Optimized Assign to use NewNode instead of directly allocating
          new nodes. Assumes that assignKey and assignItem never fail.
    v1.05  06 Feb 1989  C. Lins
          Added use of InsertProc instead of FoundProc in Insert.
    v1.06  09 Feb 1989  C. Lins
          Removed VAR from Clear, Insert, & Remove (the tree itself
          does not change).
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
                           NotFoundProc, InsertProc, Key, Data, ComponentID;

FROM ErrorHandling IMPORT
    (*--Type*) HandlerProc,
    (*--Proc*) Raise, NullHandler, ExitOnError;

FROM TypeManager IMPORT
    (*--Cons*) NullType,
    (*--Type*) TypeID,
        (*--Proc*) AssignOf, DisposeOf, CompareOf;


    (*-----------------------*)

(*

4.3.1 Internal Representation

An unbounded binary search tree is represented using nodes for storing
keys, data items, and links between nodes the left and right subtrees.
Figure 4.1 depicts graphically this structural organization.

_Figure 4.1_

Link    provides a mechanism for maintaining the lexicographical ordering
                between nodes (i.e., the structure of the tree).

Node    holds a key value and its associated data, if any, along with
                links to the left and right subtrees.

UnboundedTree   defines a descriptor record for each unbounded tree
                object. This record holds the data type IDs for the key and
                data fields plus a link to the root node of the tree.

Tree    completes the opaque definition of the abstract tree.
*)

TYPE  Link = POINTER TO Node;
TYPE  Node = RECORD
                key   : Key;  (*--key value for this node *)
                data  : Data; (*--data value for this node *)
                left  : Link; (*--link to left subtree *)
                right : Link; (*--link to right subtree*)
          END (*--Node *);

TYPE  UnboundedTree = RECORD
                keyID  : TypeID; (*--data type for the tree's keys *)
        dataID : TypeID; (*--data type for this tree's items *)
                root   : Link;   (*--link to root of this tree *)
      END (*--UnboundedTree *);

TYPE  Tree = POINTER TO UnboundedTree;


        (*-----------------------*)

(*
4.3.2 Exception Handling

treeError holds the exception result from the most recently
invoked operation of this module. The Exceptions enumeration
constant noerr indicates successful completion of the operation and
all operations that may raise an exception assign this value to
treeError before any other processing.

The handler array holds the current exception handler for the
possible exceptions that may be raised from within this module.

Both are initialized by the module initialization (see section
4.3.8).

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
4.3.3 Local Operations

NewNode allocates and initializes a new leaf node for a tree.
Complexity O(1).
*)

PROCEDURE NewNode   (    theKey   : Key         (*--in   *);
                                                 theData  : Data        (*--in   *))
                                                          : Link        (*--out  *);

VAR   theNode : Link; (*--link to new leaf node being created *)

BEGIN
  Allocate(theNode, SIZE(Node));
  IF (theNode # NIL) THEN
    WITH theNode^ DO
      key   := theKey;
      data  := theData;
      left  := NIL;
      right := NIL;
    END (*--with*);
  END (*--if*);
  RETURN theNode;
END NewNode;
(*-------------------------*)


(*
4.3.4 Constructors

Create attempts to build a new empty tree of the given type (keyType
and dataType). First, the tree descriptor is allocated and the key
and data type IDs are stored there. The pointer to the root node is
initialized to the empty state (NIL). If the descriptor allocation
fails the overflow exception is raised and the NullTree is returned,
otherwise we return the newly allocated tree. Complexity O(1).
*)

PROCEDURE Create    (    keyType  : TypeID       (*--in   *);
                                                 dataType : TypeID       (*--in   *))
                                  : Tree         (*--out  *);

VAR   newTree : Tree; (*--temporary for new tree *)

BEGIN
  treeError := noerr;
  Allocate(newTree, SIZE(UnboundedTree));
  IF (newTree = NIL) THEN
    RaiseErrIn(create, overflow);
  ELSE
    WITH newTree^ DO
          keyID  := keyType;
      dataID := dataType;
          root   := NIL;
    END(*--with*);
  END(*--if*);
  RETURN newTree;
END Create;
(*-------------------------*)


(*
MakeTree is a combination of Create(keyType, dataType) immediately
followed by Insert(theKey, theData). Complexity O(1).
*)

PROCEDURE MakeTree  (    keyType  : TypeID       (*--in   *);
                                                 dataType : TypeID       (*--in   *);
                         theKey   : Key          (*--in   *);
                                                 theData  : Data         (*--in   *))
                                  : Tree         (*--out  *);

VAR   newTree : Tree; (*--new tree being created *)

BEGIN
  treeError := noerr;
  Allocate(newTree, SIZE(UnboundedTree));
  IF (newTree = NIL) THEN
    RaiseErrIn(maketree, overflow);
  ELSE
    WITH newTree^ DO
          keyID  := keyType;
      dataID := dataType;
          root   := NewNode(theKey, theData);
          IF (root = NIL) THEN
            RaiseErrIn(maketree, overflow);
                Deallocate(newTree, SIZE(newTree^));
          END (*--if*);
    END(*--with*);
  END(*--if*);
  RETURN newTree;
END MakeTree;
(*-------------------------*)


(*
Destroy lets Clear raise the undefined exception and simply releases
dynamically allocated memory resources for theTree back to the system.
SCLStorage.Deallocate automatically releases the proper amount of space
originally allocated and alters the pointer to NIL (which is also the
value of the NullTree). Complexity: O(n).
*)

PROCEDURE Destroy   (VAR theTree  : Tree         (*--inout*));
BEGIN
  Clear(theTree);
  IF (treeError = noerr) THEN
    Deallocate(theTree, SIZE(theTree^));
  END (*--if*);
END Destroy;
(*-------------------------*)


(*
Clear uses a postorder traversal of theTree, clearing the nodes of
both subtrees before clearing the tree itself. After disposing the
subtrees the key and data values can be disposed followed by the node.
The routine takes advantage of the fact that this version of Deallocate
sets the pointer to NIL after releasing the proper amount of memory.
This saves us from having to explicitly set the root to NIL.
Complexity O(n).
*)

PROCEDURE Clear     (    theTree  : Tree         (*--inout*));

VAR   freeData : DisposeProc; (*--data value disposal routine *)
          freeKey  : DisposeProc; (*--key value disposal routine *)

  PROCEDURE ClearNodes (VAR theSubtree : Link (*--inout*));
  BEGIN
    IF (theSubtree # NIL) THEN
          WITH theSubtree^ DO
            ClearNodes(left);
            ClearNodes(right);
                freeKey(key);
                freeData(data);
          END (*--with*);
          Deallocate(theSubtree, SIZE(theSubtree^));
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

PROCEDURE Assign    (    theTree  : Tree         (*--in   *);
                     VAR toTree   : Tree         (*--inout*));

VAR   assignKey  : AssignProc; (*--key item assignment routine *)
          assignItem : AssignProc; (*--data item assignment routine *)

  PROCEDURE RecreateTarget () : BOOLEAN (*--out *);
  BEGIN
    IF (theTree = NIL) THEN
          RaiseErrIn(assign, undefined);
    ELSIF (toTree = NIL) THEN
          WITH theTree^ DO
            toTree := Create(keyID, dataID);
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
    IF (theSubtree = NIL) THEN
          toSubtree := NIL;
        ELSE
          WITH theSubtree^ DO
            toSubtree := NewNode(assignKey(key), assignItem(data));
          END (*--with*);
          IF (toSubtree = NIL) THEN
            RaiseErrIn(assign, overflow);
          ELSE
            DoAssign(theSubtree^.left, toSubtree^.left);
            DoAssign(theSubtree^.right, toSubtree^.right);
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
This algorithm is the standard recursive one for binary tree insertion.
Complexity O(log2 n).
*)

PROCEDURE Insert    (    theTree  : Tree         (*--inout*);
                         theKey   : Key          (*--in   *);
                         theData  : Data         (*--in   *);
                                                 found    : InsertProc   (*--in   *));

VAR   compare : CompareProc; (*--key comparison routine *)

  PROCEDURE DoInsert (VAR theSubtree : Link (*--inout*));
  BEGIN
    IF (theSubtree = NIL) THEN
          theSubtree := NewNode(theKey, theData);
          IF (theSubtree = NIL) THEN
            RaiseErrIn(insert, overflow);
          END (*--if*);
        ELSE
          CASE compare(theSubtree^.key, theKey) OF
            less    : DoInsert(theSubtree^.right);
          | greater : DoInsert(theSubtree^.left);
          ELSE
            found(theSubtree^.key, theSubtree^.data, theData);
          END (*--case*);
        END (*--if*);
  END DoInsert;

BEGIN
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
          IF (subTree^.right # NIL) THEN
            SwapRemove(subTree^.right);
          ELSE
            oldTree^.key := subTree^.key;
                oldTree^.data := subTree^.data;
                oldTree := subTree;
                subTree := subTree^.left;
          END (*--if*);
        END SwapRemove;

  BEGIN
    IF (subTree = NIL) THEN

          notFound(theKey); (*--ERROR key not found in the tree *)

        ELSE
          CASE compare(theKey, subTree^.key) OF
            less    : DoRemove(subTree^.left);
          | greater : DoRemove(subTree^.right);
          ELSE
                (*--key found, delete it *)
                oldTree := subTree;
                IF (oldTree^.right = NIL) THEN
                  subTree := oldTree^.left;
                ELSIF (oldTree^.left = NIL) THEN
                  subTree := oldTree^.right;
                ELSE
                  SwapRemove(oldTree^.left);
                END (*--if*);
                freeKey(oldTree^.key);
                freeData(oldTree^.data);
                Deallocate(oldTree, SIZE(oldTree^));
          END (*--case*);
        END (*--if*);
  END DoRemove;

BEGIN  (*-- Remove --*)
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
4.3.5 Selectors

IsDefined verifies to the best of its ability whether theTree has been
created and is still an active object. Complexity: O(1).
*)

PROCEDURE IsDefined (    theTree  : Tree         (*--in   *))
                                  : BOOLEAN      (*--out  *);
BEGIN
  RETURN theTree # NullTree;
END IsDefined;
(*-------------------------*)


(*
IsEmpty returns True if theTree is in the empty state, as indicated by
the root being NIL, and False otherwise.  As per the specification
(section 3.3) undefined trees are considered empty. Complexity: O(1).
*)

PROCEDURE IsEmpty   (    theTree  : Tree         (*--in   *))
                                  : BOOLEAN      (*--out  *);
BEGIN
  treeError := noerr;
  IF (theTree # NIL) THEN
    RETURN (theTree^.root = NIL);
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

PROCEDURE IsEqual   (    left     : Tree         (*--in   *);
                         right    : Tree         (*--in   *))
                                  : BOOLEAN      (*--out  *);

VAR       compare : CompareProc;        (*-- item comparison routine *)

  PROCEDURE DoIsEqual (    leftSubtree  : Link    (*--in   *);
                                                   rightSubtree : Link    (*--in   *))
                                                                                : BOOLEAN (*--out  *);
  BEGIN
    IF (leftSubtree = NIL) OR (rightSubtree = NIL) THEN
          RETURN (leftSubtree = NIL) & (rightSubtree = NIL);
    ELSIF compare(leftSubtree^.key, rightSubtree^.key) # equal THEN
          RETURN FALSE;
        ELSE
          RETURN (DoIsEqual(leftSubtree^.left, rightSubtree^.left) &
                          DoIsEqual(leftSubtree^.right, rightSubtree^.right));
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
The two TypeOf routines simply return the key data ID or data ID for the given tree.
Undefined trees, as always, raise the undefined exception and return a reasonable value,
in this case the NullType. Complexity O(1).
*)

PROCEDURE KeyTypeOf (    theTree  : Tree         (*--in   *))
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
    IF (theSubtree # NIL) THEN
          WITH theSubtree^ DO
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
examination of each node's key. As noted by Wirth [8] and others, as
few as log2 n comparisons may be needed to find theKey if theTree is
perfectly balanced. The algorithmic complexity of the search is
therefore O(log2 n) where n is the number of nodes in the tree. It is
assumed that all keys are comparable and the compare procedure is not NIL.
*)

PROCEDURE IsPresent (    theTree  : Tree         (*--in   *);
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
        END (*--with*);

        LOOP
          IF (treeIndex = NIL) THEN
            notFound(theKey);
                EXIT (*--loop*);
          END (*--if*);

          CASE compare(treeIndex^.key, theKey) OF
            equal   : found(theKey, treeIndex^.data);
                                  EXIT (*--loop*);
          | less    : treeIndex := treeIndex^.right;
          | greater : treeIndex := treeIndex^.left;
          END (*--case*);
        END (*--loop*);

  ELSE
    RaiseErrIn(ispresent, undefined);
  END (*--if*);
END IsPresent;
(*-------------------------*)


(*
4.3.6 Passive Iterators

Each of the three iterator routines accomplish recursively Preorder,
Inorder, and Postorder traversals of the given tree. If the tree is
not defined, the undefined exception is raised and the traversal is
aborted. Otherwise, traversal begins with the root of the tree following
the specifications given in section 3.1.6.2. The complexity is O(n) for
all three traversals. Once again these are elementary tree algorithms that
can be found is any college textbook on data structures.
*)

PROCEDURE Preorder  (    theTree   : Tree       (*--in   *);
                         theProcess: AccessProc (*--in   *));

  PROCEDURE DoPreorder (    theSubtree : Link (*--in   *));
  BEGIN
    IF (theSubtree # NIL) THEN
          WITH theSubtree^ DO
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
    IF (theSubtree # NIL) THEN
          WITH theSubtree^ DO
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
    IF (theSubtree # NIL) THEN
          WITH theSubtree^ DO
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
4.3.7 Active Iterators

The active iterators given below simply return eomponents of tree nodes
and are thus, for the most part, self-explanatory.

The TML Modula-2 compiler prohibits us from redeclaring an opaque
type as equal to another type. (We cannot define a NodePtr = Link).
Thus, we must explicitly define NodePtr and then use the type transfer
facility provided by VAL to coerce the tree links into iterator nodes.
*)

TYPE  NodePtr  = POINTER TO Node;

PROCEDURE RootOf  (    theTree : Tree    (*--in   *))
                                                           : NodePtr (*--out  *);
BEGIN
  IF (theTree = NIL) THEN
    RETURN NullNode;
  END (*--if*);
  RETURN NodePtr( theTree^.root);
END RootOf;
(*-------------------------*)

PROCEDURE LeftOf  (    theNode : NodePtr (*--in   *))
                                                           : NodePtr (*--out  *);
BEGIN
  IF (theNode = NIL) THEN
    RETURN NullNode;
  END (*--if*);
  RETURN NodePtr(theNode^.left);
END LeftOf;
(*-------------------------*)

PROCEDURE RightOf (    theNode : NodePtr (*--in   *))
                                                           : NodePtr (*--out  *);
BEGIN
  IF (theNode = NIL) THEN
    RETURN NullNode;
  END (*--if*);
  RETURN NodePtr(theNode^.right);
END RightOf;
(*-------------------------*)

PROCEDURE IsNull  (    theNode : NodePtr (*--in   *))
                                                           : BOOLEAN (*--out  *);
BEGIN
  RETURN theNode = NIL;
END IsNull;
(*-------------------------*)

PROCEDURE KeyOf   (    theNode : NodePtr (*--in   *))
                                                           : Key     (*--out  *);
BEGIN
  IF (theNode = NIL) THEN
    RETURN NullItem;
  END (*--if*);
  RETURN theNode^.key;
END KeyOf;
(*-------------------------*)

PROCEDURE DataOf  (    theNode : NodePtr (*--in   *))
                                                           : Data    (*--out  *);
BEGIN
  IF (theNode = NIL) THEN
    RETURN NullItem;
  END (*--if*);
  RETURN theNode^.data;
END DataOf;
(*-------------------------*)


(*
4.3.8 Module Initialization

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
END TreSUMI.

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
[6] R.S. Wiener and G.A. Ford, Modula-2 A Software Development Approach,
    John Wiley & Sons, New York, NY 1985.
[7] R.S. Wiener and R.F. Sincovec, Data Structures Using Modula-2,
    John Wiley & Sons, New York, NY 1986.
[8]     N. Wirth, Algorithms and Data Structures, Prentice-Hall, Englewood Cliffs,
        NJ 1986.
*)
