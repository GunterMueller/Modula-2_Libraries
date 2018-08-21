(*
8.2 Unbounded Path k-Balanced Binary Search Tree Implementation

The internal structures used in representing a path-balanced,
unbounded binary search tree are described in this section along
with the algorithms implementing the operations defined in the
interface.

This section is broken down as follows:
        * 8.2.1 Internal Representation
        * 8.2.2 Exception Handling
        * 8.2.3 Local Operations
        * 8.2.4 Tree Constructors
        * 8.2.5 Tree Selectors
        * 8.2.6 Passive Iterators
        * 8.2.7 Active Iterators
        * 8.2.8 Module Initialization
*)

IMPLEMENTATION MODULE IPBSUMI;
(*==============================================================
    Version  : V2.01  08 December 1989.
    Compiler : JPI TopSpeed Modula-2
    Code Size: OBJ file is 14272 bytes
    Component: Monolithic Structures - IPBk Tree (Opaque version)
               Sequential Unbounded Managed Iterator

    REVISION HISTORY
    v1.01  19 May 1988  C. Lins
      Initial implementation for TML Modula-2.
    v1.02  01 Oct 1988  C. Lins
      Updated and improved comments.
    v1.03  29 Jan 1989  C. Lins
          Added use of Key and Data aliases for generic Items.
    v1.04  03 Feb 1989  C. Lins
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
8.2.1 Internal Unbounded IPB Binary Search Tree Representation

An unbounded path-balanced binary search tree is represented using
nodes for storing keys, data items, links between nodes the left
and right subtrees, and the weight of each node.
Figure 8.1 depicts graphically this structural organization.

_Figure 8.1_

Link    provides a mechanism for maintaining the lexicographical ordering
                between nodes (i.e., the structure of the tree).

Node    holds a key value and its associated data, if any, along with
                links to the left and right subtrees. In addition, a weight field
                is maintained in order to detect when rebalancing the tree is
                necessary.

UnboundedTree   defines a descriptor record for each unbounded tree
                object. This record holds the data type IDs for the key and
                data fields plus a link to the root node of the tree. Furthermore,
                we must keep track of the balancing control factor, k, which
                describes how far out of balance a path may become before
                rebalancing is necessary.

Tree    completes the opaque definition of the abstract tree.
*)

TYPE  Link = POINTER TO Node;
TYPE  Node = RECORD
                key   : Key;    (*-- key value for this node *)
                data  : Data;   (*-- data value for this node *)
                weight: Weight; (*-- # of external nodes rooted at this tree *)
                left  : Link;   (*-- link to left subtree *)
                right : Link;   (*-- link to right subtree*)
          END (*-- Node *);

TYPE  Tree = POINTER TO UnboundedTree;

TYPE  UnboundedTree = RECORD
                keyID  : TypeID;   (*-- data type for the tree's keys *)
        dataID : TypeID;   (*-- data type for this tree's items *)
                k      : CARDINAL; (*-- k-Balance rotation control factor *)
                root   : Link;     (*-- link to root of this tree *)
      END (*-- UnboundedTree *);


(*
8.2.2 Exceptions

treeError holds the exception result from the most recently
invoked operation of this module. The Exceptions enumeration
constant noerr indicates successful completion of the operation and
all operations that may raise an exception assign this value to
treeError before any other processing.

The handler array holds the current exception handler for the
possible exceptions that may be raised from within this module.

Both are initialized by the module initialization (see section
8.2.8).

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


PROCEDURE TreeError  ()              : Exceptions  (*-- out   *);
BEGIN
  RETURN treeError;
END TreeError;
(*-------------------------*)

PROCEDURE GetHandler (    theError   : Exceptions  (*-- in    *))
                                     : HandlerProc (*-- out   *);
BEGIN
  RETURN handler[theError];
END GetHandler;
(*-------------------------*)

PROCEDURE SetHandler (    theError   : Exceptions  (*-- in    *);
                          theHandler : HandlerProc (*-- in    *));
BEGIN
  handler[theError] := theHandler;
END SetHandler;
(*-------------------------*)

PROCEDURE RaiseErrIn (    theRoutine : Operations (*-- in    *);
                          theError   : Exceptions (*-- in    *));
BEGIN
  treeError := theError;
  Raise(ComponentID + ModuleID, theRoutine, theError, handler[theError]);
END RaiseErrIn;
(*-------------------------*)


(*
8.2.3 Local Operations

NewNode allocates and initializes a new leaf node for a tree.
By definition, the number of external nodes rooted at a given
subtree is two, and thus, the weight factor is initialized to
this value. Complexity O(1).
*)

PROCEDURE NewNode   (    theKey   : Key         (*-- in    *);
                                                 theData  : Data        (*-- in    *);
                                                 theWeight: Weight      (*-- in    *))
                                                          : Link        (*-- out   *);

VAR   theNode : Link; (*-- link to new leaf node being created *)

BEGIN
  Allocate(theNode, SIZE(Node));
  IF (theNode # NIL) THEN
    WITH theNode^ DO
      key   := theKey;
      data  := theData;
          weight:= theWeight;
      left  := NIL;
      right := NIL;
    END (*--with*);
  END (*--if*);
  RETURN theNode;
END NewNode;
(*-------------------------*)

(*
Wt returns the weight value for a given node, which is the number of
external nodes in the subtree rooted at the node. A subtree of NIL
returns 1, otherwise t^.weight.
*)

PROCEDURE Wt (    theSubtree : Link   (*-- in    *))
                                                         : Weight (*-- out   *);
BEGIN
  IF (theSubtree = NIL) THEN
    RETURN 1;
  END (*--if*);
  RETURN theSubtree^.weight;
END Wt;
(*-------------------------*)

(*
The routines LeftRotation and RightRotation perform a single left
rotation and single right rotation of the given subtree, respectively.
Rotations were previously described in section 3.3.6, Tree Rotations.
Both routines have an algorithmic complexity of O(1).
*)

PROCEDURE LeftRotation (VAR theSubtree : Link (*-- inout *));

VAR   temporary : Link;

BEGIN
  temporary  := theSubtree;
  theSubtree := theSubtree^.right;
  temporary^.right := theSubtree^.left;
  theSubtree^.left := temporary;

  (*-- adjust weights --*)

  WITH temporary^ DO
    theSubtree^.weight := weight;
    weight := Wt(left) + Wt(right);
  END (*--with*);
END LeftRotation;
(*-------------------------*)

PROCEDURE RightRotation (VAR theSubtree : Link (*-- inout *));

VAR   temporary : Link;

BEGIN
  temporary  := theSubtree;
  theSubtree := theSubtree^.left;
  temporary^.left   := theSubtree^.right;
  theSubtree^.right := temporary;

  (*-- adjust weights --*)

  WITH temporary^ DO
    theSubtree^.weight := weight;
    weight := Wt(left) + Wt(right);
  END (*--with*);
END RightRotation;
(*-------------------------*)

(*
CheckRotations checks the balancing in the tree, performs any necessary
rotation, and checks whether further rotations may be needed. The global
parameter kBalance controls the frequency of rotations on the tree. The
higher the value of kBalance the fewer number of rotations. A single
rotation occurs only when Nc - Na >= kBalance, while a double rotation
occurs only when Nb - Na >= kBalance. The
value of kBalance should be tailored as necessary for particular real applications.
kBalance is an INTEGER (though it is never less than one) as the comparison
between the number of nodes in the subtrees may yield a negative result.
*)

VAR   kBalance : INTEGER; (*-- rotation control factor *)

PROCEDURE CheckRotations (VAR theSubtree : Link    (*--inout *));

VAR   wtLeft : Weight;
      wtRight: Weight;

BEGIN
  IF (theSubtree # NIL) THEN
    WITH theSubtree^ DO
      wtLeft := Wt(left);
          wtRight:= Wt(right);
        END (*--with*);

        IF (wtRight > wtLeft) THEN

          (*-- left rotation needed --*)

          IF VAL(INTEGER, Wt(theSubtree^.right^.right)) -
                 VAL(INTEGER, wtLeft) >= kBalance THEN
            LeftRotation(theSubtree);
                CheckRotations(theSubtree^.left);

          ELSIF VAL(INTEGER, Wt(theSubtree^.right^.left)) -
                        VAL(INTEGER, wtLeft) >= kBalance THEN
            RightRotation(theSubtree^.right);
            LeftRotation(theSubtree);
                CheckRotations(theSubtree^.left);
                CheckRotations(theSubtree^.right);
          END (*--if*);

        ELSIF (wtLeft > wtRight) THEN

          (*-- right rotation needed --*)

          IF VAL(INTEGER, Wt(theSubtree^.left^.left)) -
                 VAL(INTEGER, wtRight) >= kBalance THEN
            RightRotation(theSubtree);
                CheckRotations(theSubtree^.right);

          ELSIF VAL(INTEGER, Wt(theSubtree^.left^.right)) -
                        VAL(INTEGER, wtRight) >= kBalance THEN
            LeftRotation(theSubtree^.left);
            RightRotation(theSubtree);
                CheckRotations(theSubtree^.left);
                CheckRotations(theSubtree^.right);
          END (*--if*);
        END (*--if*);
  END (*--if*);
END CheckRotations;
(*-------------------------*)


(*
8.2.4 Constructors

Create attempts to build a new empty tree of the given type.
First, the tree header is allocated and the key and data type IDs are
stored in the header. The pointer to the root node is initialized
to the empty state (NIL). If the header allocation fails the
overflow exception is raised and the NullTree is returned.
Complexity O(1).
*)

PROCEDURE Create    (    keyType  : TypeID       (*-- in    *);
                                                 dataType : TypeID       (*-- in    *);
                                                 kBalance : CARDINAL     (*-- in    *))
                                  : Tree         (*-- out   *);

VAR   newTree : Tree; (*-- temporary for new tree *)

BEGIN
  treeError := noerr;
  Allocate(newTree, SIZE(UnboundedTree));
  IF (newTree = NIL) THEN
    RaiseErrIn(create, overflow);
  ELSE
    WITH newTree^ DO
          keyID  := keyType;
      dataID := dataType;
          IF (kBalance = 0) THEN
            kBalance := 1;
          END (*--if*);
          k    := kBalance;
          root := NIL;
    END(*--with*);
  END(*--if*);
  RETURN newTree;
END Create;
(*-------------------------*)

(*
MakeTree is a combination of Create(keyType, dataType) immediately
followed by Insert(theKey, theData). Complexity O(1).
*)

PROCEDURE MakeTree  (    keyType  : TypeID       (*-- in    *);
                                                 dataType : TypeID       (*-- in    *);
                                                 kBalance : CARDINAL     (*-- in    *);
                         theKey   : Key          (*-- in    *);
                                                 theData  : Data         (*-- in    *))
                                  : Tree         (*-- out   *);

VAR   newTree : Tree; (*-- new tree being created *)

BEGIN
  treeError := noerr;
  Allocate(newTree, SIZE(UnboundedTree));
  IF (newTree = NIL) THEN
    RaiseErrIn(maketree, overflow);
  ELSE
    WITH newTree^ DO
          keyID  := keyType;
      dataID := dataType;
          IF (kBalance = 0) THEN
            kBalance := 1;
          END (*--if*);
          k    := kBalance;
          root := NewNode(theKey, theData, 2);
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

PROCEDURE Destroy   (VAR theTree  : Tree         (*-- inout *));
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

PROCEDURE Clear     (    theTree  : Tree         (*-- inout *));

VAR   freeData : DisposeProc; (*-- data value disposal routine *)
          freeKey  : DisposeProc; (*-- key value disposal routine *)

  PROCEDURE ClearNodes (VAR theSubtree : Link (*-- inout *));
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

PROCEDURE Assign    (    theTree  : Tree         (*-- in    *);
                     VAR toTree   : Tree         (*-- inout *));

VAR   assignKey  : AssignProc; (*-- key item assignment routine *)
          assignItem : AssignProc; (*-- data item assignment routine *)

  PROCEDURE RecreateTarget () : BOOLEAN (*--out *);
  BEGIN
    IF (theTree = NIL) THEN
          RaiseErrIn(assign, undefined);
    ELSIF (toTree = NIL) THEN
          WITH theTree^ DO
            toTree := Create(keyID, dataID, k);
          END (*--with*);
    ELSIF (toTree = theTree) THEN
          RETURN FALSE;
    ELSE
          Clear(toTree);
          WITH theTree^ DO
            toTree^.keyID  := keyID;
            toTree^.dataID := dataID;
            toTree^.k      := k;
          END (*--with*);
    END (*--if*);
        RETURN treeError = noerr;
  END RecreateTarget;

  PROCEDURE DoAssign (    theSubtree : Link (*-- in    *);
                                          VAR toSubtree  : Link (*-- out   *));
  BEGIN
    IF (theSubtree = NIL) THEN
          toSubtree := NIL;
        ELSE
          WITH theSubtree^ DO
            toSubtree := NewNode(assignKey(key), assignItem(data), weight);
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
node within its proper position to maintain the search tree property.
Complexity worst case number of rotations is O(n) but the amortized
worst case per insertion is O(log2 n).
*)

PROCEDURE Insert    (    theTree  : Tree         (*-- inout *);
                         theKey   : Key          (*-- in    *);
                         theData  : Data         (*-- in    *);
                                                 found    : InsertProc   (*-- in    *));

VAR   compare : CompareProc; (*-- key comparison routine *)

  PROCEDURE DoInsert (VAR theSubtree : Link (*-- inout *));
  BEGIN
    IF (theSubtree = NIL) THEN
          theSubtree := NewNode(theKey, theData, 2);
          IF (theSubtree = NIL) THEN
            RaiseErrIn(insert, overflow);
          END (*--if*);
        ELSE
          CASE compare(theSubtree^.key, theKey) OF
            less    : DoInsert(theSubtree^.right);
          | greater : DoInsert(theSubtree^.left);
          ELSE
            found(theSubtree^.key, theSubtree^.data, theData);
                RETURN;
          END (*--case*);
          WITH theSubtree^ DO
            weight := Wt(left) + Wt(right);
          END (*--with*);
          CheckRotations(theSubtree);
        END (*--if*);
  END DoInsert;

BEGIN
  treeError := noerr;
  IF (theTree = NIL) THEN
    RaiseErrIn(insert, undefined);
  ELSE
    WITH theTree^ DO
      compare  := CompareOf(keyID);
          kBalance := k;
      DoInsert(root);
        END (*--with*);
  END (*--if*);
END Insert;
(*-------------------------*)

(*
Remove searches theTree for the nodes with theKey and deletes the
node from the key. The below algorithm is derived from that given
by Gonnet [2] for deletions for weight balanced and path balanced
trees. The search for the key is a recursive inorder tree traversal.
If the node being deleted has one null descendant deletion is simply
accomplished by replacing it with the other descendant. Otherwise,
the node must be moved down the tree until it has a non-null
descendant. The strategy employed here, which is better suited for
balanced trees, is to use rotations to gradually move the node
towards the fringe (e.g. leaves) of the tree. The disadvantage of
k-balanced trees when compared to AVL trees is that rotations could
occur all the way back to the root of the tree.
*)

PROCEDURE Remove    (    theTree  : Tree         (*-- inout *);
                         theKey   : Key          (*-- in    *);
                                                 notFound : NotFoundProc (*-- in    *));

VAR   compare  : CompareProc; (*-- key comparison routine *)
          freeKey  : DisposeProc; (*-- key disposal routine *)
          freeData : DisposeProc; (*-- data disposal routine *)

  PROCEDURE DoRemove (VAR subTree : Link (*-- inout *));

  VAR   oldTree : Link; (*-- link to subtree to dispose *)

  BEGIN
    IF (subTree = NIL) THEN

          (*-- ERROR key not found in the tree *)

          notFound(theKey);

        ELSE
          CASE compare(theKey, subTree^.key) OF
            less    : DoRemove(subTree^.left);
          | greater : DoRemove(subTree^.right);
          ELSE

                (*-- key found, delete it *)

                IF (subTree^.right = NIL) THEN
                  oldTree := subTree;
                  subTree := subTree^.left;
                  freeKey(oldTree^.key);
                  freeData(oldTree^.data);
                  Deallocate(oldTree, SIZE(oldTree^));
                ELSIF (subTree^.left = NIL) THEN
                  oldTree := subTree;
                  subTree := subTree^.right;
                  freeKey(oldTree^.key);
                  freeData(oldTree^.data);
                  Deallocate(oldTree, SIZE(oldTree^));

                (*-- no descendant is null, rotate on heavier side --*)

                ELSIF Wt(subTree^.left) > Wt(subTree^.right) THEN

                  (*-- left side is heavier, do a right rotation --*)

                  IF VAL(INTEGER, Wt(subTree^.left^.right)) -
                         VAL(INTEGER, Wt(subTree^.left^.left)) >=
                         VAL(INTEGER, theTree^.k) THEN
                    LeftRotation(subTree^.left);
                  END (*--if*);
                  RightRotation(subTree);
                  DoRemove(subTree^.right);
                ELSE

                  (*-- right side is heavier, do a left rotation --*)

                  IF VAL(INTEGER, Wt(subTree^.right^.left)) -
                         VAL(INTEGER, Wt(subTree^.right^.right)) >=
                         VAL(INTEGER, theTree^.k) THEN
                    RightRotation(subTree^.right);
                  END (*--if*);
                  LeftRotation(subTree);
                  DoRemove(subTree^.left);
                END (*--if*);
          END (*--case*);

          (*-- reconstruct weight information --*)

          IF (subTree # NIL) THEN
                WITH subTree^ DO
                  weight := Wt(left) + Wt(right);
                END (*--with*);
          END (*--if*);
        END (*--if*);
  END DoRemove;

BEGIN
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
8.2.5 Selectors

IsDefined verifies to the best of its ability whether theTree has been
created and is still an active object. Complexity: O(1).
*)

PROCEDURE IsDefined (    theTree  : Tree         (*-- in    *))
                                  : BOOLEAN      (*-- out   *);
BEGIN
  RETURN theTree # NullTree;
END IsDefined;
(*-------------------------*)

(*
IsEmpty returns True if theTree is in the empty state, as indicated by
the root being NIL, and False otherwise.  As per the specification
(section 3.3) undefined trees are considered empty. Complexity: O(1).
*)

PROCEDURE IsEmpty   (    theTree  : Tree         (*-- in    *))
                                  : BOOLEAN      (*-- out   *);
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

PROCEDURE IsEqual   (    left     : Tree         (*-- in    *);
                         right    : Tree         (*-- in    *))
                                  : BOOLEAN      (*-- out   *);

VAR       compare : CompareProc;        (*-- item comparison routine *)

  PROCEDURE DoIsEqual (    leftSubtree  : Link    (*-- in    *);
                                                   rightSubtree : Link    (*-- in    *))
                                                                                : BOOLEAN (*-- out   *);
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

PROCEDURE KeyTypeOf (    theTree  : Tree         (*-- in    *))
                                  : TypeID       (*-- out   *);
BEGIN
  treeError := noerr;
  IF (theTree # NIL) THEN
    RETURN theTree^.keyID;
  END (*--if*);
  RaiseErrIn(typeof, undefined);
  RETURN NullType;
END KeyTypeOf;
(*-------------------------*)

PROCEDURE DataTypeOf (    theTree  : Tree         (*-- in    *))
                                   : TypeID       (*-- out   *);
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

PROCEDURE ExtentOf   (    theTree  : Tree         (*-- in    *))
                                   : CARDINAL     (*-- out   *);

VAR   count : CARDINAL; (*-- running count of nodes in tree *)

  PROCEDURE CountNodes (    theSubtree : Link (*-- in    *));
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
examination of each node's key. As noted by Wirth [4] and others, as
few as log2 n comparisons may be needed to find theKey if theTree is
perfectly balanced. The algorithmic complexity of the search is
therefore O(log2 n). It is assumed that all keys are comparable and
the compare procedure is not NIL.
*)

PROCEDURE IsPresent (    theTree  : Tree         (*-- in    *);
                         theKey   : Key          (*-- in    *);
                                                 found    : FoundProc    (*-- in    *);
                                                 notFound : NotFoundProc (*-- in    *));

VAR   treeIndex : Link;
      compare   : CompareProc; (*-- key comparison routine *)

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
8.2.6 Passive Iterators

Each of the three iterator routines accomplish recursively Preorder,
Inorder, and Postorder traversals of the given tree. If the tree is
not defined, the undefined exception is raised and the traversal is
aborted. Otherwise, traversal begins with the root of the tree following
the specifications given in section 3.1.6.2. The complexity is O(n) for
all three traversals. Once again these are elementary tree algorithms that
can be found is any college textbook on data structures.
*)

PROCEDURE Preorder  (    theTree   : Tree       (*-- in    *);
                         theProcess: AccessProc (*-- in    *));

  PROCEDURE DoPreorder (    theSubtree : Link (*-- in    *));
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

PROCEDURE Inorder   (    theTree   : Tree       (*-- in    *);
                         theProcess: AccessProc (*-- in    *));

  PROCEDURE DoInorder (    theSubtree : Link (*-- in    *));
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

PROCEDURE Postorder (    theTree   : Tree       (*-- in    *);
                         theProcess: AccessProc (*-- in    *));

  PROCEDURE DoPostorder (    theSubtree : Link (*-- in    *));
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
8.2.7 Active Iterators

The active iterators given below simply return eomponents of tree nodes
and are thus, for the most part, self-explanatory.

The TML Modula-2 compiler prohibits us from redeclaring an opaque
type as equal to another type. (We cannot define a NodePtr = Link).
Thus, we must explicitly define NodePtr and then use the type transfer
facility provided by VAL to coerce the tree links into iterator nodes.
*)

TYPE  NodePtr = POINTER TO Node;

PROCEDURE RootOf   (    theTree : Tree    (*-- in    *))
                                                            : NodePtr (*-- out   *);
BEGIN
  IF (theTree = NIL) THEN
    RETURN NullNode;
  END (*--if*);
  RETURN NodePtr(theTree^.root);
END RootOf;
(*-------------------------*)

PROCEDURE LeftOf   (    theNode : NodePtr (*-- in    *))
                                                            : NodePtr (*-- out   *);
BEGIN
  IF (theNode = NIL) THEN
    RETURN NullNode;
  END (*--if*);
  RETURN NodePtr(theNode^.left);
END LeftOf;
(*-------------------------*)

PROCEDURE RightOf  (    theNode : NodePtr (*-- in    *))
                                                            : NodePtr (*-- out   *);
BEGIN
  IF (theNode = NIL) THEN
    RETURN NullNode;
  END (*--if*);
  RETURN NodePtr(theNode^.right);
END RightOf;
(*-------------------------*)

PROCEDURE IsNull   (    theNode : NodePtr (*-- in    *))
                                                            : BOOLEAN (*-- out   *);
BEGIN
  RETURN theNode = NIL;
END IsNull;
(*-------------------------*)

PROCEDURE KeyOf    (    theNode : NodePtr (*-- in    *))
                                                            : Key     (*-- out   *);
BEGIN
  IF (theNode = NIL) THEN
    RETURN NullItem;
  END (*--if*);
  RETURN theNode^.key;
END KeyOf;
(*-------------------------*)

PROCEDURE DataOf   (    theNode : NodePtr (*-- in    *))
                                                            : Data    (*-- out   *);
BEGIN
  IF (theNode = NIL) THEN
    RETURN NullItem;
  END (*--if*);
  RETURN theNode^.data;
END DataOf;
(*-------------------------*)

PROCEDURE WeightOf  (    theNode : NodePtr  (*-- in    *))
                                                             : Weight   (*-- out   *);
BEGIN
  IF (theNode = NIL) THEN
    RETURN 1;
  END (*--if*);
  RETURN theNode^.weight;
END WeightOf;
(*-------------------------*)


(*
8.2.8 Module Initialization

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
END IPBSUMI.

(*
References


[1]     G.H. Gonnet, Balancing Binary Trees by Internal Path Reduction,
        Communications of the ACM, Vol. 26 (12), (Dec. 1983), pp. 1074-1081.
[2]     G.H. Gonnet, Handbook of Algorithms and Data Structures, Addison-Wesley,
        Reading, MA 1984.
[3] R.S. Wiener and R.F. Sincovec, Data Structures Using Modula-2,
    John Wiley & Sons, New York, NY 1986.
[4]     N. Wirth, Algorithms and Data Structures, Prentice-Hall, Englewood Cliffs,
        NJ 1986.
*)
