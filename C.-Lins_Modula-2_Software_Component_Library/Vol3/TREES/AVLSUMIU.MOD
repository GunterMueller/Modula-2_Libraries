(*
6.4 Balanced Binary Search Tree Utilities

Below is the implementation for the print tree routine whose
interface was given above.
*)

IMPLEMENTATION MODULE AVLSUMIU;
(*==============================================================
    Version  : V2.01  08 December 1989.
    Compiler : JPI TopSpeed Modula-2
    Component: AVL Tree SUMI Utilities
    Code size: OBJ file is 1613 bytes

    REVISION HISTORY
    v1.00  17 Mar 1988  C. Lins
          Initial implementation for TML Modula-2.
    v1.01  01 Oct 1988  C. Lins
          Cleanup of comments.
          Changed PrintTree to use a single procedure parameter.
          Added HeightOf selector.
    v2.00  08 Oct 1989  C. Lins
          Created generic pc version
    v2.01  08 Dec 1989  I.S.C. Houston.
          Adapted to JPI Compiler:
          Use of type transfer functions instead of VAL.
          Use of shortened library module names.

        (C) Copyright 1989 Charles A. Lins
==============================================================*)

FROM Items IMPORT
    (*--Type*) Item;

FROM AVLSUMI IMPORT
        (*--Type*) Tree, NodePtr, Balance,
        (*--Proc*) RootOf, LeftOf, RightOf, IsNull, KeyOf, DataOf,
                           BalanceOf, IsEmpty;

        (*-----------------------*)

(*
6.4.1 Utility Selectors

HeightOf returns the height of the given tree. Height may be computed by
subtracting the level of the ðlowestð node in the tree from the level of
the root.  Complexity O(log2 n).
*)

PROCEDURE HeightOf (    theTree : Tree     (*--in   *))
                                                                : CARDINAL (*--out  *);

VAR   maxLevel : CARDINAL; (*-- level of the lowest node so far *)

  PROCEDURE CountLevels (    theNode : NodePtr  (*--in   *);
                                                         theLevel: CARDINAL (*--in   *));
  BEGIN
    IF ~IsNull(theNode) THEN
          IF (theLevel > maxLevel) THEN
            maxLevel := theLevel;
          END (*--if*);
          CountLevels(LeftOf(theNode), theLevel+1);
          CountLevels(RightOf(theNode), theLevel+1);
        END (*--if*);
  END CountLevels;

BEGIN
  maxLevel := 1;
  IF ~IsEmpty(theTree) THEN
    CountLevels(RootOf(theTree), 1);
  END (*--if*);
  RETURN maxLevel - 1;
END HeightOf;
(*-------------------------*)

(*
6.4.2 Debugging Iterators

PrintTree iterates over the given tree such that the nodes may be
printed. Trees are normally displayed with the root at the top and
the leaves at the bottom. To simplify the printing process, PrintTree
displays the tree rotated 90’ to the left. Thus the root is shown at
the left of the page/screen with the leaves at the right. Furthermore,
the left branches are shown towards the bottom of the display and the
right branches at the top. A constant indentation of two spaces between
levels is used.

The algorithm used here is a variation on the inorder tree traversal. So
that the tree is displayed properly rotated, the processing of the left
and right branches are reversed. This algorithm is derived from that
given by Wirth in [8].
*)

PROCEDURE PrintTree (    theTree: Tree       (*--in   *);
                         print  : PrintProc  (*--in   *));

  PROCEDURE DoPrintTree (    theSubtree : NodePtr  (*--in   *);
                                                         theLevel   : CARDINAL (*--in   *));
  BEGIN
    IF ~IsNull(theSubtree) THEN
          DoPrintTree(RightOf(theSubtree), theLevel+1);
          print(theLevel, KeyOf(theSubtree),
                        DataOf(theSubtree), BalanceOf(theSubtree));
          DoPrintTree(LeftOf(theSubtree), theLevel+1);
        END (*--if*);
  END DoPrintTree;

BEGIN
  IF ~IsEmpty(theTree) THEN
    DoPrintTree(RootOf(theTree), 0);
  END (*--if*);
END PrintTree;
(*-------------------------*)

END AVLSUMIU.
