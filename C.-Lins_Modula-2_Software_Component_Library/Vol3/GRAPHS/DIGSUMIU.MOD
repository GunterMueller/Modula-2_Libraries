(*
10.5    DigraphSUMI Utilities Implementation

This module provides the implementation of utility operations
supporting the unbounded form of the digraph abstract data type. Of
course, these routines could have been included in the exported operations
of the digraph module itself. The reason we have not done so it that none
of the operations in this module are primitive, i.e., require the internal
representation of the digraph in order to accomplish their function.
*)

IMPLEMENTATION MODULE DigSUMIU;
(*==============================================================
    Version  : V2.01  08 December 1989.
    Compiler : JPI TopSpeed Modula-2
    Code size: 1481 bytes
    Component: DigraphSUMI Utilities

    REVISION HISTORY
    v1.01  09 Oct 1988  C. Lins:
        Initial TML Modula-2 implementation
    v1.02  10-13 Jan 1989  C. Lins:
        Removed Graph parameter from vertex selectors.
        Added depth-first search, breadth-first search, and
        HasPath operations.
    v2.00  24 Oct 1989  C. Lins
          Created generic pc version
    v2.01  08 Dec 1989  I.S.C. Houston.
          Adapted to JPI Compiler:
          Used type transfer functions instead of VAL.
          Used shortened library module names for DOS and OS/2.

        (C) Copyright 1989 Charles A. Lins
==============================================================*)

FROM DigrSUMI IMPORT
        (*--cons*) NullGraph, NullEdge, NullVertex,
        (*--type*) Graph, Vertex, Edge, VertexLoopProc,
        (*--proc*) FirstVertex, NextVertex, FirstEdge, NextEdge,
                   InDegree, OutDegree, FinalOf, TravVertices;

FROM TypeManager IMPORT
    (*--Proc*) Create, LongCardTypeID;

FROM Items IMPORT
    (*--Type*) Item;

IMPORT SetSUMN;
IMPORT QSUMN;

(*
10.5.1  Graph Selector Utilities

To find the minimum or maximum degree of a graph we must traverse the vertices of
the given graph. Whenever a vertex is found having a degree smaller than the current
minimum (or larger than the current maximum) we update the current value of the minimum
(or maximum). For this algorithm to work correctly, we must initialize the current
minimum to the largest possible minimum value (conversely, the maximum must be initialized
to the smallest possible maximum). Since every vertex must be examined, the algorithmic
complexity of both MinDegree and MaxDegree is O(|V|).
*)

VAR   minimum : CARDINAL; (*-- current minimum degree *)

PROCEDURE FindMinDegree (    theVertex : Vertex (*--in   *));

VAR   degree : CARDINAL; (*-- degree of the current vertex *)

BEGIN
  degree := InDegree(theVertex) + OutDegree(theVertex);
  IF (degree < minimum) THEN
        minimum := degree;
  END (*--if*);
END FindMinDegree;
(*-------------------------*)

PROCEDURE MinDegree (    theGraph  : Graph       (*-- in    *))
                                                                   : CARDINAL    (*-- out   *);
BEGIN
  IF (theGraph = NullGraph) THEN
        RETURN 0;
  ELSE
        minimum := MAX(CARDINAL);
        TravVertices(theGraph, FindMinDegree);
        RETURN minimum;
  END (*--if*);
END MinDegree;
(*-------------------------*)


VAR   maximum : CARDINAL; (*-- current minimum degree *)

PROCEDURE FindMaxDegree (    theVertex : Vertex (*--in   *));

VAR   degree : CARDINAL; (*-- degree of the current vertex *)

BEGIN
  degree := InDegree(theVertex) + OutDegree(theVertex);
  IF (degree > maximum) THEN
        maximum := degree;
  END (*--if*);
END FindMaxDegree;
(*-------------------------*)

PROCEDURE MaxDegree (    theGraph  : Graph       (*-- in    *))
                                                                   : CARDINAL    (*-- out   *);
BEGIN
  maximum := MIN(CARDINAL);
  IF (theGraph # NullGraph) THEN
        TravVertices(theGraph, FindMaxDegree);
  END (*--if*);
  RETURN maximum;
END MaxDegree;
(*-------------------------*)

(*
A graph is called loop-free if every vertex of the graph contains no edges to itself.
Thus, we can iterate over the vertices returning false as soon as a vertex is found
to contain a loop. Otherwise, if all vertices have been examined without exiting we
know that there are no loops in the graph. Since the selector HasSelfLoops uses the
graph object in its call we choose to use an active iteration over the vertices
instead of the passive iterator TravVertices. An undefined graph is considered
loop-free. Complexity O(v+e).
*)

PROCEDURE IsLoopFree (    theGraph  : Graph       (*--in   *))
                                                                    : BOOLEAN     (*--out  *);

VAR   theVertex : Vertex;

BEGIN
  IF (theGraph # NullGraph) THEN
        theVertex := FirstVertex(theGraph);
        WHILE (theVertex # NullVertex) DO
          IF HasSelfLoops(theVertex) THEN
            RETURN FALSE;
          END (*--if*);
          theVertex := NextVertex(theVertex);
        END (*--while*);
  END (*--if*);
  RETURN TRUE;
END IsLoopFree;
(*-------------------------*)


(*
10.5.2  Vertex Selector Utilities

IsTerminal simply needs to determine if there are any directed edges leaving the
given vertex. Thus we can use the active iterator FirstEdge to retrieve the first
edge leaving the given vertex. If there is no first edge we know that the vertex
is terminal. Complexity O(1).
*)

PROCEDURE IsTerminal   (    theVertex : Vertex  (*-- in    *))
                                                              : BOOLEAN (*-- out   *);
BEGIN
  RETURN FirstEdge(theVertex) = NullEdge;
END IsTerminal;
(*-------------------------*)


(*
An isolated vertex not only has no edges leaving the vertex but no edge is
incident to the vertex. We can determine whether a vertex is isolated by
testing the indegree of the vertex being zero and the vertex having no edges
leaving it as we did above for IsTerminal. Complexity O(1).
*)

PROCEDURE IsIsolated   (    theVertex : Vertex  (*-- in    *))
                                                              : BOOLEAN (*-- out   *);
BEGIN
  RETURN (FirstEdge(theVertex) = NullEdge) & (InDegree(theVertex) = 0);
END IsIsolated;
(*-------------------------*)


(*
A vertex has a self-loop is at least one edge leaving the vertex has that
vertex as its destination. Thus we iterate over the edges seeking an edge
that meets this criteria. As soon as such an edge is found we return True.
If all edges leaving the vertex are examined without the routine exiting we
know that the vertex has no self-loops. Complexity O(outdegree(v)).
*)

PROCEDURE HasSelfLoops (    theVertex : Vertex  (*-- in    *))
                                                              : BOOLEAN (*-- out   *);

VAR   theEdge : Edge;   (*-- loop pointer over edges of this vertex *)

BEGIN
  theEdge := FirstEdge(theVertex);
  WHILE (theEdge # NullEdge) DO
        IF (FinalOf(theEdge) = theVertex) THEN
          RETURN TRUE;
        END (*--if*);
        theEdge := NextEdge(theEdge);
  END (*--while*);
  RETURN FALSE;
END HasSelfLoops;
(*-------------------------*)

(*
IsReachable uses a variation on the depth-first search given below. We create a
set of vertices that have been visited and each time a vertex is processed
it is added to the set. This way each vertex is examined only once. The traversal
stops once the algorithm determines that there is a path between the fromVertex
and the toVertex. Otherwise, the traversal completes without finding a path and
FALSE is returned to the caller.
*)

PROCEDURE IsReachable    (    fromVertex: Vertex      (*--in   *);
                                                          toVertex  : Vertex      (*--in   *))
                                                                        : BOOLEAN     (*--out  *);

VAR   visited  : SetSUMN.Set; (*-- set of vertices already visited *)
          pathFound: BOOLEAN;     (*-- true when path between vertices is found *)

  PROCEDURE Visit (v : Vertex);

  VAR   e : Edge;   (*-- loop index over edges from v *)
  VAR   w : Vertex; (*-- destination vertex of an edge *)

  BEGIN
    pathFound := (v = toVertex);
        IF ~pathFound THEN
          (*-- add v to the set of vertices already visited *)
          SetSUMN.Include(Item(v), visited);

          e := FirstEdge(v);
          WHILE (e # NullEdge) & ~pathFound DO
            w := FinalOf(e);
            IF ~SetSUMN.IsAMember(Item(w), visited) THEN
              Visit(w);
            END (*--if*);
            e := NextEdge(e);
          END (*--while*);
        END (*--if*);
  END Visit;

BEGIN
  visited := SetSUMN.Create(LongCardTypeID());
  Visit(fromVertex);
  SetSUMN.Destroy(visited);
  RETURN pathFound;
END IsReachable;
(*-------------------------*)


(*
10.5.3  Graph Traversal Utilities

The two algorithms below implement the standard depth-first and breadth-first
search for a directed graph as given by Sedgewick [] and Mehlhorn []. Both
algorithms stop whenever the VertexLoopProc returns FALSE.
*)

PROCEDURE DFS   (    theGraph   : Graph          (*--in   *);
                                         process    : VertexLoopProc (*--in   *));

VAR   v : Vertex; (*-- loop index over vertices *)
          continue : BOOLEAN;     (*-- controls termination of DFS *)
          visited  : SetSUMN.Set; (*-- set of vertices already visited *)

  PROCEDURE Visit (v : Vertex);

  VAR   e : Edge;   (*-- loop index over edges from v *)
  VAR   w : Vertex; (*-- destination vertex of an edge *)

  BEGIN
    continue := process(v);
        IF continue THEN
          (*-- add v to the set of vertices already visited *)
          SetSUMN.Include(Item(v), visited);

          e := FirstEdge(v);
          WHILE (e # NullEdge) & continue DO
            w := FinalOf(e);
            IF ~SetSUMN.IsAMember(Item(w), visited) THEN
              Visit(w);
            END (*--if*);
            e := NextEdge(e);
          END (*--while*);
        END (*--if*);
  END Visit;

BEGIN
  continue := TRUE;
  visited := SetSUMN.Create(LongCardTypeID());
  v := FirstVertex(theGraph);
  LOOP
    IF (v = NullVertex) THEN
          EXIT (*--loop*);
        END (*--if*);

    IF ~SetSUMN.IsAMember(Item(v), visited) THEN
          Visit(v);
          IF ~continue THEN
                EXIT (*--loop*);
          END (*--if*);
        END (*--if*);
        v := NextVertex(v);
  END (*--loop*);
  SetSUMN.Destroy(visited);
END DFS;
(*-------------------------*)

PROCEDURE BFS   (    theGraph   : Graph          (*--in   *);
                                         process    : VertexLoopProc (*--in   *));

VAR   u, v, w  : Vertex;      (*-- loop indexes over vertices *)
          e        : Edge;                (*-- loop index over edges *)
          continue : BOOLEAN;     (*-- controls termination of DFS *)
          visited  : SetSUMN.Set; (*-- set of vertices already visited *)
          toVisit  : QSUMN.Queue; (*-- vertices waiting to be visited *)

BEGIN
  continue := TRUE;
  visited  := SetSUMN.Create(LongCardTypeID());
  toVisit  := QSUMN.Create(LongCardTypeID());
  u := FirstVertex(theGraph);

  WHILE continue & (u # NullVertex) DO
        IF ~SetSUMN.IsAMember(Item(u), visited) THEN
          SetSUMN.Include(Item(u), visited);
          QSUMN.Arrive(toVisit, Item(u));

          WHILE continue & ~QSUMN.IsEmpty(toVisit) DO
        v := Vertex(QSUMN.FrontOf(toVisit));
                QSUMN.Depart(toVisit);

                continue := process(v);

                e := FirstEdge(v);
                WHILE continue & (e # NullEdge) DO
                  w := FinalOf(e);
                  IF ~SetSUMN.IsAMember(Item(w), visited) THEN
                QSUMN.Arrive(toVisit, Item(w));
                        SetSUMN.Include(Item(w), visited);
                  END (*--if*);
                  e := NextEdge(e);
                END (*--while*);
          END (*--while*);
        END (*--if*);
        u := NextVertex(u);
  END (*--while*);

  SetSUMN.Destroy(visited);
  QSUMN.Destroy(toVisit);
END BFS;
(*-------------------------*)

END DigSUMIU.
