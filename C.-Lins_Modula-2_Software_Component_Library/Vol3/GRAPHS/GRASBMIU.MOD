(*
14.4    GraphSBMI Utilities Implementation
*)

IMPLEMENTATION MODULE GraSBMIUtil;
(*=============================================================
    Version  : V2.01  08 December 1989.
    Compiler : JPI TopSpeed Modula-2
    Code size: 1419 bytes
    Component: GraphSBMI Utilities

    REVISION HISTORY
    v1.01  08 Oct 1988  C. Lins:
        Initial TML Modula-2 implementation
    v1.02  17 Feb 1989  C. Lins:
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
=============================================================*)

FROM GrafSBMI IMPORT
        (*--cons*) NullGraph, NullVertex, NullEdge,
        (*--type*) Graph, Vertex, Edge, VertexLoopProc,
        (*--proc*) FirstVertex, NextVertex, FirstEdge, NextEdge,
                           DegreeOf, IncidentOn, TravVertices, FirstOf,
                           SecondOf;

FROM TypeManager IMPORT
    (*--Proc*) Create, LongCardTypeID;

FROM Items IMPORT
    (*--Type*) Item;

IMPORT SetSUMN;
IMPORT QSUMN;

(*
14.4.1  Graph Utility Operations
*)


VAR   minimum : CARDINAL; (*-- current minimum degree *)

PROCEDURE FindMinDegree (    theVertex : Vertex (*--in   *));

VAR   degree : CARDINAL; (*-- degree of the current vertex *)

BEGIN
  degree := DegreeOf(theVertex);
  IF (degree < minimum) THEN
        minimum := degree;
  END (*--if*);
END FindMinDegree;
(*-------------------------*)

PROCEDURE MinDegree (    theGraph  : Graph       (*--in   *))
                                                                   : CARDINAL    (*--out  *);
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
  degree := DegreeOf(theVertex);
  IF (degree > maximum) THEN
        maximum := degree;
  END (*--if*);
END FindMaxDegree;
(*-------------------------*)

PROCEDURE MaxDegree (    theGraph  : Graph       (*--in   *))
                                                                   : CARDINAL    (*--out  *);
BEGIN
  maximum := MIN(CARDINAL);
  IF (theGraph # NullGraph) THEN
        TravVertices(theGraph, FindMaxDegree);
  END (*--if*);
  RETURN maximum;
END MaxDegree;
(*-------------------------*)

(*
14.4.2  Vertex Utility Operations
*)

PROCEDURE IsIsolated   (    theVertex : Vertex  (*-- in    *))
                                                              : BOOLEAN (*-- out   *);
BEGIN
  RETURN (DegreeOf(theVertex) = 0);
END IsIsolated;
(*-------------------------*)

PROCEDURE HasSelfLoops (    theVertex : Vertex  (*-- in    *))
                                                              : BOOLEAN (*-- out   *);

VAR   e   : Edge;   (*-- loop pointer over edges of this vertex *)
VAR   ep1 : Vertex; (*-- 1st endpoint of the edge *)
VAR   ep2 : Vertex; (*-- 2nd endpoint of the edge *)

BEGIN
  e := FirstEdge(theVertex);
  WHILE (e # NullEdge) DO
    IncidentOn(e, ep1, ep2);
        IF (ep1 = theVertex) & (ep2 = theVertex) THEN
          RETURN TRUE;
        END (*--if*);
        e := NextEdge(e);
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

PROCEDURE IsReachable (    fromVertex: Vertex      (*--in   *);
                                                   toVertex  : Vertex      (*--in   *))
                                                                         : BOOLEAN     (*--out  *);

VAR   visited  : SetSUMN.Set; (*-- set of vertices already visited *)
          pathFound: BOOLEAN;     (*-- true when path between vertices is found *)

  PROCEDURE Visit (v : Vertex);

  VAR   e   : Edge;   (*-- loop index over edges from v *)
  VAR   ep1 : Vertex; (*-- 1st vertex of an edge *)
  VAR   ep2 : Vertex; (*-- 2nd vertex of an edge *)

  BEGIN
    pathFound := (v = toVertex);
        IF ~pathFound THEN
          (*-- add v to the set of vertices already visited *)
          SetSUMN.Include(Item(v), visited);

          e := FirstEdge(v);
          WHILE (e # NullEdge) & ~pathFound DO
            IncidentOn(e, ep1, ep2);
                IF (ep2 = v) THEN
                  ep2 := ep1;
                END (*--if*);
            IF ~SetSUMN.IsAMember(Item(ep2), visited) THEN
              Visit(ep2);
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
12.4.3  Graph Traversal Utilities

The two algorithms below implement the standard depth-first and breadth-first
search for an undirected graph as given by Sedgewick [] and Mehlhorn []. Both
algorithms stop whenever the VertexLoopProc returns FALSE.
*)

PROCEDURE DFS   (    theGraph   : Graph          (*--in   *);
                                         process    : VertexLoopProc (*--in   *));

VAR   v : Vertex; (*-- loop index over vertices *)
          continue : BOOLEAN;     (*-- controls termination of DFS *)
          visited  : SetSUMN.Set; (*-- set of vertices already visited *)

  PROCEDURE Visit (v : Vertex);

  VAR   e : Edge;   (*-- loop index over edges from v *)
  VAR   w : Vertex; (*-- destination vertex of an edge (the vertex incident on e
                                          -- that's not equal to v). *)
  VAR   v1: Vertex; (*-- first vertex incident on e *)
  VAR   v2: Vertex; (*-- second vertex incident on e *)

  BEGIN
    continue := process(v);
        IF continue THEN
          (*-- add v to the set of vertices already visited *)
          SetSUMN.Include(Item(v), visited);

          e := FirstEdge(v);
          WHILE (e # NullEdge) & continue DO
            IncidentOn(e, v1, v2);
                IF (v1 = v) THEN
                  w := v2;
                ELSE
                  w := v1;
                END (*--if*);
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
                  w := FirstOf(e);
                  IF w = v THEN
                        w := SecondOf(e);
                  END (*--if*);
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

END GraSBMIUtil.
