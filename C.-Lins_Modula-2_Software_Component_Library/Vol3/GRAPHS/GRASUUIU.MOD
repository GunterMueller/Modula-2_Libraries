IMPLEMENTATION MODULE GraSUUIUtil;
(*==============================================================
    Version  : V2.01  08 December 1989.
    Compiler : JPI TopSpeed Modula-2
    Code size: 1410 bytes
    Component: GraphSBMI Utilities

    REVISION HISTORY
    v1.00  27 Jan 1989  C. Lins:
        Initial implementation derived from GraphSUMIUtil.
    v1.01  14 Mar 1989  C. Lins:
        Changed name of HasPath to IsReachable.
    v2.00  24 Oct 1989  C. Lins
        Created generic pc version
    v2.01  08 Dec 1989  I.S.C. Houston.
          Adapted to JPI Compiler:
          Used type transfer functions instead of VAL.
          Used shortened library module names for DOS and OS/2.

        (C) Copyright 1989 Charles A. Lins
==============================================================*)

FROM GrafSUUI IMPORT
        (*--cons*) NullGraph, NullVertex, NullEdge,
        (*--type*) Graph, Vertex, Edge, VertexLoopProc,
        (*--proc*) FirstVertex, NextVertex, FirstEdge, NextEdge,
                           DegreeOf, IncidentOn, TravVertices, FirstOf,
                           SecondOf;

FROM TypeManager IMPORT
    (*--Proc*) Create, LongCardTypeID;

FROM Items IMPORT
    (*--Type*) Item;

IMPORT SetSUUN;
IMPORT QSUUN;


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


PROCEDURE IsIsolated   (    theVertex : Vertex  (*--in   *))
                                                              : BOOLEAN (*--out  *);
BEGIN
  RETURN (DegreeOf(theVertex) = 0);
END IsIsolated;
(*-------------------------*)


PROCEDURE HasSelfLoops (    theVertex : Vertex  (*--in   *))
                                                              : BOOLEAN (*--out  *);

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


PROCEDURE IsReachable (    fromVertex: Vertex  (*--in   *);
                                                   toVertex  : Vertex  (*--in   *))
                                                                         : BOOLEAN (*--out  *);

VAR   visited  : SetSUUN.Set; (*-- set of vertices already visited *)
          pathFound: BOOLEAN;     (*-- true when path between vertices is found *)

  PROCEDURE Visit (v : Vertex);

  VAR   e   : Edge;   (*-- loop index over edges from v *)
  VAR   ep1 : Vertex; (*-- 1st vertex of an edge *)
  VAR   ep2 : Vertex; (*-- 2nd vertex of an edge *)

  BEGIN
    pathFound := (v = toVertex);
        IF ~pathFound THEN
          (*-- add v to the set of vertices already visited *)
          SetSUUN.Include(Item(v), visited);

          e := FirstEdge(v);
          WHILE (e # NullEdge) & ~pathFound DO
            IncidentOn(e, ep1, ep2);
                IF (ep2 = v) THEN
                  ep2 := ep1;
                END (*--if*);
            IF ~SetSUUN.IsAMember(Item(ep2), visited) THEN
              Visit(ep2);
            END (*--if*);
            e := NextEdge(e);
          END (*--while*);
        END (*--if*);
  END Visit;

BEGIN
  visited := SetSUUN.Create(LongCardTypeID());
  Visit(fromVertex);
  SetSUUN.Destroy(visited);
  RETURN pathFound;
END IsReachable;
(*-------------------------*)


PROCEDURE DFS   (    theGraph   : Graph          (*--in   *);
                                         process    : VertexLoopProc (*--in   *));

VAR   v : Vertex; (*-- loop index over vertices *)
          continue : BOOLEAN;     (*-- controls termination of DFS *)
          visited  : SetSUUN.Set; (*-- set of vertices already visited *)

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
          SetSUUN.Include(Item(v), visited);

          e := FirstEdge(v);
          WHILE (e # NullEdge) & continue DO
            IncidentOn(e, v1, v2);
                IF (v1 = v) THEN
                  w := v2;
                ELSE
                  w := v1;
                END (*--if*);
            IF ~SetSUUN.IsAMember(Item(w), visited) THEN
              Visit(w);
            END (*--if*);
            e := NextEdge(e);
          END (*--while*);
        END (*--if*);
  END Visit;

BEGIN
  continue := TRUE;
  visited := SetSUUN.Create(LongCardTypeID());
  v := FirstVertex(theGraph);
  LOOP
    IF (v = NullVertex) THEN
          EXIT (*--loop*);
        END (*--if*);

    IF ~SetSUUN.IsAMember(Item(v), visited) THEN
          Visit(v);
          IF ~continue THEN
                EXIT (*--loop*);
          END (*--if*);
        END (*--if*);
        v := NextVertex(v);
  END (*--loop*);
  SetSUUN.Destroy(visited);
END DFS;
(*-------------------------*)

PROCEDURE BFS   (    theGraph   : Graph          (*--in   *);
                                         process    : VertexLoopProc (*--in   *));

VAR   u, v, w  : Vertex;      (*-- loop indexes over vertices *)
          e        : Edge;                (*-- loop index over edges *)
          continue : BOOLEAN;     (*-- controls termination of DFS *)
          visited  : SetSUUN.Set; (*-- set of vertices already visited *)
          toVisit  : QSUUN.Queue; (*-- vertices waiting to be visited *)

BEGIN
  continue := TRUE;
  visited  := SetSUUN.Create(LongCardTypeID());
  toVisit  := QSUUN.Create();
  u := FirstVertex(theGraph);

  WHILE continue & (u # NullVertex) DO
        IF ~SetSUUN.IsAMember(Item(u), visited) THEN
          SetSUUN.Include(Item(u), visited);
          QSUUN.Arrive(toVisit, Item(u));

          WHILE continue & ~QSUUN.IsEmpty(toVisit) DO
        v := Vertex(QSUUN.FrontOf(toVisit));
                QSUUN.Depart(toVisit);

                continue := process(v);

                e := FirstEdge(v);
                WHILE continue & (e # NullEdge) DO
                  w := FirstOf(e);
                  IF w = v THEN
                        w := SecondOf(e);
                  END (*--if*);
                  IF ~SetSUUN.IsAMember(Item(w), visited) THEN
                QSUUN.Arrive(toVisit, Item(w));
                        SetSUUN.Include(Item(w), visited);
                  END (*--if*);
                  e := NextEdge(e);
                END (*--while*);
          END (*--while*);
        END (*--if*);
        u := NextVertex(u);
  END (*--while*);

  SetSUUN.Destroy(visited);
  QSUUN.Destroy(toVisit);
END BFS;
(*-------------------------*)

END GraSUUIUtil.
