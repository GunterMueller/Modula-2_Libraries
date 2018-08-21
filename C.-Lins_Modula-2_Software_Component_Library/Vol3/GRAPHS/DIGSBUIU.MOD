IMPLEMENTATION MODULE DigSBUIUtil;
(*====================================================================
    Version  : V2.01  08 December 1989.
    Compiler : JPI TopSpeed Modula-2
    Code size: 1474 bytes
    Component: DigraphSBUI Utilities

    INTRODUCTION
    This module provides the implementation of utility operations
        supporting the digraph SBUI abstract data type.

    REVISION HISTORY
    v1.00  24 May 1988  C. Lins:
        Initial TML Modula-2 implementation
    v1.01  26 Jan 1989  C. Lins:
        Revised to conform with DigraphSBMI implementation.
    v1.03  14 Mar 1989  C. Lins:
          Changed name of HasPath to IsReachable.
    v2.00  24 Oct 1989  C. Lins
          Created generic pc version
    v2.01  08 Dec 1989  I.S.C. Houston.
          Adapted to JPI Compiler:
          Used type transfer functions instead of VAL.
          Used shortened library module names for DOS and OS/2.

        (C) Copyright 1989 Charles A. Lins
====================================================================*)

FROM DigrSBUI IMPORT
        (*--cons*) NullGraph, NullEdge, NullVertex,
        (*--type*) Graph, Vertex, Edge, VertexLoopProc,
        (*--proc*) FirstVertex, NextVertex, FirstEdge, NextEdge,
                           InDegree, OutDegree, FinalOf, TravVertices;

FROM TypeManager IMPORT
    (*--Proc*) Create, LongCardTypeID;

FROM Items IMPORT
    (*--Type*) Item;

IMPORT SetSUUN;
IMPORT QSUUN;

(*
13.5.1  Graph Utility Operations
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
13.5.2  Vertex Utility Operations
*)

PROCEDURE IsTerminal   (    theVertex : Vertex  (*-- in    *))
                                                              : BOOLEAN (*-- out   *);
BEGIN
  RETURN FirstEdge(theVertex) = NullEdge;
END IsTerminal;
(*-------------------------*)

PROCEDURE IsIsolated   (    theVertex : Vertex  (*-- in    *))
                                                              : BOOLEAN (*-- out   *);
BEGIN
  RETURN (FirstEdge(theVertex) = NullEdge) & (InDegree(theVertex) = 0);
END IsIsolated;
(*-------------------------*)

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

PROCEDURE IsReachable (    fromVertex: Vertex  (*--in   *);
                                                   toVertex  : Vertex  (*--in   *))
                                                                         : BOOLEAN (*--out  *);

VAR   visited  : SetSUUN.Set; (*-- set of vertices already visited *)
          pathFound: BOOLEAN;     (*-- true when path between vertices is found *)

  PROCEDURE Visit (v : Vertex);

  VAR   e : Edge;   (*-- loop index over edges from v *)
  VAR   w : Vertex; (*-- destination vertex of an edge *)

  BEGIN
    pathFound := (v = toVertex);
        IF ~pathFound THEN
          (*-- add v to the set of vertices already visited *)
          SetSUUN.Include(Item(v), visited);

          e := FirstEdge(v);
          WHILE (e # NullEdge) & ~pathFound DO
            w := FinalOf(e);
            IF ~SetSUUN.IsAMember(Item(w), visited) THEN
              Visit(w);
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


(*
11.5.3  Graph Traversal Utilities
*)

PROCEDURE DFS   (    theGraph   : Graph          (*--in   *);
                                         process    : VertexLoopProc (*--in   *));

VAR   v : Vertex; (*-- loop index over vertices *)
          continue : BOOLEAN;     (*-- controls termination of DFS *)
          visited  : SetSUUN.Set; (*-- set of vertices already visited *)

  PROCEDURE Visit (v : Vertex);

  VAR   e : Edge;   (*-- loop index over edges from v *)
  VAR   w : Vertex; (*-- destination vertex of an edge *)

  BEGIN
    continue := process(v);
        IF continue THEN
          (*-- add v to the set of vertices already visited *)
          SetSUUN.Include(Item(v), visited);

          e := FirstEdge(v);
          WHILE (e # NullEdge) & continue DO
            w := FinalOf(e);
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
                  w := FinalOf(e);
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

END DigSBUIUtil.
