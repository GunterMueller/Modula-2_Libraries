(*
10.5    Digraph - Sequential Unbounded Unmanaged Iterator

See DigraphSUMI for comments. The only difference here is that assignment
of Items uses the Modula-2 assignment statement and Items are never
deallocated.
*)

IMPLEMENTATION MODULE DigrSUUI;
(*====================================================================
    Version  : V2.01  08 December 1989.
    Compiler : JPI TopSpeed Modula-2
    Code size: 5509 bytes
    Component: Digraph - Sequential Unbounded Unmanaged Iterator

    REVISION HISTORY
    v1.00  29 May 1988  C. Lins:
        Initial TML Modula-2 implementation
    v2.00  15 Jan 1989  C. Lins:
        Revised interface in conformance with new graph spec (Ch 9).
                Removed use of TypeID in Create as it's not used by this module.
    v2.01  10 Apr 1989  C. Lins:
        Corrected initialization of handlers array.
    v2.02  18 Apr 1989   C. Lins:
          Added component id constant.
    v2.00  24 Oct 1989  C. Lins
          Created generic pc version
    v2.01  08 Dec 1989  I.S.C. Houston.
          Adapted to JPI Compiler:
          Used type transfer functions instead of VAL.
          Used shortened library module names for DOS and OS/2.

        (C) Copyright 1989 Charles A. Lins
====================================================================*)

FROM JPIStorage IMPORT
        (*--proc*) Allocate, Deallocate;

FROM Items IMPORT
        (*--cons*) NullItem,
        (*--type*) Item;

FROM GraphTypes IMPORT
        (*--type*) Operations, Exceptions, ComponentID;

FROM ErrorHandling IMPORT
        (*--cons*) NullHandler,
        (*--type*) HandlerProc,
        (*--proc*) Raise;

        (*-------------------------*)

(*
10.5.1  Type Declarations
*)

TYPE  Edge   = POINTER TO EdgeNode;
TYPE  Vertex = POINTER TO VertexNode;

TYPE  VertexNode = RECORD
                inGraph : Graph;        (*-- graph in which this vertex is a member *)
                data    : Label;        (*-- data item (label) for this vertex *)
                indegree: CARDINAL; (*-- # of edges ending at this vertex *)
                next    : Vertex;       (*-- next vertex in adjacency list *)
                edges   : Edge;         (*-- link to first edge leaving this vertex *)
          END (*-- VertexNode *);

TYPE  EdgeNode = RECORD
                initial : Vertex;       (*-- source vertex for this edge *)
                final   : Vertex;       (*-- destination vertex for this edge *)
                weight  : Attribute;(*-- weight/attribute for this edge *)
                next    : Edge;         (*-- next edge leaving this vertex *)
          END (*-- EdgeNode *);

TYPE  UnboundedGraph = RECORD
                numVertices: CARDINAL;  (*-- current number of vertices *)
                numEdges   : CARDINAL;  (*-- current number of edges *)
                firstVertex: Vertex;    (*-- first vertex in adjacency list *)
          END (*-- UnboundedGraph *);

TYPE  Graph = POINTER TO UnboundedGraph;

        (*-------------------------*)

(*
10.5.2  Exceptions
*)

VAR   graphError : Exceptions;
VAR   handlers   : ARRAY Exceptions OF HandlerProc;

PROCEDURE GraphError () : Exceptions;
BEGIN
  RETURN graphError;
END GraphError;
(*-------------------------*)

PROCEDURE SetHandler (    theError   : Exceptions  (*-- in    *);
                                                  theHandler : HandlerProc (*-- in    *));
BEGIN
  handlers[theError] := theHandler;
END SetHandler;
(*-------------------------*)

PROCEDURE GetHandler (    theError   : Exceptions  (*-- in    *))
                                                                         : HandlerProc (*-- out   *);
BEGIN
  RETURN handlers[theError];
END GetHandler;
(*-------------------------*)

PROCEDURE RaiseErrIn (    theRoutine : Operations  (*-- in    *);
                                                  theError   : Exceptions  (*-- in    *));
BEGIN
  graphError := theError;
  Raise(ComponentID + ModuleID, theRoutine, theError, handlers[theError]);
END RaiseErrIn;
(*-------------------------*)

(*
10.5.3  Local Routines

ClearEdges removes all edges from a given vertex. O(outdegree(v))
*)

PROCEDURE ClearEdges    (    theVertex: Vertex   (*--inout*));

VAR   theEdge : Edge;   (*-- edge being removed from vertex *)

BEGIN
  WITH theVertex^ DO
        WHILE (edges # NullEdge) DO
          theEdge := edges;
          edges := edges^.next;
          DEC(inGraph^.numEdges);
          Deallocate(theEdge, SIZE(theEdge^));
        END (*--while*);
  END (*--with*);
END ClearEdges;
(*-------------------------*)

PROCEDURE NewVertex (    theGraph   : Graph      (*--in   *);
                                                 theItem    : Label      (*--in   *);
                                                 theRoutine : Operations (*--in   *))
                                                                    : Vertex     (*--out  *);

VAR   theVertex : Vertex;               (*-- newly created vertex *)

BEGIN
  Allocate(theVertex, SIZE(VertexNode));
  IF (theVertex = NullVertex) THEN
        RaiseErrIn(theRoutine, overflow);
  ELSE
        WITH theVertex^ DO
          inGraph := theGraph;
          data    := theItem;
          indegree:= 0;
          edges   := NullEdge;
          next    := NullVertex;
        END (*--with*);
  END (*--if*);
  RETURN theVertex;
END NewVertex;
(*-------------------------*)

PROCEDURE NewEdge (    fromVertex : Vertex     (*--in   *);
                                           toVertex   : Vertex     (*--in   *);
                                           theWeight  : Attribute  (*--in   *);
                                           theRoutine : Operations (*--in   *))
                                                                  : Edge           (*--out  *);

VAR   theEdge : Edge;   (*-- newly created edge *)

BEGIN
  Allocate(theEdge, SIZE(EdgeNode));
  IF (theEdge = NullEdge) THEN
    RaiseErrIn(theRoutine, overflow);
  ELSE
    WITH theEdge^ DO
          initial := fromVertex;
          final   := toVertex;
          weight  := theWeight;
          next    := NullEdge;
    END (*--with*);
  END (*--if*);
  RETURN theEdge;
END NewEdge;
(*-------------------------*)

(*
10.5.4  Graph Constructors
*)

PROCEDURE Create   ()                      : Graph     (*-- out   *);

VAR   newGraph : Graph; (*-- temporary for new graph object *)

BEGIN
  graphError := noerr;
  Allocate(newGraph, SIZE(UnboundedGraph));
  IF (newGraph = NullGraph) THEN
    RaiseErrIn(create, overflow);
  ELSE
    WITH newGraph^ DO
          numVertices := 0;
          numEdges    := 0;
          firstVertex := NullVertex;
        END (*--with*);
  END (*--if*);
  RETURN newGraph;
END Create;
(*-------------------------*)

PROCEDURE Destroy (VAR theGraph      : Graph     (*-- inout *));
BEGIN
  Clear(theGraph);
  IF (graphError = noerr) THEN
    Deallocate(theGraph, SIZE(theGraph^));
  END (*--if*);
END Destroy;
(*-------------------------*)

PROCEDURE Clear   (VAR theGraph      : Graph    (*-- inout *));

VAR   theVertex : Vertex;          (*-- loop index over vertices *)
VAR   oldVertex : Vertex;          (*-- vertex to deallocate *)

BEGIN
  graphError := noerr;
  IF (theGraph = NullGraph) THEN
    RaiseErrIn(clear, undefined);
  ELSE
    WITH theGraph^ DO
          theVertex := firstVertex;
          WHILE (theVertex # NullVertex) DO
            ClearEdges(theVertex);
                oldVertex := theVertex;
                theVertex := theVertex^.next;
                Deallocate(oldVertex, SIZE(oldVertex^));
          END (*--while*);
          firstVertex := NullVertex;
          numVertices := 0;
          numEdges    := 0;
        END (*--with*);
  END (*--if*);
END Clear;
(*-------------------------*)

PROCEDURE Assign  (    theGraph      : Graph     (*-- in    *);
                                   VAR toGraph       : Graph     (*-- inout *));

  PROCEDURE RecreateTarget (): BOOLEAN (*-- out   *);
  BEGIN
        IF (theGraph = NullGraph) THEN
      RaiseErrIn(assign, undefined);

        ELSIF (toGraph = NullGraph) THEN
      WITH theGraph^ DO
        toGraph := Create();
          END (*--with*);

        ELSIF (theGraph = toGraph) THEN
      RETURN FALSE;

        ELSE
          Clear(toGraph);
        END (*--if*);
        RETURN (graphError = noerr);
  END RecreateTarget;

  TYPE  MapVertex = RECORD
              old : Vertex; (*-- vertex from source graph *)
              new : Vertex; (*-- corresponging vertex in target graph *)
            END (*--MapVertex*);
  TYPE  MapVertices = ARRAY [0..0] OF MapVertex;

  VAR   vertexMap : POINTER TO MapVertices;
  VAR   mapExtent : CARDINAL;

  PROCEDURE CreateVertexMap;
  BEGIN
        Allocate(vertexMap,
                         VAL(CARDINAL, SIZE(MapVertex)) * theGraph^.numVertices);
        mapExtent := 0;
  END CreateVertexMap;

  PROCEDURE AddToVertexMap (    oldVertex : Vertex (*--in   *);
                                                                newVertex : Vertex (*--in   *));
  BEGIN
        WITH vertexMap^[mapExtent] DO
          old := oldVertex;
          new := newVertex;
        END (*--with*);
    INC(mapExtent);
  END AddToVertexMap;

  PROCEDURE VertexInMap (    oldVertex : Vertex (*--in   *))
                                                                           : Vertex (*--out  *);

  VAR   index : CARDINAL; (*-- loop index over mapping entries *)

  BEGIN
    FOR index := 0 TO mapExtent-1 DO
          WITH vertexMap^[index] DO
        IF (oldVertex = old) THEN
                  RETURN new;
                END (*--if*);
          END (*--with*);
    END (*--for*);
        RETURN NullVertex;
  END VertexInMap;

  PROCEDURE DestroyVertexMap;
  BEGIN
        Deallocate(vertexMap, SIZE(vertexMap^));
  END DestroyVertexMap;

  PROCEDURE CopyVertices () : BOOLEAN;

  VAR   v : Vertex;     (*--loop index over vertices being copied *)
  VAR   newVertex : Vertex; (*--new vertex in target graph *)
  VAR   lastVertex: Vertex; (*--last vertex added to list of vertices *)

    PROCEDURE TailInsert (VAR first : Vertex (*--inout *);
                                                  VAR last  : Vertex (*--inout *));
    BEGIN
      IF (first = NullVertex) THEN
            first := newVertex;
          ELSE
            last^.next := newVertex;
          END (*--if*);
          last := newVertex;
    END TailInsert;

  BEGIN
        CreateVertexMap;
        IF (vertexMap = NIL) THEN
          RETURN FALSE;
        END (*--if*);

        v := theGraph^.firstVertex;
        lastVertex := NullVertex;

    WHILE (v # NullVertex) DO
          newVertex := NewVertex(toGraph, v^.data, assign);
          IF (newVertex = NullVertex) THEN
            DestroyVertexMap;
                RETURN FALSE;
          END (*--if*);

          newVertex^.indegree := v^.indegree;
          TailInsert(toGraph^.firstVertex, lastVertex);
          INC(toGraph^.numVertices);
          AddToVertexMap(v, newVertex);

          v := v^.next;
        END (*--while*);

        RETURN TRUE;
  END CopyVertices;


  PROCEDURE CopyEdges;

  VAR   v : Vertex;                     (*--loop index over vertices in source graph *)
  VAR   e : Edge;               (*--loop index over edges in source graph *)
  VAR   fromVertex: Vertex; (*--vertex in target graph corresponding to v *)
  VAR   newEdge   : Edge;       (*--new edge for target graph *)
  VAR   lastEdge  : Edge;       (*--last edge inserted into new list of edges *)

    PROCEDURE TailInsert (VAR first : Edge (*--inout *);
                                                  VAR last  : Edge (*--inout *));
    BEGIN
      IF (first = NullEdge) THEN
            first := newEdge;
          ELSE
            last^.next := newEdge;
          END (*--if*);
          last := newEdge;
    END TailInsert;

  BEGIN
        v := theGraph^.firstVertex;
    WHILE (v # NullVertex) DO
          e := v^.edges;
          lastEdge := NullEdge;
          fromVertex := VertexInMap(v);
          WHILE (e # NullEdge) DO
            newEdge := NewEdge(fromVertex,
                                                   VertexInMap(e^.final),
                                                   e^.weight,
                                                   assign);
                IF (newEdge = NullEdge) THEN
                  RETURN;
                END (*--if*);

                TailInsert(fromVertex^.edges, lastEdge);
                INC(toGraph^.numEdges);
                e := e^.next;
          END (*--while*);
          v := v^.next;
        END (*--while*);
  END CopyEdges;

BEGIN (*--Assign --*)
  graphError := noerr;
  IF RecreateTarget() & CopyVertices() THEN
        CopyEdges;
        DestroyVertexMap;
  END (*--if*);
END Assign;
(*-------------------------*)

(*
10.5.5  Vertex Constructors
*)

PROCEDURE Insert   (VAR theGraph   : Graph     (*--inout*);
                                        theItem    : Label     (*--in   *);
                                        VAR theVertex  : Vertex    (*--out  *));
BEGIN
  graphError := noerr;
  theVertex  := NullVertex;

  IF (theGraph = NullGraph) THEN
    RaiseErrIn(insert, undefined);
  ELSE
    theVertex := NewVertex(theGraph, theItem, insert);
        IF (theVertex # NullVertex) THEN
      WITH theGraph^ DO
                theVertex^.next := firstVertex;
            firstVertex := theVertex;
            INC(numVertices);
          END (*--with*);
    END (*--if*);
  END (*--if*);
END Insert;
(*-------------------------*)

PROCEDURE Remove   (VAR theGraph   : Graph     (*--inout*);
                                    VAR theVertex  : Vertex    (*--inout*));

VAR   loopVertex : Vertex; (*--loop index over vertices *)
VAR   priorVertex: Vertex; (*--immediate predecessor of theVertex *)

BEGIN
  graphError := noerr;
  IF (theGraph = NullGraph) THEN
    RaiseErrIn(remove, undefined);

  ELSIF (theVertex = NullVertex) THEN
    RaiseErrIn(remove, nullvertex);

  ELSIF (theVertex^.inGraph # theGraph) THEN
    RaiseErrIn(remove, novertex);

  ELSIF (theVertex^.indegree > 0) THEN
        RaiseErrIn(remove, referenced);

  ELSE
    loopVertex  := theGraph^.firstVertex;
        priorVertex := NullVertex;
        WHILE (loopVertex # theVertex) DO
          priorVertex := loopVertex;
          loopVertex  := loopVertex^.next;
        END (*--while*);

        ClearEdges(theVertex);
        IF (priorVertex = NullVertex) THEN
          theGraph^.firstVertex := theVertex^.next;
        ELSE
          priorVertex^.next := theVertex^.next;
        END (*--if*);
        Deallocate(theVertex, SIZE(theVertex^));
        DEC(theGraph^.numVertices);
  END (*--if*);
END Remove;
(*-------------------------*)

PROCEDURE SetLabel (    theVertex  : Vertex    (*--inout*);
                                            theItem    : Label     (*--in   *));
BEGIN
  graphError := noerr;
  IF (theVertex = NullVertex) THEN
    RaiseErrIn(setlabel, nullvertex);
  ELSE
    theVertex^.data := theItem;
  END (*--if*);
END SetLabel;
(*-------------------------*)

(*
10.5.6  Edge Constructors
*)

PROCEDURE Link          (VAR theGraph   : Graph     (*--inout*);
                                             fromVertex : Vertex    (*--in   *);
                                             toVertex   : Vertex    (*--in   *);
                                             theWeight  : Attribute (*--in   *);
                                             VAR theEdge    : Edge      (*--out  *));
BEGIN
  graphError := noerr;
  theEdge    := NullEdge;

  IF (theGraph = NullGraph) THEN
    RaiseErrIn(link, undefined);
  ELSIF (fromVertex = NullVertex) OR (toVertex = NullVertex) THEN
    RaiseErrIn(link, nullvertex);
  ELSIF (fromVertex^.inGraph # theGraph) OR
                (toVertex^.inGraph # theGraph) THEN
    RaiseErrIn(link, novertex);
  ELSE

    theEdge := NewEdge(fromVertex, toVertex, theWeight, link);
        IF (theEdge # NullEdge) THEN
          theEdge^.next := fromVertex^.edges;
          fromVertex^.edges := theEdge;
          IF (fromVertex # toVertex) THEN
                INC(toVertex^.indegree);
          END (*--if*);
          INC(theGraph^.numEdges);
        END (*--if*);
  END (*--if*);
END Link;
(*-------------------------*)

PROCEDURE Unlink        (VAR theGraph   : Graph     (*--inout*);
                                         VAR theEdge    : Edge      (*--inout*));

VAR   e : Edge;   (*--pointer to edge (v,w), if any *)
VAR   f : Edge;   (*--pointer to edge preceeding (v,w) in adjacency list *)

BEGIN
  graphError := noerr;

  IF (theGraph = NullGraph) THEN
    RaiseErrIn(unlink, undefined);

  ELSIF (theEdge = NullEdge) THEN
        RaiseErrIn(unlink, nulledge);

  ELSIF (theEdge^.initial^.inGraph # theGraph) THEN
    RaiseErrIn(unlink, noedge);

  ELSE

        e := theEdge^.initial^.edges;
        f := NullEdge;
        WHILE (e # theEdge) DO
          f := e;
          e := e^.next;
        END (*--while*);

        WITH theEdge^ DO

          IF (f = NullEdge) THEN
            initial^.edges := next;
          ELSE
            f^.next := next;
          END (*--if*);

          IF (initial # final) THEN
            DEC(final^.indegree);
          END (*--if*);
          DEC(initial^.inGraph^.numEdges);

          initial := NullVertex;
          next    := NullEdge;
          Deallocate(theEdge, SIZE(theEdge^));
        END (*--with*);
  END (*--if*);
END Unlink;
(*-------------------------*)

PROCEDURE SetAttribute  (    theEdge    : Edge      (*--inout*);
                                             theWeight  : Attribute (*--in   *));
BEGIN
  graphError := noerr;
  IF (theEdge = NullEdge) THEN
        RaiseErrIn(setattr, nulledge);
  ELSE
    theEdge^.weight := theWeight;
  END (*--if*);
END SetAttribute;
(*-------------------------*)

(*
10.5.7  Graph Selectors
*)

PROCEDURE IsDefined (    theGraph  : Graph       (*-- in    *))
                                                                   : BOOLEAN     (*-- out   *);
BEGIN
  RETURN (theGraph # NullGraph);
END IsDefined;
(*-------------------------*)

PROCEDURE IsEmpty   (    theGraph  : Graph       (*-- in    *))
                                                                   : BOOLEAN     (*-- out   *);
BEGIN
  graphError := noerr;
  IF (theGraph = NullGraph) THEN
    RaiseErrIn(isempty, undefined);
        RETURN TRUE;
  END (*--if*);
  RETURN theGraph^.numVertices = 0;
END IsEmpty;
(*-------------------------*)

PROCEDURE OrderOf   (    theGraph  : Graph       (*-- in    *))
                                                                   : CARDINAL    (*-- out   *);
BEGIN
  graphError := noerr;
  IF (theGraph = NullGraph) THEN
    RaiseErrIn(orderof, undefined);
        RETURN 0;
  END (*--if*);
  RETURN theGraph^.numVertices;
END OrderOf;
(*-------------------------*)

PROCEDURE SizeOf    (    theGraph  : Graph       (*-- in    *))
                                                                   : CARDINAL    (*-- out   *);
BEGIN
  graphError := noerr;
  IF (theGraph = NullGraph) THEN
    RaiseErrIn(sizeof, undefined);
        RETURN 0;
  END (*--if*);
  RETURN theGraph^.numEdges;
END SizeOf;
(*-------------------------*)

(*
10.5.8  Vertex Selectors
*)

PROCEDURE InDegree  (    theVertex : Vertex      (*--in   *))
                                                                   : CARDINAL    (*--out  *);
BEGIN
  graphError := noerr;
  IF (theVertex = NullVertex) THEN
    RaiseErrIn(indegree, nullvertex);
  ELSE
    RETURN theVertex^.indegree;
  END (*--if*);
  RETURN 0;
END InDegree;
(*-------------------------*)

PROCEDURE OutDegree (    theVertex : Vertex      (*--in   *))
                                                                   : CARDINAL    (*--out  *);

VAR   theEdge   : Edge;         (*-- loop index over edges of the vertex *)
VAR   edgeCount : CARDINAL; (*--running count of edges leaving this vertex *)

BEGIN
  graphError := noerr;
  edgeCount  := 0;

  IF (theVertex = NullVertex) THEN
    RaiseErrIn(outdegree, nullvertex);
  ELSE
    theEdge := theVertex^.edges;
        WHILE (theEdge # NullEdge) DO
          INC(edgeCount);
          theEdge := theEdge^.next;
        END (*--while*);
  END (*--if*);

  RETURN edgeCount;
END OutDegree;
(*-------------------------*)

PROCEDURE LabelOf   (    theVertex : Vertex      (*--in   *))
                                                                   : Label       (*--out  *);
BEGIN
  graphError := noerr;
  IF (theVertex = NullVertex) THEN
    RaiseErrIn(labelof, nullvertex);
  ELSE
    RETURN theVertex^.data;
  END (*--if*);
  RETURN NullItem;
END LabelOf;
(*-------------------------*)

PROCEDURE IsVertex  (    theGraph  : Graph       (*--in   *);
                                             theVertex : Vertex      (*--in   *))
                                                           : BOOLEAN     (*--out  *);
BEGIN
  graphError := noerr;
  IF (theGraph = NullGraph) THEN
    RaiseErrIn(isvertex, undefined);
  ELSIF (theVertex = NullVertex) THEN
    RaiseErrIn(isvertex, nullvertex);
  ELSE
    RETURN theVertex^.inGraph = theGraph;
  END (*--if*);
  RETURN FALSE;
END IsVertex;
(*-------------------------*)

PROCEDURE GraphOf   (    theVertex : Vertex      (*--in   *))
                                                                   : Graph       (*--out  *);
BEGIN
  graphError := noerr;
  IF (theVertex = NullVertex) THEN
    RaiseErrIn(graphof, nullvertex);
  ELSE
    RETURN theVertex^.inGraph;
  END (*--if*);
  RETURN NullGraph;
END GraphOf;
(*-------------------------*)

(*
10.5.9  Edge Selectors
*)

PROCEDURE AttributeOf (    theEdge   : Edge        (*--in   *))
                                                                 : Attribute   (*--out  *);
BEGIN
  graphError := noerr;
  IF (theEdge = NullEdge) THEN
    RaiseErrIn(attrof, nulledge);
  ELSE
    RETURN theEdge^.weight;
  END (*--if*);
  RETURN NullItem;
END AttributeOf;
(*-------------------------*)

PROCEDURE InitialOf   (    theEdge   : Edge        (*--in   *))
                                                                 : Vertex      (*--out  *);
BEGIN
  graphError := noerr;
  IF (theEdge = NullEdge) THEN
    RaiseErrIn(initialof, nulledge);
  ELSE
    RETURN theEdge^.initial;
  END (*--if*);
  RETURN NullVertex;
END InitialOf;
(*-------------------------*)

PROCEDURE FinalOf     (    theEdge   : Edge        (*--in   *))
                                                                 : Vertex      (*--out  *);
BEGIN
  graphError := noerr;
  IF (theEdge = NullEdge) THEN
    RaiseErrIn(finalof, nulledge);
  ELSE
    RETURN theEdge^.final;
  END (*--if*);
  RETURN NullVertex;
END FinalOf;
(*-------------------------*)

PROCEDURE IsEdge      (    theGraph  : Graph       (*--in   *);
                                               theEdge   : Edge        (*--in   *))
                                                             : BOOLEAN     (*--out  *);
BEGIN
  graphError := noerr;
  IF (theGraph = NullGraph) THEN
    RaiseErrIn(isedge, undefined);
  ELSIF (theEdge = NullEdge) THEN
    RaiseErrIn(isedge, nulledge);
  ELSIF (theEdge^.initial = NullVertex) THEN
    RaiseErrIn(isedge, nullvertex);
  ELSE
    RETURN theEdge^.initial^.inGraph = theGraph;
  END (*--if*);
  RETURN FALSE;
END IsEdge;
(*-------------------------*)

(*
10.5.10 Passive Iterators
*)

PROCEDURE LoopVertices (    theGraph  : Graph          (*--in   *);
                                                        process   : VertexLoopProc (*--in   *));

VAR   theVertex : Vertex; (*--loop index over vertices *)

BEGIN
  graphError := noerr;
  IF (theGraph = NullGraph) THEN
    RaiseErrIn(loopvertices, undefined);
  ELSE
        theVertex := theGraph^.firstVertex;
        WHILE (theVertex # NullVertex) & process(theVertex) DO
          theVertex := theVertex^.next;
        END (*--while*);
  END (*--if*);
END LoopVertices;
(*-------------------------*)

PROCEDURE LoopEdges    (    theGraph  : Graph          (*--in   *);
                                                        process   : EdgeLoopProc   (*--in   *));

VAR   theVertex : Vertex; (*--loop index over vertices *)
VAR   theEdge   : Edge;   (*--loop index over edges of a vertex *)

BEGIN
  graphError := noerr;
  IF (theGraph = NullGraph) THEN
    RaiseErrIn(loopedges, undefined);
  ELSE
        theVertex := theGraph^.firstVertex;
        WHILE (theVertex # NullVertex) DO
          theEdge := theVertex^.edges;
          WHILE (theEdge # NullEdge) DO
            IF ~process(theEdge) THEN
                  RETURN;
                END (*--if*);
                theEdge := theEdge^.next;
          END (*--while*);
          theVertex := theVertex^.next;
        END (*--while*);
  END (*--if*);
END LoopEdges;
(*-------------------------*)

PROCEDURE LoopIterate  (    theVertex : Vertex         (*--in   *);
                                                        process   : EdgeLoopProc   (*--in   *));

VAR   theEdge : Edge;   (*--loop index over edges of the vertex *)

BEGIN
  graphError := noerr;
  IF (theVertex = NullVertex) THEN
    RaiseErrIn(loopiterate, nullvertex);
  ELSE
        theEdge := theVertex^.edges;
        WHILE (theEdge # NullEdge) & process(theEdge) DO
          theEdge := theEdge^.next;
        END (*--while*);
  END (*--if*);
END LoopIterate;
(*-------------------------*)


PROCEDURE TravVertices (    theGraph  : Graph      (*--in   *);
                                                        process   : VertexProc (*--in   *));

VAR   theVertex : Vertex; (*--loop index over vertices *)

BEGIN
  graphError := noerr;
  IF (theGraph = NullGraph) THEN
    RaiseErrIn(travvertices, undefined);
  ELSE
        theVertex := theGraph^.firstVertex;
        WHILE (theVertex # NullVertex) DO
          process(theVertex);
          theVertex := theVertex^.next;
        END (*--while*);
  END (*--if*);
END TravVertices;
(*-------------------------*)

PROCEDURE TravEdges    (    theGraph  : Graph      (*--in   *);
                                                        process   : EdgeProc   (*--in   *));

VAR   theVertex : Vertex; (*--loop index over vertices *)
VAR   theEdge   : Edge;   (*--loop index over edges of a vertex *)

BEGIN
  graphError := noerr;
  IF (theGraph = NullGraph) THEN
    RaiseErrIn(travedges, undefined);
  ELSE
        theVertex := theGraph^.firstVertex;
        WHILE (theVertex # NullVertex) DO
          theEdge := theVertex^.edges;
          WHILE (theEdge # NullEdge) DO
            process(theEdge);
                theEdge := theEdge^.next;
          END (*--while*);
          theVertex := theVertex^.next;
        END (*--while*);
  END (*--if*);
END TravEdges;
(*-------------------------*)

PROCEDURE Iterate      (    theVertex : Vertex     (*--in   *);
                                                        process   : EdgeProc   (*--in   *));

VAR   theEdge : Edge;   (*--loop index over edges of the vertex *)

BEGIN
  graphError := noerr;
  IF (theVertex = NullVertex) THEN
    RaiseErrIn(iterate, nullvertex);
  ELSE
        theEdge := theVertex^.edges;
        WHILE (theEdge # NullEdge) DO
          process(theEdge);
          theEdge := theEdge^.next;
        END (*--while*);
  END (*--if*);
END Iterate;
(*-------------------------*)

(*
10.5.11 Active Iterators
*)

PROCEDURE FirstVertex  (    theGraph  : Graph      (*-- in    *))
                                                                          : Vertex     (*-- out   *);
BEGIN
  graphError := noerr;
  IF (theGraph = NullGraph) THEN
    RaiseErrIn(firstvertex, undefined);
  ELSE
        RETURN theGraph^.firstVertex;
  END (*--if*);
  RETURN NullVertex;
END FirstVertex;
(*-------------------------*)

PROCEDURE NextVertex   (    theVertex : Vertex     (*-- in    *))
                                                                          : Vertex     (*-- out   *);
BEGIN
  graphError := noerr;
  IF (theVertex = NullVertex) THEN
    RaiseErrIn(nextvertex, nullvertex);
  ELSE
        RETURN theVertex^.next;
  END (*--if*);
  RETURN NullVertex;
END NextVertex;
(*-------------------------*)

PROCEDURE FirstEdge    (    theVertex : Vertex     (*-- in    *))
                                                                          : Edge       (*-- out   *);
BEGIN
  graphError := noerr;
  IF (theVertex = NullVertex) THEN
    RaiseErrIn(firstedge, nullvertex);
  ELSE
    RETURN theVertex^.edges;
  END (*--if*);
  RETURN NullEdge;
END FirstEdge;
(*-------------------------*)

PROCEDURE NextEdge     (    theEdge   : Edge       (*-- in    *))
                                                                          : Edge       (*-- out   *);
BEGIN
  graphError := noerr;
  IF (theEdge = NullEdge) THEN
    RaiseErrIn(nextedge, nulledge);
  ELSE
    RETURN theEdge^.next;
  END (*--if*);
  RETURN NullEdge;
END NextEdge;
(*-------------------------*)

(*
10.5.12 Module Initialization
*)

BEGIN
  FOR graphError := MIN(Exceptions) TO MAX(Exceptions) DO
    SetHandler(graphError, NullHandler);
  END (*--for*);
  graphError := noerr;
        NullGraph := NIL;
        NullVertex := NIL;
        NullEdge := NIL;
END DigrSUUI.
