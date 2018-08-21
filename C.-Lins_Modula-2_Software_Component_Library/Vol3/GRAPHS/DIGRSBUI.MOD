(*
11.4    Digraph - Sequential Bounded Unmanaged Iterator

In this section we provide the implementation module corresponding to
the interface given above in 11.3. The following scheme is used in
organizing this section:
* 11.4.1  Internal Representation
* 11.4.2  Exception Handling
* 11.4.3  Local Routines
* 11.4.4  Graph Constructors
* 11.4.5  Vertex Constructors
* 11.4.6  Edge Constructors
* 11.4.7  Graph Selectors
* 11.4.8  Vertex Selectors
* 11.4.9  Edge Selectors
* 11.4.10 Passive Iterators
* 11.4.11 Active Iterators
* 11.4.12 Module Initialization
*)

IMPLEMENTATION MODULE DigrSBUI;
(*====================================================================
    Version  : V2.01  08 December 1989.
    Compiler : JPI TopSpeed Modula-2
    Code size:  5830 bytes
    Component: Graph Data Type - Directed Bounded Unmanaged Iterator

    REVISION HISTORY
    v1.00  26 May 1988  C. Lins:
        Initial TML Modula-2 implementation
    v1.01  26 Jan 1989  C. Lins:
        Re-derived from DigraphSBMI module.
    v1.02  10 Apr 1989  C. Lins:
        Corrected initialization of handlers array.
    v1.03  18 Apr 1989   C. Lins:
          Added component id constant.
    v2.00  24 Oct 1989  C. Lins
          Created generic pc version
    v2.01  08 Dec 1989  I.S.C. Houston.
          Adapted to JPI Compiler:
          Used type transfer functions instead of VAL.
          Used shortened library module names for DOS and OS/2.
          Used JPIStorage Allocate and Deallocate procedures.

        (C) Copyright 1989 Charles A. Lins
====================================================================*)

FROM SYSTEM IMPORT
        (*--type*) ADDRESS,
        (*--proc*) ADR;

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
11.4.1  Type Declarations
*)

TYPE  Vertex = POINTER TO VertexNode;
TYPE  Edge   = POINTER TO EdgeNode;

TYPE  VertexNode = RECORD
                inGraph : Graph;        (*-- graph in which this vertex is a member *)
                data    : Label;        (*-- data item (label) for this vertex *)
                indegree: CARDINAL; (*-- # of edges ending at this vertex *)
                next    : CARDINAL;     (*-- next vertex in adjacency list *)
                edges   : Edge;         (*-- link to first edge leaving this vertex *)
          END (*-- VertexNode *);

TYPE  EdgeNode = RECORD
                initial : Vertex;       (*-- source vertex for this edge *)
                final   : Vertex;       (*-- destination vertex for this edge *)
                weight  : Attribute;(*-- weight/attribute for this edge *)
                next    : Edge;         (*-- next edge leaving this vertex *)
          END (*-- EdgeNode *);

CONST MaxVertex = 2000;
TYPE  VertexIndex = [1 .. MaxVertex];
TYPE  AdjList = ARRAY VertexIndex OF VertexNode;

TYPE  BoundedGraph = RECORD
                maxVertices: CARDINAL;  (*-- maximum number of vertices *)
                numVertices: CARDINAL;  (*-- current number of vertices *)
                numEdges   : CARDINAL;  (*-- current number of edges *)
                firstVertex: CARDINAL;  (*-- 1st vertex in adjacency list *)
                available  : CARDINAL;  (*-- 1st free vertex in adjacency list *)
                vertices   : AdjList;   (*-- adjacency list of vertices *)
          END (*-- BoundedGraph *);

TYPE  Graph = POINTER TO BoundedGraph;

        (*-------------------------*)


(*
11.4.2  Exceptions
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
11.4.3  Local Routines
*)

PROCEDURE InitFreeList (VAR theGraph : Graph      (*--inout*));

  PROCEDURE InitVertex   (VAR theNode  : VertexNode (*--inout*);
                                                          theNext  : CARDINAL   (*--in   *));
  BEGIN
    WITH theNode DO
          inGraph  := theGraph;
          data     := NullItem;
          indegree := 0;
          next     := theNext;
          edges    := NullEdge;
    END (*--with*);
  END InitVertex;

VAR   v : VertexIndex; (*-- running index over vertices of the graph *)

BEGIN
  WITH theGraph^ DO
    FOR v := MIN(VertexIndex) TO maxVertices-1 DO
          InitVertex(vertices[v], v+1);
        END (*--for*);
        InitVertex(vertices[maxVertices], 0);
        numVertices := 0;
        firstVertex := 0;
        available   := MIN(VertexIndex);
  END (*--with*);
END InitFreeList;
(*-------------------------*)

PROCEDURE ClearEdges    (    theVertex: Vertex   (*--inout*));

VAR   theEdge : Edge;            (*-- edge to be removed *)

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
11.4.4  Graph Constructors
*)

CONST baseSize = SIZE(BoundedGraph) - SIZE(AdjList);
CONST nodeSize = SIZE(VertexNode);

PROCEDURE Create   (    theSize    : CARDINAL  (*-- in    *))
                                                                   : Graph     (*-- out   *);

VAR   newGraph : Graph; (*-- temporary for new graph object *)

BEGIN
  graphError := noerr;
  Allocate(newGraph, baseSize + (VAL(INTEGER, theSize) * nodeSize));
  IF (newGraph = NullGraph) THEN
    RaiseErrIn(create, overflow);
  ELSE
    WITH newGraph^ DO
          maxVertices := theSize;
          numEdges    := 0;
        END (*--with*);
        InitFreeList(newGraph);
  END (*--if*);
  RETURN newGraph;
END Create;
(*-------------------------*)

PROCEDURE Destroy (VAR theGraph      : Graph     (*-- inout *));

VAR   v : Vertex; (*-- loop index over vertices *)

BEGIN
  Clear(theGraph);
  IF (graphError = noerr) THEN
    Deallocate(theGraph, baseSize + (theGraph^.maxVertices * nodeSize));
  END (*--if*);
END Destroy;
(*-------------------------*)

PROCEDURE Clear   (VAR theGraph      : Graph    (*--inout*));

VAR   theVertex : CARDINAL;        (*-- loop index over vertices *)

BEGIN
  graphError := noerr;
  IF (theGraph = NullGraph) THEN
    RaiseErrIn(clear, undefined);
  ELSE
    WITH theGraph^ DO
          theVertex  := firstVertex;
          WHILE (theVertex # 0) DO
            ClearEdges(ADR(vertices[theVertex]));
                theVertex := vertices[theVertex].next;
          END (*--for*);
        END (*--with*);
        InitFreeList(theGraph);
  END (*--if*);
END Clear;
(*-------------------------*)

PROCEDURE Assign  (    theGraph      : Graph     (*--in   *);
                                   VAR toGraph       : Graph     (*--inout*));

  PROCEDURE RecreateTarget (): BOOLEAN (*-- out   *);
  BEGIN
        IF (theGraph = NullGraph) THEN
      RaiseErrIn(assign, undefined);

        ELSIF (toGraph = NullGraph) THEN
      WITH theGraph^ DO
        toGraph := Create(maxVertices);
          END (*--with*);

        ELSIF (theGraph = toGraph) THEN
      RETURN FALSE;

        ELSIF (theGraph^.numVertices > toGraph^.maxVertices) THEN
          RaiseErrIn(assign, overflow);

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

  VAR   index : CARDINAL;

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

  VAR   v : CARDINAL;   (*--loop index over vertices being copied *)
  VAR   theVertex : CARDINAL; (*-- index to new vertex *)
  VAR   lastVertex: CARDINAL; (*--last vertex added to list of vertices *)

    PROCEDURE TailInsert (VAR first : CARDINAL (*--inout *);
                                                  VAR last  : CARDINAL (*--inout *));
    BEGIN
      IF (first = 0) THEN
            first := theVertex;
          ELSE
            toGraph^.vertices[last].next := theVertex;
          END (*--if*);
          last := theVertex;
    END TailInsert;

  BEGIN
        CreateVertexMap;
        IF (vertexMap = NIL) THEN
          RETURN FALSE;
        END (*--if*);

        v := theGraph^.firstVertex;
        lastVertex := 0;

    WHILE (v # 0) DO
          WITH toGraph^ DO
            theVertex := available;
            available := vertices[available].next;

            WITH vertices[theVertex] DO
              inGraph  := toGraph;
                  data     := theGraph^.vertices[v].data;
                  indegree := theGraph^.vertices[v].indegree;
                  next     := 0;
                  edges    := NullEdge;
            END (*--with*);

            TailInsert(firstVertex, lastVertex);
            INC(numVertices);
            AddToVertexMap(ADR(theGraph^.vertices[v]),
                                           ADR(vertices[theVertex]));
          END (*--with*);
          v := theGraph^.vertices[v].next;
        END (*--while*);

        RETURN TRUE;
  END CopyVertices;

  PROCEDURE CopyEdges;

  VAR   v : CARDINAL;             (*--loop index over vertices *)
  VAR   e : Edge;                 (*--loop index over edges being copied *)
  VAR   fromVertex: Vertex;   (*--vertex in target graph *)
  VAR   newEdge   : Edge;         (*--new edge for target graph *)
  VAR   lastEdge  : Edge;         (*--last edge inserted into new list of edges *)

    PROCEDURE TailInsert (VAR first   : Edge (*--inout*);
                                                  VAR last    : Edge (*--inout*));
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
    WHILE (v # 0) DO
          lastEdge := NullEdge;
          WITH theGraph^ DO
            e := vertices[v].edges;
            fromVertex := VertexInMap(ADR(vertices[v]));
          END (*--with*);
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
          v := theGraph^.vertices[v].next;
        END (*--while*);
  END CopyEdges;

BEGIN (*-- Assign --*)
  graphError := noerr;
  IF RecreateTarget() & CopyVertices() THEN
        CopyEdges;
        DestroyVertexMap;
  END (*--if*);
END Assign;
(*-------------------------*)

(*
11.4.5  Vertex Constructors
*)

PROCEDURE Insert   (VAR theGraph   : Graph     (*--inout*);
                                        theItem    : Label     (*--in   *);
                                        VAR theVertex  : Vertex    (*--out  *));

VAR   theIndex : CARDINAL; (*-- vertex index into the adjacency list *)

BEGIN
  graphError := noerr;
  theVertex  := NullVertex;

  IF (theGraph = NullGraph) THEN
    RaiseErrIn(insert, undefined);
  ELSIF (theGraph^.available = 0) THEN
    RaiseErrIn(insert, overflow);
  ELSE
    WITH theGraph^ DO
          theIndex := available;
          available := vertices[available].next;
          WITH vertices[theIndex] DO
            inGraph  := theGraph;
                data     := theItem;
                indegree := 0;
                next     := firstVertex;
                edges    := NullEdge;
          END (*--with*);
          firstVertex := theIndex;
          INC(numVertices);
          theVertex := ADR(vertices[theIndex]);
        END (*--with*);
  END (*--if*);
END Insert;
(*-------------------------*)

PROCEDURE Remove   (VAR theGraph   : Graph     (*--inout*);
                                    VAR theVertex  : Vertex    (*--inout*));

VAR   theIndex   : CARDINAL; (*-- of theItem to be removed *)
VAR   priorVertex: CARDINAL; (*-- immediate predecessor of theVertex *)

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
        ClearEdges(theVertex);

    WITH theGraph^ DO

      theIndex  := firstVertex;
          priorVertex := 0;
          WHILE (ADR(vertices[theIndex]) # theVertex) DO
            priorVertex := theIndex;
            theIndex    := vertices[theIndex].next;
          END (*--while*);

          IF (priorVertex = 0) THEN
                firstVertex := vertices[theIndex].next;
          ELSE
                vertices[priorVertex].next := vertices[theIndex].next;
          END (*--if*);

          vertices[theIndex].next := available;
          vertices[theIndex].inGraph := NullGraph;
          available := theIndex;
          DEC(numVertices);
        END (*--with*);
        theVertex := NullVertex;
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
11.4.6  Edge Constructors
*)

PROCEDURE Link     (VAR theGraph   : Graph     (*--inout*);
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
        END (*--with*);
  END (*--if*);
END Link;
(*-------------------------*)

PROCEDURE Unlink   (VAR theGraph   : Graph     (*--inout*);
                                    VAR theEdge    : Edge      (*--inout*));

VAR   e : Edge;   (*-- pointer to edge (v,w), if any *)
VAR   f : Edge;   (*-- pointer to edge preceeding (v,w) in adjacency list *)

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
11.4.7  Graph Selectors
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

PROCEDURE MaxOrderOf (    theGraph  : Graph       (*--in   *))
                                                                    : CARDINAL    (*--out  *);
BEGIN
  graphError := noerr;
  IF (theGraph = NullGraph) THEN
    RaiseErrIn(maxorderof, undefined);
        RETURN 0;
  END (*--if*);
  RETURN theGraph^.maxVertices;
END MaxOrderOf;
(*-------------------------*)

(*
11.4.8  Vertex Selectors
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
VAR   edgeCount : CARDINAL; (*-- running count of edges leaving this vertex *)

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
11.4.9  Edge Selectors
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
11.4.10 Passive Iterators
*)

PROCEDURE LoopVertices (    theGraph  : Graph          (*--in   *);
                                                        process   : VertexLoopProc (*--in   *));

VAR   theIndex : CARDINAL; (*--loop index over vertices *)

BEGIN
  graphError := noerr;
  IF (theGraph = NullGraph) THEN
    RaiseErrIn(loopvertices, undefined);
  ELSE
    WITH theGraph^ DO
          theIndex := firstVertex;
          WHILE (theIndex # 0) & process(ADR(vertices[theIndex])) DO
            theIndex := vertices[theIndex].next;
          END (*--while*);
        END (*--with*);
  END (*--if*);
END LoopVertices;
(*-------------------------*)

PROCEDURE LoopEdges    (    theGraph  : Graph          (*--in   *);
                                                        process   : EdgeLoopProc   (*--in   *));

VAR   theIndex  : CARDINAL; (*--loop index over vertices *)
VAR   theEdge   : Edge;   (*--loop index over edges of a vertex *)

BEGIN
  graphError := noerr;
  IF (theGraph = NullGraph) THEN
    RaiseErrIn(loopedges, undefined);
  ELSE
    WITH theGraph^ DO
          theIndex := firstVertex;
          WHILE (theIndex # 0) DO
            theEdge := vertices[theIndex].edges;
            WHILE (theEdge # NullEdge) DO
              IF ~process(theEdge) THEN
                    RETURN;
                  END (*--if*);
                  theEdge := theEdge^.next;
            END (*--while*);
            theIndex := vertices[theIndex].next;
          END (*--with*);
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

VAR   v : CARDINAL; (*-- loop index over vertices *)

BEGIN
  graphError := noerr;
  IF (theGraph = NullGraph) THEN
    RaiseErrIn(travvertices, undefined);
  ELSE
    WITH theGraph^ DO
          v := firstVertex;
          WHILE (v # 0) DO
            process(ADR(vertices[v]));
                v := vertices[v].next;
          END (*--while*);
        END (*--with*);
  END (*--if*);
END TravVertices;
(*-------------------------*)

PROCEDURE TravEdges    (    theGraph  : Graph      (*--in   *);
                                                        process   : EdgeProc   (*--in   *));

VAR   v : CARDINAL; (*-- loop index over vertices *)
VAR   e : Edge;         (*-- loop index over edges of a vertex *)

BEGIN
  graphError := noerr;
  IF (theGraph = NullGraph) THEN
    RaiseErrIn(travedges, undefined);
  ELSE
    WITH theGraph^ DO
          v := firstVertex;
          WHILE (v # 0) DO
                e := vertices[v].edges;
                WHILE (e # NullEdge) DO
              process(e);
                  e := e^.next;
                END (*--while*);
                v := vertices[v].next;
          END (*--while*);
        END (*--with*);
  END (*--if*);
END TravEdges;
(*-------------------------*)

PROCEDURE Iterate      (    theVertex : Vertex     (*--in   *);
                                                        process   : EdgeProc   (*--in   *));

VAR   theEdge : Edge;   (*-- loop index over edges of the vertex *)

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
11.4.12 Active Iterators
*)

PROCEDURE FirstVertex  (    theGraph  : Graph      (*--in   *))
                                                                          : Vertex     (*--out  *);
BEGIN
  graphError := noerr;
  IF (theGraph = NullGraph) THEN
    RaiseErrIn(firstvertex, undefined);
  ELSE
        RETURN ADR(theGraph^.vertices[theGraph^.firstVertex]);
  END (*--if*);
  RETURN NullVertex;
END FirstVertex;
(*-------------------------*)

PROCEDURE NextVertex   (    theVertex : Vertex     (*--in   *))
                                                                          : Vertex     (*--out  *);
BEGIN
  graphError := noerr;
  IF (theVertex = NullVertex) THEN
    RaiseErrIn(nextvertex, nullvertex);
  ELSE
        WITH theVertex^ DO
          IF (next # 0) THEN
                RETURN ADR(inGraph^.vertices[next]);
          END (*--if*);
        END (*--with*);
  END (*--if*);
  RETURN NullVertex;
END NextVertex;
(*-------------------------*)

PROCEDURE FirstEdge    (    theVertex : Vertex     (*--in   *))
                                                                          : Edge       (*--out  *);
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

PROCEDURE NextEdge     (    theEdge   : Edge       (*--in   *))
                                                                          : Edge       (*--out  *);
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
11.4.13 Module Initialization
*)

BEGIN
  FOR graphError := MIN(Exceptions) TO MAX(Exceptions) DO
    SetHandler(graphError, NullHandler);
  END (*--for*);
  SetHandler(noerr, NullHandler);
  graphError := noerr;
        NullGraph := NIL;
        NullVertex := NIL;
        NullEdge := NIL;
END DigrSBUI.
