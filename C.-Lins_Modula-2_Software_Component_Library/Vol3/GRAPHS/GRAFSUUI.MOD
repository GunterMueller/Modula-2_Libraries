IMPLEMENTATION MODULE GrafSUUI;
(*==============================================================
    Version  : V2.01  08 December 1989.
    Compiler : JPI TopSpeed Modula-2
    Code size: 6568 bytes
    Component: Graph - Sequential Unbounded Unmanaged Iterator

    REVISION HISTORY
    v1.00  27 Jan 1989  C. Lins:
        Initial implementation derived from GraphSUMI.
    v1.01  10 Apr 1989  C. Lins:
        Corrected initialization of handlers array.
    v1.02  18 Apr 1989   C. Lins:
        Added component id constant.
    v2.00  24 Oct 1989  C. Lins
          Created generic pc version
    v2.01  08 Dec 1989  I.S.C. Houston.
          Adapted to JPI Compiler:
          Used type transfer functions instead of VAL.
          Used shortened library module names for DOS and OS/2.

        (C) Copyright 1989 Charles A. Lins
==============================================================*)

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

TYPE  EdgePtr = POINTER TO EdgeNode;
TYPE  Edge    = POINTER TO EdgeRefNode;
TYPE  Vertex  = POINTER TO VertexNode;

CONST NullRef = NIL;

TYPE  EdgeRefNode = RECORD
                realEdge : EdgePtr;     (*--link to the actual edge node *)
                prior    : Edge;        (*--prior edge in this edge list *)
                next     : Edge;        (*--next edge in this edge list *)
          END (*--EdgeRefNode *);

TYPE  VertexNode = RECORD
                inGraph : Graph;   (*--graph in which this vertex is a member *)
                data    : Label;   (*--data item (label) for this vertex *)
                degree  : CARDINAL;(*--degree of this vertex *)
                prior   : Vertex;  (*--prior vertex in adjacency list *)
                next    : Vertex;  (*--next vertex in adjacency list *)
                edges   : Edge;    (*--link to first edge of this vertex *)
          END (*--VertexNode *);

TYPE  EdgeNode = RECORD
                first   : Vertex;       (*--first vertex for this edge *)
                second  : Vertex;       (*--second vertex for this edge *)
                weight  : Attribute;(*--weight/attribute for this edge *)
                edgeRef1: Edge;     (*--link to edge in 1st vertex edge list *)
                edgeRef2: Edge;     (*--link to edge in 2nd vertex edge list *)
                prior   : EdgePtr;      (*--prior edge in the set of all edges *)
                next    : EdgePtr;      (*--next edge in the set of all edges *)
          END (*--EdgeNode *);

TYPE  UnboundedGraph = RECORD
                numVertices: CARDINAL;  (*--current number of vertices *)
                numEdges   : CARDINAL;  (*--current number of edges *)
                firstVertex: Vertex;    (*--first vertex in adjacency list *)
                firstEdge  : EdgePtr;   (*--first edge in edge set *)
          END (*--UnboundedGraph *);

TYPE  Graph = POINTER TO UnboundedGraph;

        (*-------------------------*)

VAR   graphError : Exceptions;
VAR   handlers   : ARRAY Exceptions OF HandlerProc;

PROCEDURE GraphError () : Exceptions;
BEGIN
  RETURN graphError;
END GraphError;
(*-------------------------*)

PROCEDURE SetHandler (    theError   : Exceptions  (*--in   *);
                                                  theHandler : HandlerProc (*--in   *));
BEGIN
  handlers[theError] := theHandler;
END SetHandler;
(*-------------------------*)

PROCEDURE GetHandler (    theError   : Exceptions  (*--in   *))
                                                                         : HandlerProc (*--out  *);
BEGIN
  RETURN handlers[theError];
END GetHandler;
(*-------------------------*)

PROCEDURE RaiseErrIn (    theRoutine : Operations  (*--in   *);
                                                  theError   : Exceptions  (*--in   *));
BEGIN
  graphError := theError;
  Raise(ComponentID + ModuleID, theRoutine, theError, handlers[theError]);
END RaiseErrIn;
(*-------------------------*)


PROCEDURE NewVertex (    theGraph   : Graph      (*--in   *);
                                                 theItem    : Label      (*--in   *);
                                                 theRoutine : Operations (*--in   *))
                                                                    : Vertex     (*--out  *);

VAR   newVertex : Vertex;               (*-- newly created vertex *)

BEGIN
  Allocate(newVertex, SIZE(VertexNode));
  IF (newVertex = NullVertex) THEN
        RaiseErrIn(theRoutine, overflow);
  ELSE
        WITH newVertex^ DO
          inGraph := theGraph;
          data    := theItem;
          degree  := 0;
          edges   := NullEdge;
          prior   := NullVertex;
          next    := NullVertex;
        END (*--with*);
  END (*--if*);
  RETURN newVertex;
END NewVertex;
(*-------------------------*)

PROCEDURE AddVertex (    theGraph  : Graph  (*--inout*);
                                             theVertex : Vertex (*--inout*));
BEGIN
  WITH theGraph^ DO
        IF (firstVertex # NullVertex) THEN
          firstVertex^.prior := theVertex;
        END (*--if*);
        theVertex^.next := firstVertex;
        firstVertex := theVertex;
        INC(numVertices);
  END (*--with*);
END AddVertex;
(*-------------------------*)

PROCEDURE FreeVertex (    theGraph  : Graph  (*--inout*);
                                          VAR theVertex : Vertex (*--inout*));
BEGIN
  WITH theVertex^ DO
        IF (prior = NullVertex) THEN
          theGraph^.firstVertex := next;
        ELSE
          prior^.next := next;
        END (*--if*);
        IF (next # NullVertex) THEN
          next^.prior := prior;
        END (*--if*);
  END (*--with*);
  DEC(theGraph^.numVertices);
  Deallocate(theVertex, SIZE(theVertex^));
END FreeVertex;
(*-------------------------*)


PROCEDURE NewEdge (    vertex1    : Vertex     (*--in   *);
                                           vertex2    : Vertex     (*--in   *);
                                           theWeight  : Attribute  (*--in   *);
                                           theRoutine : Operations (*--in   *))
                                                          : EdgePtr    (*--out  *);

VAR       newEdgeRef : EdgePtr; (*--new edge being created *)

BEGIN
  Allocate(newEdgeRef, SIZE(EdgeNode));
  IF (newEdgeRef = NullRef) THEN
    RaiseErrIn(theRoutine, overflow);
  ELSE
        WITH newEdgeRef^ DO
          first    := vertex1;
          second   := vertex2;
          weight   := theWeight;
          edgeRef1 := NullEdge;
          edgeRef2 := NullEdge;
          next     := NullRef;
          prior    := NullRef;
        END (*--with*);
  END (*--if*);
  RETURN newEdgeRef;
END NewEdge;
(*-------------------------*)

PROCEDURE AddEdge (    theGraph : Graph    (*--inout*);
                                           theEdge  : EdgePtr  (*--inout*));
BEGIN
  WITH theGraph^ DO
        IF (firstEdge # NullRef) THEN
          firstEdge^.prior := theEdge;
        END (*--if*);
        theEdge^.next := firstEdge;
        firstEdge := theEdge;
        INC(numEdges);
  END (*--with*);
END AddEdge;
(*-------------------------*)

PROCEDURE FreeEdge (    theGraph : Graph    (*--inout*);
                                        VAR theEdge  : EdgePtr  (*--inout*));
BEGIN
  WITH theEdge^ DO
        IF (prior = NullRef) THEN
          theGraph^.firstEdge := next;
        ELSE
          prior^.next := next;
        END (*--if*);
        IF (next # NullRef) THEN
          next^.prior := prior;
        END (*--if*);
  END (*--with*);
  DEC(theGraph^.numEdges);
  Deallocate(theEdge, SIZE(theEdge^));
END FreeEdge;
(*-------------------------*)


PROCEDURE NewEdgeRef (    theEdgePtr : EdgePtr    (*--in   *);
                                              theRoutine : Operations (*--in   *))
                                                             : Edge           (*--out  *);

VAR       newEdge : Edge; (*--new edge reference being created *)

BEGIN
  Allocate(newEdge, SIZE(EdgeRefNode));
  IF (newEdge = NullEdge) THEN
    RaiseErrIn(theRoutine, overflow);
  ELSE
        WITH newEdge^ DO
          realEdge := theEdgePtr;
          next     := NullEdge;
          prior    := NullEdge;
        END (*--with*);
  END (*--if*);
  RETURN newEdge;
END NewEdgeRef;
(*-------------------------*)

PROCEDURE AddEdgeRef (    theVertex : Vertex (*--inout*);
                                                  theEdge   : Edge   (*--inout*));
BEGIN
  WITH theVertex^ DO
        IF (edges # NullEdge) THEN
          edges^.prior := theEdge;
        END (*--if*);
    theEdge^.next := edges;
    edges := theEdge;
        INC(degree);
  END (*--with*);
END AddEdgeRef;
(*-------------------------*)

PROCEDURE FreeEdgeRef (    theVertex : Vertex (*--inout*);
                                           VAR theEdge   : Edge   (*--inout*));
BEGIN
  WITH theEdge^ DO
        IF (prior = NullEdge) THEN
          theVertex^.edges := next;
        ELSE
          prior^.next := next;
        END (*--if*);
        IF (next # NullEdge) THEN
          next^.prior := prior;
        END (*--if*);
  END (*--with*);
  DEC(theVertex^.degree);
  Deallocate(theEdge, SIZE(theEdge^));
END FreeEdgeRef;
(*-------------------------*)

PROCEDURE ClearEdgeRefs (VAR theVertex : Vertex    (*--inout*));

VAR   theEdge : Edge; (*--edge reference being removed *)

BEGIN
  WITH theVertex^ DO
        WHILE (edges # NIL) DO
          theEdge := edges;
          edges := edges^.next;
          DEC(degree);
          Deallocate(theEdge, SIZE(theEdge^));
        END (*--while*);
  END (*--with*);
END ClearEdgeRefs;
(*-------------------------*)


PROCEDURE Create   ()                      : Graph     (*--out  *);

VAR   newGraph : Graph; (*--temporary for new graph object *)

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
          firstEdge   := NullRef;
        END (*--with*);
  END (*--if*);
  RETURN newGraph;
END Create;
(*-------------------------*)

PROCEDURE Destroy (VAR theGraph      : Graph     (*--inout *));
BEGIN
  Clear(theGraph);
  IF (graphError = noerr) THEN
    Deallocate(theGraph, SIZE(theGraph^));
  END (*--if*);
END Destroy;
(*-------------------------*)

PROCEDURE Clear   (VAR theGraph      : Graph    (*--inout *));

  PROCEDURE ClearEdges (VAR theGraph : Graph    (*--inout *));

  VAR   theEdge : EdgePtr; (*--edge to be removed *)

  BEGIN
    WITH theGraph^ DO
          WHILE (firstEdge # NullRef) DO
            theEdge := firstEdge;
            firstEdge := firstEdge^.next;
            DEC(numEdges);
            Deallocate(theEdge, SIZE(theEdge^));
          END (*--while*);
    END (*--with*);
  END ClearEdges;
  (*-------------------------*)

VAR   theVertex : Vertex;          (*--loop index over vertices *)
VAR   oldVertex : Vertex;          (*--vertex to deallocate *)

BEGIN
  graphError := noerr;
  IF (theGraph = NullGraph) THEN
    RaiseErrIn(clear, undefined);
  ELSE
    WITH theGraph^ DO
          theVertex := firstVertex;
          WHILE (theVertex # NullVertex) DO
            ClearEdgeRefs(theVertex);
                oldVertex := theVertex;
                theVertex := theVertex^.next;
                Deallocate(oldVertex, SIZE(oldVertex^));
          END (*--while*);
          firstVertex := NullVertex;
          numVertices := 0;
        END (*--with*);
        ClearEdges(theGraph);
  END (*--if*);
END Clear;
(*-------------------------*)

PROCEDURE Assign  (    theGraph      : Graph     (*--in   *);
                                   VAR toGraph       : Graph     (*--inout *));


  PROCEDURE RecreateTarget (): BOOLEAN (*--out  *);
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

          TailInsert(toGraph^.firstVertex, lastVertex);
          INC(toGraph^.numVertices);
          AddToVertexMap(v, newVertex);

          v := v^.next;
        END (*--while*);

        RETURN TRUE;
  END CopyVertices;

  PROCEDURE CopyEdges;

  VAR   theEdge   : EdgePtr; (*--loop index over edges in source graph *)
  VAR   newEdge   : EdgePtr; (*--new edge for target graph *)
  VAR   lastEdge  : EdgePtr; (*--last edge inserted into new list of edges *)
  VAR   epRef1    : Edge;
  VAR   epRef2    : Edge;
  VAR   vertex1   : Vertex; (*--first vertex of theEdge in target graph *)
  VAR   vertex2   : Vertex; (*--second vertex of theEdge in target graph *)

  BEGIN
        theEdge    := theGraph^.firstEdge;
        lastEdge   := NullRef;
        WHILE (theEdge # NullRef) DO
          vertex1 := VertexInMap(theEdge^.first);
          vertex2 := VertexInMap(theEdge^.second);
          newEdge := NewEdge(vertex1,
                                                 vertex2,
                                                 theEdge^.weight,
                                                 assign);
          IF (newEdge = NullRef) THEN
                RETURN;
          END (*--if*);
          epRef1 := NewEdgeRef(newEdge, assign);
          IF (epRef1 = NullEdge) THEN
            Deallocate(newEdge, SIZE(newEdge^));
            RETURN;
          END (*--if*);
          newEdge^.edgeRef1 := epRef1;

          IF (vertex1 # vertex2) THEN
            epRef2 := NewEdgeRef(newEdge, assign);
            IF (epRef2 = NullEdge) THEN
              Deallocate(newEdge, SIZE(newEdge^));
              Deallocate(epRef1, SIZE(epRef1^));
              RETURN;
            END (*--if*);
            newEdge^.edgeRef2 := epRef2;
            AddEdgeRef(vertex2, epRef2);
      END (*--if*);

          AddEdgeRef(vertex1, epRef1);
          AddEdge(toGraph, newEdge);

          theEdge := theEdge^.next;
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


PROCEDURE Insert   (VAR theGraph   : Graph     (*--inout *);
                                        theItem    : Label     (*--in    *);
                                        VAR theVertex  : Vertex    (*--out   *));
BEGIN
  graphError := noerr;
  theVertex  := NullVertex;

  IF (theGraph = NullGraph) THEN
    RaiseErrIn(insert, undefined);
  ELSE
    theVertex := NewVertex(theGraph, theItem, insert);
        IF (theVertex # NullVertex) THEN
      AddVertex(theGraph, theVertex);
    END (*--if*);
  END (*--if*);
END Insert;
(*-------------------------*)


PROCEDURE Remove   (VAR theGraph   : Graph     (*--inout*);
                                    VAR theVertex  : Vertex    (*--inout*));


  PROCEDURE OtherVertex (    theVertex : Vertex (*--in   *);
                                                     theEdge   : Edge   (*--in   *))
                                                                           : Vertex (*--out  *);
  BEGIN
    WITH theEdge^.realEdge^ DO
          IF (first = theVertex) THEN
            RETURN second;
          ELSE
            RETURN first;
          END (*--if*);
    END (*--with*);
  END OtherVertex;

  PROCEDURE OtherEdge (    theEdge : Edge (*--in   *))
                                                                   : Edge (*--out  *);
  BEGIN
    WITH theEdge^.realEdge^ DO
          IF (edgeRef1 = theEdge) THEN
            RETURN edgeRef2;
          ELSE
            RETURN edgeRef1;
          END (*--if*);
    END (*--with*);
  END OtherEdge;

VAR   anEdge  : Edge;   (*--loop index over edges *)
VAR   vertex2 : Vertex; (*--other endpoint in anEdge *)
VAR   edge2   : Edge;   (*--edge in vertex2's edge list *)

BEGIN
  graphError := noerr;
  IF (theGraph = NullGraph) THEN
    RaiseErrIn(remove, undefined);

  ELSIF (theVertex = NullVertex) THEN
    RaiseErrIn(remove, nullvertex);

  ELSIF (theVertex^.inGraph # theGraph) THEN
    RaiseErrIn(remove, novertex);

  ELSE
    anEdge := theVertex^.edges;
        WHILE (anEdge # NullEdge) DO
          vertex2 := OtherVertex(theVertex, anEdge);
          edge2   := OtherEdge(anEdge);
          FreeEdgeRef(vertex2, edge2);
          FreeEdge(theGraph, anEdge^.realEdge);
          anEdge := anEdge^.next;
        END (*--while*);
        ClearEdgeRefs(theVertex);
        FreeVertex(theGraph, theVertex);
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


PROCEDURE Link         (VAR theGraph   : Graph     (*--inout*);
                                            endpoint1  : Vertex    (*--in   *);
                                            endpoint2  : Vertex    (*--in   *);
                                            theWeight  : Attribute (*--in   *);
                                            VAR theEdge    : Edge      (*--out  *));

VAR   newEdgePtr : EdgePtr;
VAR   newEdge    : Edge;    (*-- edge ref for {ep2, ep1} *)

BEGIN
  graphError := noerr;
  theEdge    := NullEdge;

  IF (theGraph = NullGraph) THEN
    RaiseErrIn(link, undefined);
  ELSIF (endpoint1 = NullVertex) OR (endpoint2 = NullVertex) THEN
    RaiseErrIn(link, nullvertex);
  ELSIF (endpoint1^.inGraph # theGraph) OR
                (endpoint2^.inGraph # theGraph) THEN
    RaiseErrIn(link, novertex);
  ELSE

    newEdgePtr := NewEdge(endpoint1, endpoint2, theWeight, link);
        IF (newEdgePtr = NullRef) THEN
          RETURN;
        END (*--if*);

        theEdge := NewEdgeRef(newEdgePtr, link);
        IF (theEdge = NullEdge) THEN
          Deallocate(newEdgePtr, SIZE(newEdgePtr^));
          RETURN;
        END (*--if*);
        newEdgePtr^.edgeRef1 := theEdge;

        IF (endpoint1 # endpoint2) THEN
          newEdge := NewEdgeRef(newEdgePtr, link);
          IF (newEdge = NullEdge) THEN
            Deallocate(newEdgePtr, SIZE(newEdgePtr^));
            Deallocate(theEdge, SIZE(theEdge^));
            RETURN;
          END (*--if*);
          newEdgePtr^.edgeRef2 := newEdge;
          AddEdgeRef(endpoint2, newEdge);
    END (*--if*);

        AddEdge(theGraph, newEdgePtr);
        AddEdgeRef(endpoint1, theEdge);
  END (*--if*);
END Link;
(*-------------------------*)

PROCEDURE Unlink       (VAR theGraph   : Graph     (*--inout*);
                                        VAR theEdge    : Edge      (*--inout*));

VAR   theRealEdge  : EdgePtr;

BEGIN
  graphError := noerr;

  IF (theGraph = NullGraph) THEN
    RaiseErrIn(unlink, undefined);

  ELSIF (theEdge = NullEdge) THEN
        RaiseErrIn(unlink, nulledge);

  ELSIF (theEdge^.realEdge^.first = NullVertex) THEN
    RaiseErrIn(unlink, nullvertex);

  ELSIF (theEdge^.realEdge^.first^.inGraph # theGraph) THEN
    RaiseErrIn(unlink, noedge);

  ELSE
        theRealEdge := theEdge^.realEdge;
    WITH theRealEdge^ DO
          FreeEdgeRef(first, edgeRef1);
          IF (edgeRef2 # NullEdge) THEN
            FreeEdgeRef(second, edgeRef2);
          END (*--if*);
        END (*--with*);
        FreeEdge(theGraph, theRealEdge);
        theEdge := NullEdge;
  END (*--if*);
END Unlink;
(*-------------------------*)

PROCEDURE SetAttribute (    theEdge    : Edge      (*--inout*);
                                            theWeight  : Attribute (*--in   *));
BEGIN
  graphError := noerr;
  IF (theEdge = NullEdge) THEN
        RaiseErrIn(setattr, nulledge);
  ELSE
    WITH theEdge^ DO
      realEdge^.weight := theWeight;
    END (*--with*);
  END (*--if*);
END SetAttribute;
(*-------------------------*)


PROCEDURE IsDefined (    theGraph  : Graph       (*--in   *))
                                                                   : BOOLEAN     (*--out  *);
BEGIN
  RETURN (theGraph # NullGraph);
END IsDefined;
(*-------------------------*)

PROCEDURE IsEmpty   (    theGraph  : Graph       (*--in   *))
                                                                   : BOOLEAN     (*--out  *);
BEGIN
  graphError := noerr;
  IF (theGraph = NullGraph) THEN
    RaiseErrIn(isempty, undefined);
        RETURN TRUE;
  END (*--if*);
  RETURN theGraph^.numVertices = 0;
END IsEmpty;
(*-------------------------*)

PROCEDURE OrderOf   (    theGraph  : Graph       (*--in   *))
                                                                   : CARDINAL    (*--out  *);
BEGIN
  graphError := noerr;
  IF (theGraph = NullGraph) THEN
    RaiseErrIn(orderof, undefined);
        RETURN 0;
  END (*--if*);
  RETURN theGraph^.numVertices;
END OrderOf;
(*-------------------------*)

PROCEDURE SizeOf    (    theGraph  : Graph       (*--in   *))
                                                                   : CARDINAL    (*--out  *);
BEGIN
  graphError := noerr;
  IF (theGraph = NullGraph) THEN
    RaiseErrIn(sizeof, undefined);
        RETURN 0;
  END (*--if*);
  RETURN theGraph^.numEdges;
END SizeOf;
(*-------------------------*)


PROCEDURE DegreeOf  (    theVertex : Vertex      (*--in   *))
                                                                   : CARDINAL    (*--out  *);
BEGIN
  graphError := noerr;
  IF  (theVertex = NullVertex) THEN
    RaiseErrIn(degreeof, nullvertex);
    RETURN 0;
  END (*--if*);
  RETURN theVertex^.degree;
END DegreeOf;
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
    RETURN NullGraph;
  END (*--if*);
  RETURN theVertex^.inGraph;
END GraphOf;
(*-------------------------*)


PROCEDURE AttributeOf (    theEdge   : Edge        (*--in   *))
                                                                 : Attribute   (*--out  *);
BEGIN
  graphError := noerr;
  IF (theEdge = NullEdge) THEN
    RaiseErrIn(attrof, nulledge);
  ELSE
    RETURN theEdge^.realEdge^.weight;
  END (*--if*);
  RETURN NullItem;
END AttributeOf;
(*-------------------------*)

PROCEDURE FirstOf        (    theEdge   : Edge       (*--in  *))
                                                                : Vertex     (*--out *);
BEGIN
  graphError := noerr;
  IF (theEdge = NullEdge) THEN
    RaiseErrIn(firstof, nulledge);
  ELSE
    RETURN theEdge^.realEdge^.first;
  END (*--if*);
  RETURN NullVertex;
END FirstOf;
(*-------------------------*)

PROCEDURE SecondOf   (    theEdge   : Edge       (*--in  *))
                                                                : Vertex     (*--out *);
BEGIN
  graphError := noerr;
  IF (theEdge = NullEdge) THEN
    RaiseErrIn(secondof, nulledge);
  ELSE
    RETURN theEdge^.realEdge^.second;
  END (*--if*);
  RETURN NullVertex;
END SecondOf;
(*-------------------------*)

PROCEDURE IncidentOn  (    theEdge   : Edge       (*--in   *);
                                           VAR endpoint1 : Vertex         (*--out  *);
                                           VAR endpoint2 : Vertex         (*--out  *));
BEGIN
  graphError := noerr;
  IF (theEdge = NullEdge) THEN
    RaiseErrIn(incidenton, nulledge);
    endpoint1 := NullVertex;
    endpoint2 := NullVertex;
  ELSE
    WITH theEdge^.realEdge^ DO
      endpoint1 := first;
      endpoint2 := second;
    END (*--with*);
  END (*--if*);
END IncidentOn;
(*-------------------------*)

PROCEDURE IsEdge     (    theGraph  : Graph      (*--in *);
                                              theEdge   : Edge       (*--in *))
                                                            : BOOLEAN    (*--out *);
BEGIN
  graphError := noerr;
  IF (theGraph = NullGraph) THEN
    RaiseErrIn(isedge, undefined);
  ELSIF (theEdge = NullEdge) THEN
    RaiseErrIn(isedge, nulledge);
  ELSIF (theEdge^.realEdge^.first = NullVertex) THEN
    RaiseErrIn(isedge, nullvertex);
  ELSE
    RETURN theEdge^.realEdge^.first^.inGraph = theGraph;
  END (*--if*);
  RETURN FALSE;
END IsEdge;
(*-------------------------*)


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

VAR   theEdge : EdgePtr; (*--loop index over edges of a graph *)

BEGIN
  graphError := noerr;
  IF (theGraph = NullGraph) THEN
    RaiseErrIn(loopedges, undefined);
  ELSE
        theEdge := theGraph^.firstEdge;
        WHILE (theEdge # NullRef) & process(theEdge^.edgeRef1) DO
          theEdge := theEdge^.next;
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

VAR   theEdge : EdgePtr;   (*--loop index over edges *)

BEGIN
  graphError := noerr;
  IF (theGraph = NullGraph) THEN
    RaiseErrIn(travedges, undefined);
  ELSE
        theEdge := theGraph^.firstEdge;
        WHILE (theEdge # NullRef) DO
          process(theEdge^.edgeRef1);
          theEdge := theEdge^.next;
        END (*--while*);
  END (*--if*);
END TravEdges;
(*-------------------------*)

PROCEDURE Iterate      (    theVertex : Vertex     (*--in   *);
                                                        process   : EdgeProc   (*--in   *));

VAR   theEdge : Edge; (*--loop index over edges of the vertex *)

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


PROCEDURE FirstVertex  (    theGraph  : Graph      (*--in   *))
                                                                          : Vertex     (*--out  *);
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

PROCEDURE NextVertex   (    theVertex : Vertex     (*--in   *))
                                                                          : Vertex     (*--out  *);
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


BEGIN
  FOR graphError := MIN(Exceptions) TO MAX(Exceptions) DO
    SetHandler(graphError, NullHandler);
  END (*--for*);
  SetHandler(noerr, NullHandler);
  graphError := noerr;
        NullGraph := NIL;
        NullVertex := NIL;
        NullEdge := NIL;
END GrafSUUI.
