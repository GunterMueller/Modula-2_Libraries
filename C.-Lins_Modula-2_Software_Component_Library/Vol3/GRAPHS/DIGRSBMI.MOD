(*
11.2    Digraph - Sequential Bounded Managed Iterator

In this section we provide the implementation module corresponding to
the interface given above in 11.1. The following scheme is used in
organizing this section:
* 11.2.1  Internal Representation
* 11.2.2  Exception Handling
* 11.2.3  Local Routines
* 11.2.4  Graph Constructors
* 11.2.5  Vertex Constructors
* 11.2.6  Edge Constructors
* 11.2.7  Graph Selectors
* 11.2.8  Vertex Selectors
* 11.2.9  Edge Selectors
* 11.2.10 Passive Iterators
* 11.2.11 Active Iterators
* 11.2.12 Module Initialization
*)

IMPLEMENTATION MODULE DigrSBMI;
(*==============================================================
    Version  : V2.01  08 December 1989.
    Compiler : JPI TopSpeed Modula-2
    Code size: 6300 bytes
    Component: Digraph - Sequential Bounded Managed Iterator

    THE ABSTRACTION
    This module provides interface definitions for the directed,
        bounded, unmanaged, iterator graph abstract data type.

    REVISION HISTORY
    v1.00  26 May 1988  C. Lins:
        Initial TML Modula-2 implementation
    v1.01  11-14 Jan 1989  C. Lins:
        Revised interface in conformance with new graph spec (Ch 9).
        Changed implementation to represent vertex abstractly.
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

        (C) Copyright 1989 Charles A. Lins
==============================================================*)

FROM SYSTEM IMPORT
        (*--type*) ADDRESS,
        (*--proc*) ADR;

FROM JPIStorage IMPORT
        (*--proc*) Allocate, Deallocate;

FROM Items IMPORT
        (*--cons*) NullItem,
        (*--type*) Item, AssignProc, DisposeProc;

FROM GraphTypes IMPORT
        (*--type*) Operations, Exceptions, ComponentID;

FROM ErrorHandling IMPORT
        (*--cons*) NullHandler,
        (*--type*) HandlerProc,
        (*--proc*) Raise;

FROM TypeManager IMPORT
        (*--cons*) NullType,
        (*--type*) TypeID,
        (*--proc*) AssignOf, DisposeOf;

        (*-------------------------*)

(*
11.2.1  Internal Representation

The internal representation for a bounded digraph is similar to that used in
the previous chapter for its unbounded counterpart. The main difference is in
the adjacency list representation. We still have a singly linked list
of vertices, the difference being that the list is stored in an array of
vertex nodes. Thus, a Vertex could be represented using an array index. This
was not done for several reasons:
        1. because the TML Modula-2 compiler prohibits redefinition of an
           opaque type as equal to an already defined type (TYPE Vertex = CARDINAL
           is flagged as an error);
        2. making the definition of Vertex transparent in the Definition Module
           exposes to clients an aspect of the implementation (violating information
           hiding principles); and
        3. transformation of the unbounded form into the bounded form, as was done
           here, is facilitated by taking another approach.

The approach used in this module takes advantage of the fact than an element
of an array can be accessed through its ADDRESS. So internally the module can
use indices into the adjacency list where necessary while still maintaining
an abstract presentation to client modules.

This organization is shown below in Figure 11.1.

Figure 11.1

Edge:                   completes the opaque definition as a reference to a dynamically
                                allocated edge node.

Vertex:                 completes the opaque definition as the address of a vertex node
                                in the adjacency list array.

VertexNode:             defines the information requirements for a single vertex of a
                                graph.
        data:           contains the label data item associated with a vertex.
        next:           index of the next vertex in the set of vertices for a graph. The
                                last vertex of the list has a 'next' of 0 indicating
                                the end of the list.
        edges:          link to the first directed edge leaving this vertex. If the vertex
                                has no edges leaving it, this field is set to the NullEdge.
        indegree:       is used in maintaining a count of the number of edges having this
                                vertex as its destination excluding self-loops (which are edges
                                whose source and destination vertices are the same). This count
                                is maintained by the constructors Link, Unlink and Assign. It
                                is used by the constructor Remove in detecting attempts to
                                delete vertices referenced as the destination of an edge. Since
                                the alternative technique for detecting this precondition is
                                a complete traversal of all the edges of a graph, we have chosen
                                to record this information incrementally as edges are inserted
                                and removed from the graph.
        inGraph:        contains the reference to the enclosing graph object for the vertex.
                                This avoids having the graph as a parameter to the vertex selectors.
                                In addition, simplifying the membership test between a vertex and a
                                graph, as well as the membership test for edges (through the
                                initial or final vertex references of the edge).

EdgeNode:               defines the information requirements for a single edge of a graph.
        initial:        contains a reference to the initial (or source) vertex of the edge.
        final:          contains a reference to the final (or destination) vertex of the edge.
        weight:         contains the attribute of the edge.
        next:           contains the link to the next edge leaving the initial vertex.
                                The last edge of this list contains the NullEdge as its value
                                indicating the end of the list.

UnboundedGraph: describes (and holds) attributes of the graph itself.
        labelType,
        attrType:       contain the data type ID for the vertex label and edge attribute,
                                respectively. These two fields are used to retrieve the procedures
                                accomplishing assignment and disposal of data items.
        numVertices,
        numEdges:       contain counts of the total number of vertices and edges in the
                                graph, respectively. Thus, the selectors OrderOf and SizeOf are
                                O(1) algorithms instead of O(|V|) or O(|E|).
        firstVertex: index of the first vertex in the adjacency list for a graph.
        available:      index of the first available vertex in the adjacency list of a graph.
        vertices:       the adjacency list of vertices. When a graph is initially created
                                the array is sized based on the maximum number of vertices desired. So
                                while the upper bound on the VertexIndex is MaxVertex elements, the
                                actual size of the array will be something less than this.
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
                labelType  : TypeID;    (*-- vertex label data type ID *)
                attrType   : TypeID;    (*-- edge attribute data type ID *)
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
11.2.2  Exception Handling

graphError holds the exception result from the most recently
invoked operation of this module. The Exceptions enumeration
constant noerr indicates successful completion of the operation and
all operations that may raise an exception assign this value to
graphError before any other processing.

The handlers array holds the current exception handler for the
possible exceptions that may be raised from within this module.

Both are initialized by the module initialization (see 10.3.12).

GraphError      simply returns the current exception result stored
                        in graphError and is used to determine whether a graph
                        operation completed successfully.

SetHandler      makes theHandler the current exception handler for theError
                        by storing theHandler in the handler array.

GetHandler      returns the current exception handler for theError from the
                        handler array.
*)

VAR   graphError : Exceptions;
VAR   handlers   : ARRAY Exceptions OF HandlerProc;

PROCEDURE GraphError () : Exceptions               (*--out  *);
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

(*
11.2.3  Local Routines

FreeAttribute is responsible for retrieval of the edge attribute item disposal
routine and freeing the attribute when no longer needed. This occurs when
        1. a graph is cleared or destroyed (Clear);
        2. an edge is removed from a graph (Unlink);
        3. a vertex is removed from a graph and any edges leaving it are implicitly
           removed (ClearEdges); or
        4. a new attribute is assigned to an edge (SetAttribute).
Complexity: O(1).
*)

PROCEDURE FreeAttribute (    theEdge : Edge (*--inout*));

VAR   free : DisposeProc; (*-- attribute disposal routine, if any *)

BEGIN
  WITH theEdge^ DO
        free := DisposeOf(initial^.inGraph^.attrType);
        free(weight);
  END (*--with*);
END FreeAttribute;
(*-------------------------*)

(*
FreeLabel corresponds to FreeAttribute, above, for the clean-up of vertex labels
when they are no longer needed. The conditions are similar to those above:
        1. a graph is cleared or destroyed (Clear);
        2. an vertex is removed from a graph (Remove); or
        3. a new label is assigned to a vertex (SetLabel).
Complexity: O(1).
*)

PROCEDURE FreeLabel (    theVertex : Vertex (*--inout*));

VAR   free : DisposeProc; (*-- label disposal routine, if any *)

BEGIN
  WITH theVertex^ DO
        free := DisposeOf(inGraph^.labelType);
        free(data);
  END (*--with*);
END FreeLabel;
(*-------------------------*)

(*
InitVertex initializes a single vertex for the free list. O(1)
InitFreeList initializes the free list of available vertices. The free list
of vertices is initialized when a graph is created or when a graph is cleared
of its contents. O(s), where s is the size of the bounded array of vertices.
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

(*
ClearEdges removes all edges leaving a given vertex. This is necessary when
        1. a graph is cleared or destroyed (Clear); or
        2. a vertex is removed from a graph (Remove).
Essentially, the algorithm loops over each edge in the edge list for the vertex
removing the edge from the list, updating the number of edges in the graph,
deallocation of the edge attribute and, finally, deallocation of the edge itself.
Since we guarantee the last edge in the list has a 'next' of NullEdge, 'edges'
(the vertex's link to the first edge) is properly set to the NullEdge upon
completion of the routine. O(outdegree(v)).
*)

PROCEDURE ClearEdges    (    theVertex: Vertex   (*--inout*));

VAR   theEdge : Edge;            (*-- edge to be removed *)

BEGIN
  WITH theVertex^ DO
        WHILE (edges # NullEdge) DO
          theEdge := edges;
          edges := edges^.next;
          DEC(inGraph^.numEdges);
          FreeAttribute(theEdge);
          Deallocate(theEdge, SIZE(theEdge^));
        END (*--while*);
  END (*--with*);
END ClearEdges;
(*-------------------------*)

(*
NewEdge simple creates a new edge with the specified vertex endpoints and weight.
The edge is not added to any edge list, leaving this to the caller. The overflow
exception is automatically raised, if necessary, when a new edge node cannot be
allocated. The structural sharing of the edge attribute is controlled, as described
above for NewVertex, using a similar mechanism. Complexity O(1).
*)

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
11.2.4  Graph Constructors

Create attempts to form a new, empty graph object with the given maximum
number of vertices (theSize) and vertex label and edge label data types.
First, the graph descriptor is allocated, the free list of vertices is
initialized, and the vertex and edge data type IDs are stored in the graph
descriptor. The number of vertices and edges are initialized to zero.
If the descriptor allocation fails the overflow exception is raised and the
NullGraph is returned, otherwise we return the newly allocated graph.
Complexity O(theSize) due to the initialization of the free list of vertices.
*)

CONST baseSize = SIZE(BoundedGraph) - SIZE(AdjList);
CONST nodeSize = SIZE(VertexNode);

PROCEDURE Create   (    labels     : TypeID    (*--in   *);
                                            attributes : TypeID    (*--in   *);
                                            theSize    : CARDINAL  (*--in   *))
                                                                   : Graph     (*--out  *);

VAR   newGraph : Graph; (*-- temporary for new graph object *)

BEGIN
  graphError := noerr;
  Allocate(newGraph, baseSize + (VAL(INTEGER, theSize) * nodeSize));
  IF (newGraph = NullGraph) THEN
    RaiseErrIn(create, overflow);
  ELSE
    WITH newGraph^ DO
          labelType   := labels;
          attrType    := attributes;
          maxVertices := theSize;
          numEdges    := 0;
        END (*--with*);
        InitFreeList(newGraph);
  END (*--if*);
  RETURN newGraph;
END Create;
(*-------------------------*)

(*
Destroy clears theGraph and then deallocates it making theGraph undefined.
SCLStorage.Deallocate automatically releases the proper amount of space
originally allocated and alters the pointer to NIL (which is also the
value of the NullGraph). Complexity O(v+e).
*)

PROCEDURE Destroy (VAR theGraph      : Graph     (*--inout*));
BEGIN
  Clear(theGraph);
  IF (graphError = noerr) THEN
    Deallocate(theGraph, baseSize + (theGraph^.maxVertices * nodeSize));
  END (*--if*);
END Destroy;
(*-------------------------*)

(*
Clear removes all vertices and edges from theGraph making theGraph empty.
We do this by iterating over each of the vertices and clearing all edges
leaving the vertex (ClearEdges). As a final step we ensure that the graph is left in the
empty state by resetting the head of the adjacency list to NIL and the number
of vertices and edges in the graph to zero. The free list of available
vertices is also reinitialized. Complexity O(v+e).
*)

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
                FreeLabel(ADR(vertices[theVertex]));
                theVertex := vertices[theVertex].next;
          END (*--for*);
        END (*--with*);
        InitFreeList(theGraph);
  END (*--if*);
END Clear;
(*-------------------------*)

(*
Assign copies the source graph, theGraph, to the target graph, toGraph. The
main body of Assign does this by first copying all the vertices followed by
copying all the edges from the source to the destination graph.
The algorithmic complexity is O(v^2+e) due to the mapping between the vertices
of the source and target graphs while copying the edges (see the discussion
of the vertexMap following RecreateTarget).
*)

PROCEDURE Assign  (    theGraph      : Graph     (*--in   *);
                                   VAR toGraph       : Graph     (*--inout*));

(*
RecreateTarget reconstructs the target graph descriptor so that the fields
defining the vertex label and edge attribute data types,
and (optionally) maximum number of vertices between the
source and destination graphs are the same. If the source and destination
graphs are the same the routine returns FALSE indicating that the postconditions
for the assignment operation are already met.
The routine returns TRUE if the recreation of the target was successful.
Complexity O(v+e) where v and e are the number of vertices and
edges, respectively, in the original toGraph.  When Clearing the target graph
is unnecessary (the toGraph is initially the NullGraph) the complexity falls
to O(1)..
*)

  PROCEDURE RecreateTarget (): BOOLEAN (*--out  *);
  BEGIN
        IF (theGraph = NullGraph) THEN
      RaiseErrIn(assign, undefined);

        ELSIF (toGraph = NullGraph) THEN
      WITH theGraph^ DO
        toGraph := Create(labelType, attrType, maxVertices);
          END (*--with*);

        ELSIF (theGraph = toGraph) THEN
      RETURN FALSE;

        ELSIF (theGraph^.numVertices > toGraph^.maxVertices) THEN
          RaiseErrIn(assign, overflow);

        ELSE
          Clear(toGraph);
      WITH theGraph^ DO
                toGraph^.labelType := labelType;
                toGraph^.attrType  := attrType;
          END (*--with*);
        END (*--if*);
        RETURN (graphError = noerr);
  END RecreateTarget;

(*
One thorny issue in graph assignment is how to set-up the copied edges with the
proper initial and final vertices? The edges of the source graph contain references
to the source graph's vertices, not those of the target graph. The vertex
labels cannot be used since more than one vertex can have the same label. In
this case, an edge from the second (or greater) such vertex in the target graph
would be linked incorrectly to the first vertex having that label. The solution
is in having some form of temporary mapping from the source graph's vertices to
their counterpart in the target graph. The necessary operations are add a mapping
between a vertex from the source graph and its corresponding vertex in the
target graph, and given a source graph vertex return the target graph vertex
mapped to that source vertex.

The data structure implementing our vertex mapping is an unordered array of mapping
entries, one per vertex, between the vertices of the source graph and the
target graph. This array is dynamically created on the heap based on the
number of vertices in the source graph. (The ARRAY [0..0] OF ð construct is
a special feature of the TML Modula-2 compiler allowing dynamic arrays.) The
variable mapExtent controls where MapVertex entries are inserted into the
array. A post-increment scheme is used so mapVertex is always one greater than
the number of entries stored in the array.
*)

  TYPE  MapVertex = RECORD
              old : Vertex; (*-- vertex from source graph *)
              new : Vertex; (*-- corresponging vertex in target graph *)
            END (*--MapVertex*);
  TYPE  MapVertices = ARRAY [0..0] OF MapVertex;

  VAR   vertexMap : POINTER TO MapVertices;
  VAR   mapExtent : CARDINAL;


(*
CreateVertexMap allocates a dynamic array of vertex mapping entries on the
heap based on the number of vertices in the source graph. vertexMap is set to
NIL by Allocate if there isn't enough memory available to meet the request.
*)

  PROCEDURE CreateVertexMap;
  BEGIN
        Allocate(vertexMap,
                         VAL(CARDINAL, SIZE(MapVertex)) * theGraph^.numVertices);
        mapExtent := 0;
  END CreateVertexMap;

(*
AddVertexToMap adds a mapping between the vertices of the source and target graphs.
*)

  PROCEDURE AddToVertexMap (    oldVertex : Vertex (*--in   *);
                                                                newVertex : Vertex (*--in   *));
  BEGIN
        WITH vertexMap^[mapExtent] DO
          old := oldVertex;
          new := newVertex;
        END (*--with*);
    INC(mapExtent);
  END AddToVertexMap;

(*
VertexInMap returns the mapping between the vertices of the source and target
graphs. Since every vertex is represented failure to find a mapping is
indicative of either a programming error in CopyVertices or a hardware/system
software error at runtime.
*)

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

(*
DestroyVertexMap frees up the memory used by the vertexMap when the Assign
operation is complete. Remember that Deallocate automatically
releases the proper amount of space.
*)

  PROCEDURE DestroyVertexMap;
  BEGIN
        Deallocate(vertexMap, SIZE(vertexMap^));
  END DestroyVertexMap;

(*
CopyVertices duplicates the vertices from the source graph to the destination
graph returning TRUE if every vertex was successfully copied and FALSE otherwise.
This BOOLEAN result is used by the main body of Assign to control whether to
continue with the Assign operation by copying the edges. The following local
variables are used:
        1. v:                   indicates the current vertex being copied from the source
                                        graph. This is also used as a 'loop index' over the vertices
                                        from the source graph.
        2. newVertex:   temporary for a new vertex for the destination graph.
        3. lastVertex:  last vertex inserted into the destination graph. This is
                                        used by TailInsert to add a new vertex to the end of the
                                        destination graph's adjacency list.
        4. assignItem:  vertex label assignment routine.

Assignment of the vertex label presents an interesting situation. When a vertex
is added to a graph, the client module expects the given label to be copied using
the Modula-2 assignment statement (even for dynamically allocated data items)
since we simply need to store the value in the vertex object. This is known as
'structural sharing'. But when a graph is duplicated using Assign, new copies
of the vertex labels are necessary - avoiding the problems presented by structural
sharing of dynamically allocated items as described in Volume 1. CopyVertices
resolves this through the assignment procedure associated with the graph's label
data type duplicating the label as a NewVertex is created. Complexity O(v) where
v is the number of vertices in the source graph.
*)

  PROCEDURE CopyVertices () : BOOLEAN;

  VAR   v : CARDINAL;   (*--loop index over vertices being copied *)
  VAR   theVertex : CARDINAL; (*-- index to new vertex *)
  VAR   lastVertex: CARDINAL; (*--last vertex added to list of vertices *)
  VAR   assignItem: AssignProc;

(*
TailInsert adds newVertex to the end of the target graph's adjacency list
given pointers to the first and last elements of the list. Complexity O(1).
*)

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

        assignItem := AssignOf(theGraph^.labelType);
        v := theGraph^.firstVertex;
        lastVertex := 0;

    WHILE (v # 0) DO
          WITH toGraph^ DO
            theVertex := available;
            available := vertices[available].next;

            WITH vertices[theVertex] DO
              inGraph  := toGraph;
                  data     := assignItem(theGraph^.vertices[v].data);
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


(*
CopyEdges iterates over the edges of the source graph through the adjacency
list of vertices and the edge lists for each vertex. As each edge in the
source graph is encountered, a new edge is constructed in the target graph.
This edge is then added to the target graph's vertex equivalent to the edges'
initial vertex. The initial and final vertices of the new edge are retrieved
from the vertex map created by CopyVertices. Note that NewEdge will raise the
overflow exception if necessary.

The following local variables are used:
        v:                      indicates the current vertex from the source graph whose
                                edges are being copied. This is also used as a 'loop index'
                                over the vertices of the source graph.
        e:                      indicates the current edge of the source graph being copied.
                                Also used as a 'loop index' over the edges leaving each
                                vertex, v.
        fromVertex: the vertex of the target graph corresponding to 'v' in the
                                source graph. Since all edges leaving any given vertex, v,
                                have have 'v' as their initial vertex, fromVertex can be
                                derived from the current vertex of the source graph rather than
                                repeatedly retrieving it from the initial vertex of the edge.
        newEdge:        temporary for a new edge in the target graph.
        lastEdge:       last edge inserted into the edge list of the current vertex
                                (fromVertex) destination graph. This is used by TailInsert
                                to add a new edge to the end of fromVertex's edge list.
        assignItem:     edge attribute assignment routine.

So while the basic loop over the edges is linear with respect to the number of
edges, the linear search of the vertex map nested within the loop over the
source graph's vertices gives this algorithm time complexity O(v^2). A better
mapping algorithm (i.e, one of constant time O(1)) would yield a linear time
algorithm overall for the graph assignment.
*)

  PROCEDURE CopyEdges;

  VAR   v : CARDINAL;             (*--loop index over vertices *)
  VAR   e : Edge;                 (*--loop index over edges being copied *)
  VAR   fromVertex: Vertex;   (*--vertex in target graph *)
  VAR   newEdge   : Edge;         (*--new edge for target graph *)
  VAR   lastEdge  : Edge;         (*--last edge inserted into new list of edges *)
  VAR   assignItem: AssignProc; (*--attribute assignment procedure *)

(*
TailInsert adds newEdge to the end of the edge list given pointers to the first
and last elements of the list. Complexity O(1)
*)

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
        assignItem := AssignOf(theGraph^.attrType);
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
                                                   assignItem(e^.weight),
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
11.2.5  Vertex Constructors

Insert adds a vertex to the given graph labeling the vertex with the
given item.  If there are no available vertices in the free list the
overflow exception is raised and the Insert operation aborted. Complexity O(1).
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

(*
Remove deletes the given vertex from the specified graph. If no such vertex
can be found in the graph the novertex exception is raised and the routine
aborted. Deleting a vertex that is the head of an edge would leave dangling
edges (pointing to a non-existent vertex), and so, if the vertex is referenced
by an edge (other than a self-loop) the exception references is raised and
Remove is aborted. After we have checked that no exceptions can occur we remove
all edges leaving the vertex, remove the vertex from the adjacency list,
release any dynamically allocated memory used by the vertex label, release the
vertex itself, and update the count of vertices in the graph. Complexity
O(v).
*)

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
        FreeLabel(theVertex);

    WITH theGraph^ DO

(*
The following loop searches for the given vertex in the adjacency list array
keeping track of the array index of theVertex's predecessor. This is used
immediately after the loop to update the singly-linked list of vertices for the graph.
*)
      theIndex  := firstVertex;
          priorVertex := 0;
          WHILE (ADR(vertices[theIndex]) # theVertex) DO
            priorVertex := theIndex;
            theIndex    := vertices[theIndex].next;
          END (*--while*);

(*
Remove the vertex from the adjacency list.
*)
          IF (priorVertex = 0) THEN
                firstVertex := vertices[theIndex].next;
          ELSE
                vertices[priorVertex].next := vertices[theIndex].next;
          END (*--if*);

(*
Update the graph component by adding the deleted vertex to the available list and the
count of the number of vertices in the graph. Ensure that the vertex is no longer a
valid member of the graph by clearing the vertexes reference to its enclosing graph.
*)
          vertices[theIndex].next := available;
          vertices[theIndex].inGraph := NullGraph;
          available := theIndex;
          DEC(numVertices);
        END (*--with*);
        theVertex := NullVertex;
  END (*--if*);
END Remove;
(*-------------------------*)

(*
SetLabel assigns a new label to the given vertex of the graph. Prior to
assigning a new vertex label we must release any dynamically allocated memory
used by the old vertex label. Complexity O(1).
*)

PROCEDURE SetLabel (    theVertex  : Vertex    (*--inout*);
                                            theItem    : Label     (*--in   *));
BEGIN
  graphError := noerr;
  IF (theVertex = NullVertex) THEN
    RaiseErrIn(setlabel, nullvertex);
  ELSE
        FreeLabel(theVertex);
    theVertex^.data := theItem;
  END (*--if*);
END SetLabel;
(*-------------------------*)

(*
11.2.6  Edge Constructors

Link adds a directed edge between the from and to vertices labeling the
edge with the given weight attribute. The new edge is linked to the front
of the fromVertex's edge list. Thus, edges appear in reverse order to their
order of insertion. Complexity O(1).
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

(*
Unlink removes a directed edge between the two vertices, fromVertex and
toVertex. Complexity O(d) where d is the out-degree of the fromVertex.
*)

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

(*
The following loop searches for the edge being removed in the list of edges
associated with the edges' initial vertex. In addition, we need the prior
edge in the list for the actual deletion of the edge from the list. This is
a standard singly-linked list technique for the deletion of list nodes.
This loop is guaranteed to succeed as Link ensures that the edge is placed in
the initial vertex's list of edges. Complexity O(outdegree(v)).
*)
        e := theEdge^.initial^.edges;
        f := NullEdge;
        WHILE (e # theEdge) DO
          f := e;
          e := e^.next;
        END (*--while*);

        WITH theEdge^ DO

(*
Update the edge list of the initial vertex by removing the edge.
*)
          IF (f = NullEdge) THEN
            initial^.edges := next;
          ELSE
            f^.next := next;
          END (*--if*);

(*
Self-loops are not included in the final vertex's indegree count since such
edges may be freely unlinked and do not effect the removal of the vertex. In
any case, we must maintain the proper count of edges in the graph. As a final
step, the edge attribute and the edge itself may be deallocated. To prevent
clients from successfully accessing copies of an edge, the 'initial' and 'next'
fields are set to empty (and illegal) values. This prevents any copies from
being considered legal edges of the graph even though they have been deleted.
*)
          IF (initial # final) THEN
            DEC(final^.indegree);
          END (*--if*);
          DEC(initial^.inGraph^.numEdges);

      FreeAttribute(theEdge);
          initial := NullVertex;
          next    := NullEdge;
          Deallocate(theEdge, SIZE(theEdge^));
        END (*--with*);
  END (*--if*);
END Unlink;
(*-------------------------*)

(*
SetAttribute assigns a new edge labelling to the given edge. Prior to assigning
a new edge attribute we must release any dynamically allocated memory
used by the old edge attribute. Complexity O(1).
*)

PROCEDURE SetAttribute  (    theEdge    : Edge      (*--inout*);
                                             theWeight  : Attribute (*--in   *));
BEGIN
  graphError := noerr;
  IF (theEdge = NullEdge) THEN
        RaiseErrIn(setattr, nulledge);
  ELSE
    FreeAttribute(theEdge);
    theEdge^.weight := theWeight;
  END (*--if*);
END SetAttribute;
(*-------------------------*)

(*
11.2.7  Graph Selectors

IsDefined verifies to the best of its ability whether theGraph has been
created and is still an active object. Complexity: O(1).
*)

PROCEDURE IsDefined (    theGraph  : Graph       (*--in   *))
                                                                   : BOOLEAN     (*--out  *);
BEGIN
  RETURN (theGraph # NullGraph);
END IsDefined;
(*-------------------------*)

(*
IsEmpty returns True if theGraph is in the empty state, as indicated by
the number of vertices being zero, and False otherwise.  As per the
specification (–9.3) undefined graphs are considered empty. Complexity: O(1).
*)

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

(*
TypeOf simply returns the vertex label and edge attribute data type IDs for
the given graph. Undefined graphs, as always, raise the undefined exception
and return a reasonable value, in this case the NullType. Complexity O(1).
*)

PROCEDURE TypeOf    (    theGraph  : Graph       (*--in   *);
                                         VAR labelType : TypeID      (*--out  *);
                                         VAR attrType  : TypeID      (*--out  *));
BEGIN
  graphError := noerr;
  IF (theGraph = NullGraph) THEN
    RaiseErrIn(typeof, undefined);
        labelType := NullType;
        attrType  := NullType;
  ELSE
        labelType := theGraph^.labelType;
        attrType  := theGraph^.attrType;
  END (*--if*);
END TypeOf;
(*-------------------------*)

(*
OrderOf returns the number of vertices in the graph, or zero for an undefined
graph. Complexity O(1).
*)

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

(*
OrderOf returns the number of edges in the graph, or zero for an undefined
graph. Complexity O(1).
*)

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

(*
MaxOrderOf retuens the maximum number of vertices for the given graph, or zero
for an undefined graph. Essentially, this is the maximum size of the list of
vertices (both used and available). Complexity O(1).
*)

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
11.2.8  Vertex Selectors

InDegree returns the number of edges entering the given vertex. Complexity O(1).
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

(*
OutDegree returns the number of edges leaving the given vertex. We do this by
simply iterating over the edges of the vertex counting them along the way.
Complexity O(outdegree(v)).
*)

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

(*
LabelOf returns the vertex label associated with the given vertex.
If the vertex is undefined the NullItem is returned. Complexity O(1).
Complexity O(1).
*)

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

(*
Since we have stored a copy of the graph object associated with each vertex
as a field of the vertex itself, IsVertex simply needs to compare the given
graph with its own local state. This saves us from having to search the graph.
Complexity O(1).
*)

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

(*
GraphOf simply returns its copy of the enclosing graph or the NullGraph if the
vertex is undefined. Complexity O(1).
*)

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
11.2.9  Edge Selectors

AttributeOf returns the edge attribute associated with the given edge.
If the edge is undefined the NullItem is returned. Complexity O(1).
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

(*
Given an edge, InitialOf returns the vertex that is the origin of
the directed edge or the NullVertex if the edge is undefined. Complexity O(1).
*)

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

(*
Given an edge, FinalOf returns the vertex that is the destination of
the directed edge or the NullVertex if the edge is undefined. Complexity O(1).
*)

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

(*
IsEdge returns true if there the given directed edge is an edge of the given
graph and false otherwise. An advantage of having each vertex identify its
enclosing graph object is use of this field in testing whether the edge is
part of a specified graph. This saves use from having to search every edge in
the graph. Complexity O(1).
*)

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
11.2.10 Passive Iterators

LoopVertices simply iterates over the vertices of the given graph until every
vertex has been examined or the process procedure parameter returns FALSE,
whichever occurs first. Complexity O(v).
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

(*
LoopEdges loops over the vertices of the given graph to access the edges associated
with each vertex. Once the process procedure parameter returns FALSE, we exit
both WHILE statements through the use of a RETURN which exits the procedure.
Complexity O(v+e), though the v component will dominate the equation if v >> e.
*)

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

(*
LoopIterate simply loops over the edges leaving a specified vertex. Complexity
O(outdegree(v)).
*)

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

(*
TravVertices simply iterates over every vertex in the graph. Complexity O(v).
*)

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

(*
TravEdges simply iterates over every edge in the graph. Since the only way
to get at all the edges is through the vertices, we iterate over all the
vertices and over each edge leaving each vertex. Complexity O(v+e).
*)

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

(*
Iterate simply loops over the edges leaving a specified vertex of a graph.
Complexity O(outdegree(v)).
*)

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
11.2.11 Active Iterators

Each of the active iterators are essentially selectors for the underlying
representation of the adjacency list. FirstVertex returns the abstract vertex
corresponding to the first vertex in the graph's adjacency list. NextVertex
returns the abstract vertex that is the successor to the given vertex in the
graph's adjacency list. FirstEdge returns the edge at the head of the vertexes
list of edges emanating from the given vertex. NextEdge returns the edge
immediately following the given edge in a previosuly given vertexes edge list.
The algorithmic complexity of all four routines is O(1).
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
11.2.12 Module Initialization

The module's local variables are initialized to known states.
graphError is used to fill the handlers array with a routine
that does nothing when an exception is raised (saving the
declaration of a special loop control variable for this purpose).
Applying MIN and MAX to cover all exceptions ensures that this initialization
will be unaffected by any future changes to the number of Exceptions
or their order of declaration within the enumeration.  Since a FOR loop
control variable is undefined following the loop, graphError must be
set to indicate that an error has not yet occurred.
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
END DigrSBMI.
