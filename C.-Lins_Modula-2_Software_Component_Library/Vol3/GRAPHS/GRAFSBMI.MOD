(*
14.2    Graph - Sequential Bounded Managed Iterator

In this section we provide the implementation module corresponding to
the interface given above in –12.1. The following scheme is used in
organizing this section:
* 14.2.1  Internal Representation
* 14.2.2  Exception Handling
* 14.2.3  Local Routines
* 14.2.4  Graph Constructors
* 14.2.5  Vertex Constructors
* 14.2.6  Edge Constructors
* 14.2.7  Graph Selectors
* 14.2.8  Vertex Selectors
* 14.2.9  Edge Selectors
* 14.2.10 Passive Iterators
* 14.2.11 Active Iterators
* 14.2.12 Module Initialization
*)

IMPLEMENTATION MODULE GrafSBMI;
(*==============================================================
    Version  : V2.01  08 December 1989.
    Compiler : JPI TopSpeed Modula-2
    Code size: 7492 bytes
    Component: Graph - Sequential Bounded Managed Iterator

    REVISION HISTORY
    v1.00  17 Feb 1989  C. Lins:
        Initial implementation based on unbounded graph module.
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
14.2.1  Internal Representation

The internal representation of an unbounded directed graph uses a variant of the
adjacency list structure used in Chapter 10 for the directed graph (shown below
in Figure 12.1). In this representation, the set of vertices for a graph is given
by a linear list. Since a bounded graph has a fixed number of vertices (maximum)
the module manages its own 'heap' of vertex nodes. This heap is hidden from
clients through the opaque definition of a vertex as an address which is really
the address of an item in the array.
Secondly, each vertex is associated with the set of undirected edges incident on
the vertex. The set of all edges of the graph is thus the union of all the sets
of edges for each of the graph's vertices.

This is the same conceptual model used in representing the unbounded directed graph
of Chapter 10. In that chapter we were able to easily implement this model directly.
Often, undirected graphs are represented using the directed graph model where each
undirected edge, {v, w}, is presented as two directed edges, (v, w) and (w, v).
Though such an approach could be taken with this implementation, we consider it
unsatisfactory for two reasons:
        1. iteration over the edges would cause each edge to be processed twice making
           the implementation structure visible to client modules; and
        2. the selector SetAttribute would have to deallocate and assign the weight
           attribute in two edge nodes in order to keep them in sync.

The solution used here keeps only a single edge node for each edge while the edge
lists of the vertices contain references to the 'real' edge nodes. These real edges
are stored in a doubly-linked list the head of which is kept in the descriptor of
the graph itself. The edge references are also held in doubly-linked lists and each
vertex has a reference to the head of their list of edges.

_Figure 12.1_

Following the figure is a description of the various TYPEs used for our internal
representation including the component fields of each record structure. One
aspect of this representation that may cause some confusion is that the abstract
edge entity is in reality an edge reference. Since are two of these for each edge,
the reader may wonder which reference is used by client modules for the edge
selectors. This potential problem is resolved by the convention that client modules
are never aware of more than one of the two edge references, the second edge
reference is only used internally. Furthermore, when edges are added to a graph
the reference given back to the client is always for the directed edge reference
(v, w) where v is the first vertex of the pair of vertices to which the edge is
linked.

EdgePtr:                defines a reference to a dynamically allocated undirected edge node.
Edge:                   completes the opaque definition as a reference to a dynamically
                                allocated directed edge node.
Vertex:                 completes the opaque definition as a reference to a dynamically
                                allocated vertex node.

VertexNode:             defines the information requirements for a single vertex of a
                                graph.
        data:           contains the label data item associated with a vertex.
        next:           link to the next vertex in the set of vertices for a graph. The
                                last vertex of the list has a 'next' of NullVertex indicating
                                the end of the list.
        prior:          link to the previous vertex in the set of vertices for a graph. The
                                first vertex of the list has a 'prior' of NullVertex indicating
                                the front of the list.
        edges:          link to the first directed edge leaving this vertex. If the vertex
                                has no edges leaving it, this field is set to the NullEdge.
        degree:         is used in maintaining a count of the number of edges incident on this
                                vertex as its destination excluding self-loops (which are edges
                                whose first and second vertices are the same). This count
                                is maintained by the constructors Link, Unlink and Assign. It
                                is used to make the selector DegreeOf an O(1) algorithm instead
                                of an O(Degree(v)) algorithm. The alternative technique for would be
                                a complete traversal of all the edges associated with a vertex.
        inGraph:        contains the reference to the enclosing graph object for the vertex.
                                This avoids having the graph as a parameter to the vertex selectors.
                                In addition, simplifying the membership test between a vertex and a
                                graph, as well as the membership test for edges (through the
                                initial or final vertex references of the edge).

EdgeNode:               defines the information requirements for an undirected edge of a graph.
        first:          contains a reference to the first vertex of the edge.
        second:         contains a reference to the second vertex of the edge.
        edgeRef1:       contains a reference to the directed edge (first, second) in the
                                first vertexes edge list corresponding to this undirected edge.
        edgeRef2:       contains a reference to the directed edge (second, first) in the
                                second vertexes edge list corresponding to this undirected edge.
        weight:         contains the attribute of the edge.
        next:           contains the link to the next edge in the graph.
                                The last edge of this list contains the NullRef as its value
                                indicating the end of the list.
        prior:          contains the link to the prior edge in the linked list of edges
                                in the graph. The first edge of this list contains the NullRef as
                                its value indicating the front of the list.

EdgeRefNode:    defines the information requirements for a directed edge of a graph.
                                There are two such nodes for every EdgeNode.
        realEdge:       contains a reference to the actual undirected edge corresponding
                                to this directed edge.
        next:           contains a link to the next directed edge in a vertexes edge list.
                                The last edge in an edge list contains a value of NullEdge indicating
                                the end of the list.
        prior:          contains a link to the previous directed edge in a vertexes edge list.
                                The first edge in an edge list contains a value of NullEdge indicating
                                the front of the list.

UnboundedGraph: describes (and holds) attributes of the graph itself.
        labelType,
        attrType:       contain the data type ID for the vertex label and edge attribute,
                                respectively. These two fields are used to retrieve the procedures
                                accomplishing assignment and disposal of data items.
        numVertices,
        numEdges:       contain counts of the total number of vertices and edges in the
                                graph, respectively. Thus, the selectors OrderOf and SizeOf are
                                O(1) algorithms instead of O(|V|) or O(|E|).
        firstVertex: reference to the first vertex in the adjacency list for a graph.
        firstEdge:  reference to the first edge in the set of all edges for a graph.
*)

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

CONST maxVertex   = 1480;
TYPE  VertexIndex = [1..maxVertex];
TYPE  Vertices    = ARRAY VertexIndex OF VertexNode;

TYPE  BoundedGraph = RECORD
                labelType  : TypeID;    (*--vertex label data type ID *)
                attrType   : TypeID;    (*--edge attribute data type ID *)
                maxVertices: CARDINAL;  (*--maximum number of vertices *)
                numVertices: CARDINAL;  (*--current number of vertices *)
                numEdges   : CARDINAL;  (*--current number of edges *)
                firstVertex: Vertex;    (*--first vertex in adjacency list *)
                available  : Vertex;    (*--first vertex in available list *)
                firstEdge  : EdgePtr;   (*--first edge in edge set *)
                vertices   : Vertices;  (*--bounded adjacency list *)
          END (*--BoundedGraph *);

TYPE  Graph = POINTER TO BoundedGraph;

        (*-------------------------*)

(*
14.2.2  Exception Handling

graphError holds the exception result from the most recently
invoked operation of this module. The Exceptions enumeration
constant noerr indicates successful completion of the operation and
all operations that may raise an exception assign this value to
graphError before any other processing. The handlers array holds the
current exception handler for the possible exceptions that may be
raised from within this module. Both are initialized by the module
initialization (see –10.3.12).

GraphError      simply returns the current exception result stored
                        in graphError and is used to determine whether a graph
                        operation completed successfully.
SetHandler      makes theHandler the current exception handler for theError
                        by storing theHandler in the handlers array.
GetHandler      returns the current exception handler for theError from the
                        handlers array.
*)

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

(*
14.2.3  Local Routines

FreeAttribute is responsible for retrieval of the edge attribute item disposal
routine and freeing the attribute when no longer needed. This occurs when
        1. a graph is cleared or destroyed (Clear);
        2. an edge is removed from a graph (Unlink);
        3. a vertex is removed from a graph and any edges leaving it are implicitly
           removed (ClearEdges); or
        4. a new attribute is assigned to an edge (SetAttribute).
Complexity: O(1).
*)

PROCEDURE FreeAttribute (    theEdge : EdgePtr (*--inout*));

VAR   free : DisposeProc; (*-- attribute disposal routine, if any *)

BEGIN
  WITH theEdge^ DO
        free := DisposeOf(first^.inGraph^.attrType);
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
                                                          theNext  : Vertex     (*--in   *));
  BEGIN
    WITH theNode DO
          inGraph  := NullGraph;
          data     := NullItem;
          degree   := 0;
          prior    := NullVertex;
          next     := theNext;
          edges    := NullEdge;
    END (*--with*);
  END InitVertex;

VAR   v : VertexIndex; (*-- running index over vertices of the graph *)

BEGIN
  WITH theGraph^ DO
    FOR v := MIN(VertexIndex) TO maxVertices-1 DO
          InitVertex(vertices[v], ADR(vertices[v+1]));
        END (*--for*);
        InitVertex(vertices[maxVertices], NullVertex);
        numVertices := 0;
        firstVertex := NullVertex;
        available   := ADR(vertices[MIN(VertexIndex)]);
  END (*--with*);
END InitFreeList;
(*-------------------------*)


(*
NewVertex retrievs a new, empty, vertex node from the list of available nodes.
The vertex field
inGraph is set to the proper value (its enclosing graph object) while indegree,
edges, and next are initialized to an empty state. The caller is responsible for
adding the vertex to the adjacency list for the graph. The routine also automatically
raises the overflow exception with the appropriate parameters, if necessary. As
noted in Volume 1, the version of Allocate used here sets theVertex to NIL if the
allocation fails. Thus, we ensure that theVertex returned is the NullVertex in
case of a memory management failure. Complexity O(1).
*)

PROCEDURE NewVertex (    theGraph   : Graph      (*--in   *);
                                                 theItem    : Label      (*--in   *);
                                                 theRoutine : Operations (*--in   *))
                                                                    : Vertex     (*--out  *);

VAR   newVertex : Vertex;               (*-- newly created vertex *)

BEGIN
  IF (theGraph^.numVertices = theGraph^.maxVertices) THEN
        RaiseErrIn(theRoutine, overflow);
  ELSE
        WITH theGraph^ DO
          newVertex := available;
          available := newVertex^.next;
        END (*--with*);
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

(*
AddVertex adds theVertex to theGraph's set of all vertices. The vertex
is placed at the front of the doubly-linked list used to represent this
set. Complexity O(1)
*)

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

(*
FreeVertex removes theVertex from theGraph's set of all vertices.
This routine uses a standard doubly-linked list removal
algorithm giving a constant time complexity of O(1).
*)

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
  FreeLabel(theVertex);
  WITH theGraph^ DO
        DEC(numVertices);
        theVertex^.next := available;
        available := theVertex;
  END (*--with*);
  theVertex := NullVertex;
END FreeVertex;
(*-------------------------*)


(*
NewEdge simply creates a new edge with the specified vertex endpoints and weight.
The edge is not added to any edge list, leaving this to the caller. The overflow
exception is automatically raised, if necessary, when a new edge node cannot be
allocated. Complexity O(1).
*)

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

(*
AddEdge adds theEdge to theGraph's set of all edges. The edge is placed
at the front of the doubly-linked list used to represent this set. Complexity O(1)
*)

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

(*
FreeEdge removes theEdge from theGraph's set of all edges.
This routine uses a standard doubly-linked list removal
algorithm giving a constant time complexity of O(1).
*)

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
  FreeAttribute(theEdge);
  Deallocate(theEdge, SIZE(theEdge^));
END FreeEdge;
(*-------------------------*)


(*
NewRef constructs a new edge reference node. The node is linked to its
associated EdgeNode while the links into a vertexes edge list are set to
the empty state. Complexity O(1).
*)

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

(*
AddEdgeRef adds theEdge to theVertex's set of edges incident on theVertex.
The edge is placed at the front of the doubly-linked list used to
represent this set. Complexity O(1)
*)

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

(*
FreeEdgeRef removes theEdge from theVertex's set of edges incident on
theVertex. This routine uses a standard doubly-linked list removal
algorithm giving a constant time complexity of O(1).
*)

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

(*
ClearEdgeRefs removes all directed edge referencess from a given vertexes edge list.
O(outdegree(v))
*)

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

(*
14.2.4  Graph Constructors

Create attempts to form a new, empty graph object with the given vertex
label and edge label data types. First, the graph descriptor is allocated and the vertex
and edge data type IDs are stored there. The number of vertices and edges are
initialized to zero. The pointer to the head of the adjacency list (firstVertex)
is initialized to the empty state (NIL). If the descriptor allocation
fails the overflow exception is raised and the NullGraph is returned,
otherwise we return the newly allocated graph. Complexity O(1).
*)

CONST baseSize = SIZE(BoundedGraph) - SIZE(Vertices);
CONST nodeSize = SIZE(VertexNode);

PROCEDURE Create   (    labels     : TypeID    (*--in   *);
                                            attributes : TypeID    (*--in   *);
                                            theSize    : CARDINAL  (*--in   *))
                                                                   : Graph     (*--out  *);

VAR   newGraph : Graph; (*--temporary for new graph object *)

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
          numVertices := 0;
          numEdges    := 0;
          firstEdge   := NullRef;
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
value of the NullGraph). Complexity O(|V|+|E|).
*)

PROCEDURE Destroy (VAR theGraph      : Graph     (*--inout *));
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
of vertices and edges in the graph to zero. Complexity O(|V|+|E|).
*)

PROCEDURE Clear   (    theGraph      : Graph    (*--inout *));

  PROCEDURE ClearEdges (VAR theGraph : Graph    (*--inout *));

  VAR   theEdge : EdgePtr; (*--edge to be removed *)

  BEGIN
    WITH theGraph^ DO
          WHILE (firstEdge # NullRef) DO
            theEdge := firstEdge;
            firstEdge := firstEdge^.next;
            DEC(numEdges);
            FreeAttribute(theEdge);
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
        ClearEdges(theGraph);
    WITH theGraph^ DO
          theVertex := firstVertex;
          WHILE (theVertex # NullVertex) DO
            ClearEdgeRefs(theVertex);
                FreeLabel(theVertex);
                oldVertex := theVertex;
                theVertex := theVertex^.next;
                oldVertex^.next := available;
                available := oldVertex;
                oldVertex^.inGraph := NullGraph;
          END (*--while*);
          firstVertex := NullVertex;
          numVertices := 0;
        END (*--with*);
  END (*--if*);
END Clear;
(*-------------------------*)

(*
Assign copies the source graph, theGraph, to the target graph, toGraph. The
main body of Assign does this by first copying all the vertices followed by
copying all the edges from the source to the destination graph.
The algorithmic complexity is O(|V|^2+|E|) due to the mapping between the vertices
of the source and target graphs while copying the edges (see the discussion
of the vertexMap following RecreateTarget).
*)

PROCEDURE Assign  (    theGraph      : Graph     (*--in   *);
                                   VAR toGraph       : Graph     (*--inout *));

(*
RecreateTarget reconstructs the target graph descriptor so that the fields
defining the vertex label and edge attribute data types, and (optionally)
maximum number of vertices between the source and destination graphs are the
same. If the source and destination graphs are the same the routine returns
FALSE indicating that the postconditions for the assignment operation are
already met. The routine returns TRUE if the recreation of the target was
successful. Complexity O(v+e) where v and e are the number of vertices and
edges, respectively, in the original toGraph.  When Clearing the target graph
is unnecessary (the toGraph is initially the NullGraph) the complexity falls
to O(1).
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
This BOOLEAN result is used by the main body of Assign to control whether the
graph assignment operation continues by copying the edges. The following local
variables are used:
        1. v:                   indicates the current vertex being copied from the source
                                        graph. This is also used as a 'loop index' over the vertices
                                        of the source graph.
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

  VAR   v : Vertex;     (*--loop index over vertices being copied *)
  VAR   newVertex : Vertex; (*--new vertex in target graph *)
  VAR   lastVertex: Vertex; (*--last vertex added to list of vertices *)
  VAR   assignItem: AssignProc;

(*
TailInsert adds newVertex to the end of the target graph's adjacency list
given pointers to the first and last elements of the list. Complexity O(1).
*)

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

        assignItem := AssignOf(theGraph^.labelType);
        v := theGraph^.firstVertex;
        lastVertex := NullVertex;

    WHILE (v # NullVertex) DO
          newVertex := NewVertex(toGraph, assignItem(v^.data), assign);
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
  VAR   assignItem: AssignProc; (*--attribute assignment procedure *)
  VAR   vertex1   : Vertex; (*--first vertex of theEdge in target graph *)
  VAR   vertex2   : Vertex; (*--second vertex of theEdge in target graph *)

  BEGIN
        assignItem := AssignOf(theGraph^.attrType);
        theEdge    := theGraph^.firstEdge;
        lastEdge   := NullRef;
        WHILE (theEdge # NullRef) DO
          vertex1 := VertexInMap(theEdge^.first);
          vertex2 := VertexInMap(theEdge^.second);
          newEdge := NewEdge(vertex1,
                                                 vertex2,
                                                 assignItem(theEdge^.weight),
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

(*
14.2.5  Vertex Constructors

Insert adds a vertex to the given graph labeling the vertex with the given item.
The first step is to allocate a new vertex node which, if successful,
is followed by adding the vertex at the head of the adjacency list. If we cannot
create a new vertex node the overflow exception is raised and the Insert operation
aborted. Complexity O(1).
*)

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

(*
Remove deletes the given vertex from the specified graph. If no such vertex
can be found in the graph the novertex exception is raised and the routine
aborted. After we have checked that no exceptions can occur we remove
all edges incident on the vertex, remove the vertex from the adjacency list,
release any dynamically allocated memory used by the vertex label, release the
vertex itself, and update the count of vertices in the graph. Complexity
O(degree(v)).
*)

PROCEDURE Remove   (VAR theGraph   : Graph     (*--inout*);
                                    VAR theVertex  : Vertex    (*--inout*));

(*
Given a vertex and an edge reference, OtherVertex returns the other
member of the unordered pair of vertices associated with the edge.
For example, given the vertex v and the undirected edge {v, w}
OtherVertex returns the vertex w. Complexity O(1)
*)

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

(*
Given a directed edge reference, OtherEdge returns the
other directed edge used in representing an undirected edge.
Complexity O(1).
*)

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

(*
SetLabel assigns a new label to the given vertex of the graph. Prior to
assigning a new vertex label we must release any dynamically allocated memory
used by the old vertex label. Complexity O(1).
*)

PROCEDURE SetLabel (    theVertex  : Vertex    (*--inout*);
                                            theItem    : Label     (*--in   *));
BEGIN
  graphError := noerr;
  IF (theVertex = NullVertex) OR (theVertex^.inGraph = NullGraph) THEN
    RaiseErrIn(setlabel, nullvertex);
  ELSE
        FreeLabel(theVertex);
    theVertex^.data := theItem;
  END (*--if*);
END SetLabel;
(*-------------------------*)

(*
14.2.6  Edge Constructors

Link adds an undirected edge between the from and to vertices labelling the
edge with the given weight attribute. The first step creates a new edge. Secondly,
new references for each vertices edge list are created (only one edge reference
is created when endpoint1 = endpoint2). Lastly, the new edge references and the
edge itself are added to their respective lists. If the memory allocation for
any of the nodes fails, allocations performed prior to the one that failed are
deallocated preventing needless clutter. Complexity O(1).
*)

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

(*
Unlink removes an undirected edge between the two vertices, fromVertex and
toVertex. Complexity O(d) where d is the degree of the from vertex.
*)

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

(*
SetAttribute assigns a new edge labeling to the given edge. Prior to assigning
a new edge attribute we must release any dynamically allocated memory
used by the old edge attribute. Complexity O(1).
*)

PROCEDURE SetAttribute (    theEdge    : Edge      (*--inout*);
                                            theWeight  : Attribute (*--in   *));
BEGIN
  graphError := noerr;
  IF (theEdge = NullEdge) THEN
        RaiseErrIn(setattr, nulledge);
  ELSE
    WITH theEdge^ DO
      FreeAttribute(realEdge);
      realEdge^.weight := theWeight;
    END (*--with*);
  END (*--if*);
END SetAttribute;
(*-------------------------*)

(*
14.2.7  Graph Selectors

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
SizeOf returns the number of edges in the graph, or zero for an undefined
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

PROCEDURE MaxOrderOf (    theGraph  : Graph      (*--in   *))
                                                                    : CARDINAL   (*--out  *);
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
14.2.8  Vertex Selectors

DegreeOf returns the number of edges incident on the given vertex.
We do this by simply returning the value maintained by Link and Unlink.
Complexity O(1).
*)

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

(*
LabelOf returns the vertex label associated with the given vertex.
If the vertex is undefined the NullItem is returned. Complexity O(1).
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
    RETURN NullGraph;
  END (*--if*);
  RETURN theVertex^.inGraph;
END GraphOf;
(*-------------------------*)

(*
14.2.9  Edge Selectors

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
    RETURN theEdge^.realEdge^.weight;
  END (*--if*);
  RETURN NullItem;
END AttributeOf;
(*-------------------------*)

(*
Given an edge, FirstOf returns the  first vertex in the set of vertices
associated with the given edge or the NullVertex if the edge is undefined.
Complexity O(1).
*)

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

(*
Given an edge, SecondOf returns the second vertex in the set of vertices
associated with the given edge or the NullVertex if the edge is undefined.
Complexity O(1).
*)

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

(*
IncidentOn is simply a combination of FirstOf and SecondOf.
Complexity O(1).
*)

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

(*
IsEdge returns true if there the given directed edge is an edge of the given
graph and false otherwise. An advantage of having each vertex identify its
enclosing graph object is use of this field in testing whether the edge is
part of a specified graph. This saves use from having to search every edge in
the graph. Complexity O(1).
*)

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

(*
14.2.10 Passive Iterators

LoopVertices simply iterates over the vertices of the given graph until every
vertex has been examined or the process procedure parameter returns FALSE,
whichever occurs first. Complexity O(|V|).
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

(*
LoopEdges loops over the vertices of the given graph to access the edges associated
with each vertex. Once the process procedure parameter returns FALSE, we exit
both WHILE statements through the use of a RETURN which exits the procedure.
Complexity O(|E|).
*)

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

(*
LoopIterate simply loops over the edges leaving a specified vertex. Complexity
O(degree(v)).
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
TravVertices simply iterates over every vertex in the graph. Complexity O(|V|).
*)

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

(*
TravEdges simply iterates over every edge in the graph using the
set of all edges. Complexity O(|E|).
*)

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

(*
14.2.11 Active Iterators

Each of the active iterators are essentially selectors for the underlying
representation of the adjacency list. Their complexity is O(1).
*)

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

(*
14.2.12 Module Initialization

The module's local variables are initialized to known states. graphError is
used to fill the handler array with a routine that does nothing when an exception
is raised (saving the declaration of a special loop control variable for this
purpose). Applying MIN and MAX to cover all exceptions ensures that this
initialization will be unaffected by any future changes to the number of
Exceptions or their order of declaration within the enumeration. Since a FOR
loop control variable is undefined following the loop, graphError must be
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
END GrafSBMI.
