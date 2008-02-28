package net.harnly.dendron
import scala.collection.immutable.{HashSet}

trait Graphs[V <: Vertex, E <: Edge[V]]
{
	type GraphType <: Graph

	trait Graph
	{
		this: GraphType =>

		def vertices: Set[V]
		def edges: Set[E]

		// information

		// mutators	
		def addVertex(vertex: V): GraphType
		def addEdge(edge: E): GraphType

		def removeVertex(vertex: V): GraphType	
		def removeEdge(edge: E): GraphType

		// -- supplied --
		// information
		def isTrivial = (vertices.size == 1) && (edges.size == 0)
		def isNull = (vertices.size == 0) && (edges.size == 0)

		// O(E)
		def edgesOf(vertex: V): Set[E] = 
			edges.filter(_.contains(vertex))

		// O(E)
		def edgesOf(vertex1: V, vertex2: V): Set[E] = {
			val verticesWeWant = List(vertex1, vertex2)
			edges.filter( e =>
				verticesWeWant.forall( v =>
					e.contains(v)
				)
			)
		}

		// O(E)
		def neighbors(vertex: V): Set[V] = 
			edgesOf(vertex).map(_.otherVertex(vertex))

		// mutators
		def addVertices(vertices: V*): GraphType =
		vertices.foldLeft(this)( _.addVertex(_))

		def removeVertices(vertices: V*): GraphType =
		vertices.foldLeft(this)( _.removeVertex(_))

		def addEdges(edges: E*): GraphType =
		edges.foldLeft(this)( _.addEdge(_))

		def removeEdges(edges: E*): GraphType =
		edges.foldLeft(this)( _.removeEdge(_))

		def removeEdge(vertex1: V, vertex2: V): GraphType = 
		edgesOf(vertex1, vertex2).foldLeft(this)( _.removeEdge(_))

		def +(vertex: V) = addVertex(vertex)
		def +(edge: E) = addEdge(edge)

		def -(vertex: V) = removeVertex(vertex)
		def -(edge: E) = removeEdge(edge)
//		def ++(vertices: V*) = addVertices(vertices : _*)
//		def ++(edges: E*) = addEdges(edges : _*)
//		def --(vertices: V*) = removeVertices(vertices : _*)
//		def --(edges: E*) = removeEdges(edges : _*)

	}
}

