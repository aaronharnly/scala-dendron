package net.harnly.dendron
import scala.collection.immutable.{HashSet}

trait Graph[V, E <: Edge[V]]
{
	def vertices: Set[V]
	def edges: Set[E]

	// information
	def self: Graph[V,E] = this

	// mutators	
	def addVertex(vertex: V): Graph[V,E]
	def addEdge(edge: E): Graph[V,E]

	def removeVertex(vertex: V): Graph[V,E]	
	def removeEdge(edge: E): Graph[V,E]

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
	
	def verticesInEdges: Set[V] = 
		edges.foldLeft(Set.empty[V])( (set, edge) =>
			set ++ edge
		)
	
	def verticesNotInEdges: Set[V] = 
		vertices -- verticesInEdges

	// O(E)
	def neighborsOf(vertex: V): Set[V] = 
		edgesOf(vertex).map(_.otherVertex(vertex))

	// mutators
	def addVertices(vertices: V*) =
	vertices.foldLeft(self)( _.addVertex(_))

	def removeVertices(vertices: V*) =
	vertices.foldLeft(self)( _.removeVertex(_))

	def addEdges(edges: E*) =
	edges.foldLeft(self)( _.addEdge(_))

	def removeEdges(edges: E*) =
	edges.foldLeft(self)( _.removeEdge(_))

	def removeEdge(vertex1: V, vertex2: V): Graph[V,E] = 
	edgesOf(vertex1, vertex2).foldLeft(self)( _.removeEdge(_))

	def +(vertex: V) = addVertex(vertex)
	def +(edge: E) = addEdge(edge)

	def -(vertex: V) = removeVertex(vertex)
	def -(edge: E) = removeEdge(edge)

//	def ++(vertices: V*) = addVertices(vertices : _*)
//	def ++(edges: E*) = addEdges(edges : _*)
//	def --(vertices: V*) = removeVertices(vertices : _*)
//	def --(edges: E*) = removeEdges(edges : _*)

}
