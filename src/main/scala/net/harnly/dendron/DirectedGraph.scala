package net.harnly.dendron

trait DirectedGraph[V, E <: DirectedEdge[V]]
extends Graph[V,E]
{
	// -- abstract --
	def addVertex(vertex: V): DirectedGraph[V,E]
	def addEdge(edge: E): DirectedGraph[V,E]

	def removeVertex(vertex: V): DirectedGraph[V,E]	
	def removeEdge(edge: E): DirectedGraph[V,E]
	override def removeEdge(vertex1: V, vertex2: V): DirectedGraph[V,E] = 
		getEdge(vertex1, vertex2).map(e => removeEdge(e)).getOrElse(this)
	
	// -- supplied --
	def getEdge(vertex1: V, vertex2: V): Option[E] = edges.find(e => e.tail == vertex1 && e.head == vertex2)

	def outgoingEdgesOf(vertex1: V, vertex2: V): Set[E] = Set(
		getEdge(vertex1, vertex2).toList : _*
	)
	
	def incomingEdgesOf(vertex: V): Set[E] = edges.filter(_.head == vertex)
	def outgoingEdgesOf(vertex: V): Set[E] = edges.filter(_.tail == vertex)
	
	def outgoingTargetsOf(vertex: V): Set[V] = outgoingEdgesOf(vertex).map(_.head)
	def incomingSourcesOf(vertex: V): Set[V] = incomingEdgesOf(vertex).map(_.tail)

	def indegreeOf(vertex: V): Int = incomingEdgesOf(vertex).size
	def outdegreeOf(vertex: V): Int = outgoingEdgesOf(vertex).size
}

object DirectedGraph
{
	def neighbors[V, E <: DirectedEdge[V], G <: DirectedGraph[V,E]](
		graph: G, v: V
	): Set[E] = graph.outgoingEdgesOf(v)

	def edgeBetween[V, E <: DirectedEdge[V], G <: DirectedGraph[V,E]](
		graph: G, v1: V, v2: V
	): Option[E] = graph.getEdge(v1,v2)
	
	def incomingEdgesOf[V, E <: DirectedEdge[V], G <: DirectedGraph[V,E]](
		graph: G, v: V
	): Set[E] = graph.incomingEdgesOf(v)

	def outgoingEdgesOf[V, E <: DirectedEdge[V], G <: DirectedGraph[V,E]](
		graph: G, v: V
	): Set[E] = graph.outgoingEdgesOf(v)
	
	
	def rootsOf[V,E <: DirectedEdge[V],G <: DirectedGraph[V,E]](graph: G): Set[V] =
	 graph.vertices.filter( v => 
		(graph.indegreeOf(v) == 0)
	)

	def leavesOf[V,E <: DirectedEdge[V],G <: DirectedGraph[V,E]](graph: G): Set[V] =
	graph.vertices.filter( v =>
		(graph.outdegreeOf(v) == 0)
	)
}
