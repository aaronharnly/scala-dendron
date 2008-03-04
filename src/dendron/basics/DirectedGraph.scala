package net.harnly.dendron

trait DirectedGraph[V, E <: DirectedEdge[V]]
extends Graph[V,E]
{
	// -- abstract --
	def addVertex(vertex: V): DirectedGraph[V,E]
	def addEdge(edge: E): DirectedGraph[V,E]

	def removeVertex(vertex: V): DirectedGraph[V,E]	
	def removeEdge(edge: E): DirectedGraph[V,E]
	
	// -- supplied --
	def incomingEdgesOf(vertex: V): Set[E] =
	edges.filter(_.head == vertex)
	
	def outgoingEdgesOf(vertex: V): Set[E] =
	edges.filter(_.tail == vertex)

	def indegreeOf(vertex: V): Int = incomingEdgesOf(vertex).size
	def outdegreeOf(vertex: V): Int = outgoingEdgesOf(vertex).size
}
