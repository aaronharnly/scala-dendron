package net.harnly.dendron
import net.harnly.dendron.datatypes.{UnorderedPair,PairSet}

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
	
	def incomingEdgesOf(vertex: V): Set[E] = edges.filter(_.head == vertex)
	def outgoingEdgesOf(vertex: V): Set[E] = edges.filter(_.tail == vertex)
	
	def outgoingTargetsOf(vertex: V): Set[V] = outgoingEdgesOf(vertex).map(_.head)
	def incomingSourcesOf(vertex: V): Set[V] = incomingEdgesOf(vertex).map(_.tail)

	def indegreeOf(vertex: V): Int = incomingEdgesOf(vertex).size
	def outdegreeOf(vertex: V): Int = outgoingEdgesOf(vertex).size
}

