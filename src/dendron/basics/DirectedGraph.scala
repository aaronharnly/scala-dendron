package net.harnly.dendron
import net.harnly.dendron.datatypes.{UnorderedPair,PairSet}

trait DirectedEdge[+V]
extends Edge[V]
{
	// abstract
	def head: V
	def tail: V
	
	def invert: DirectedEdge[V]

	// supplied
	
	// product
	override def toString: String = "(" + tail + " -> " + head + ")"
}


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

object DirectedEdge
{
	def dummy[V]: DirectedEdge[V] = new DirectedEdge[V]{
		val head = null.asInstanceOf[V]
		val tail = null.asInstanceOf[V]
		val vertices = new PairSet[V](head, tail)
		def invert = this
	}
}
