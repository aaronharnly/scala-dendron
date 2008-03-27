package net.harnly.dendron
import net.harnly.dendron.datatypes.{UnorderedPair,PairSet}

trait Edge[V]
extends Iterable[V]
{
	// -- abstract --
	def unorderedVertices: UnorderedPair[V]
	
	// supplied
	// -- Iterable --
	def elements = vertices.elements

	// Vertex access
	def vertices: Seq[V] = unorderedVertices
	def oneVertex: V = unorderedVertices.oneItem
	def otherVertex(first: V): V = unorderedVertices.otherItem(first)
	def orderedVertices: (V,V) = {
		val v1 = unorderedVertices.oneItem
		val v2 = unorderedVertices.otherItem(v1)
		(v1, v2)
	}
	
	// Queries
	def contains(vertex: V) = vertices.elements.contains(vertex)
	def isLoop: Boolean = orderedVertices._1 == orderedVertices._2
	def isProper = ! isLoop
	
	// Any overrides
	override def toString: String = "(" + orderedVertices._1 + " -- " + orderedVertices._2 + ")"
}

trait DirectedEdge[V]
extends Edge[V]
{
	// abstract
	def head: V
	def tail: V
	
	def invert: DirectedEdge[V]

	// -- supplied --
	// Edge overrides
	override def vertices: Seq[V] = List(tail, head)
	override def orderedVertices: (V,V) = (tail, head)

	// Any overrides
	override def toString: String = "(" + tail + " -> " + head + ")"
}

object DirectedEdge
{
	def dummy[V]: DirectedEdge[V] = new DirectedEdge[V]{
		val head = null.asInstanceOf[V]
		val tail = null.asInstanceOf[V]
		val unorderedVertices = new PairSet[V](head, tail)
		def invert = this
	}
}
