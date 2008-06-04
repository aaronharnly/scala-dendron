package net.harnly.dendron
import scala.collection.immutable.{Set2}

trait Edge[V]
extends Iterable[V]
{
	// -- abstract --
	def unorderedVertices: Set2[V]
	
	// supplied
	// -- Iterable --
	def elements = vertices.elements

	// Vertex access
	def vertices: Seq[V] = unorderedVertices.toList
	def orderedVertices: (V,V) = {
		val ordered = unorderedVertices.toList
		(ordered(0),ordered(1))
	}
	def otherVertex(v: V): V = {
		val ordered = orderedVertices
		if (ordered._1 == v)
			ordered._2
		else
			ordered._1
	}
	
	// Queries
	def contains(vertex: V) = vertices.elements.contains(vertex)
	def isLoop: Boolean = {
		val ordered = orderedVertices
		ordered._1 == ordered._2
	}
	def isProper = ! isLoop
	
	// Any overrides
	override def toString: String = {
		val ordered = orderedVertices
		"(" + ordered._1 + " -- " + ordered._2 + ")"
	}
	
	override def equals(obj: Any) = obj match {
		case that: Edge[_] =>
			this.unorderedVertices == that.unorderedVertices
		case _ => false
	}
}

trait DirectedEdge[V]
extends Edge[V]
{
	// abstract
	def head: V
	def tail: V
	
	def invert: DirectedEdge[V]

	// -- supplied --
	def unorderedVertices = new Set2(tail, head)
	// Edge overrides
	override def vertices: Seq[V] = List(tail, head)
	override def orderedVertices: (V,V) = (tail, head)

	// Any overrides
	override def toString: String = "(" + tail + " -> " + head + ")"
	override def equals(obj: Any) = obj match {
		case that: DirectedEdge[_] =>
			this.head == that.head &&
			this.tail == that.tail
		case _ => false
	}
}

object DirectedEdge
{
	def dummy[V]: DirectedEdge[V] = new DirectedEdge[V]{
		val head = null.asInstanceOf[V]
		val tail = null.asInstanceOf[V]
		def invert = this
	}
}
