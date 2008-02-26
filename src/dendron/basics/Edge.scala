package net.harnly.dendron

trait UnorderedPair[+A]
extends Iterable[A]
{
	def oneItem: A
	def otherItem(first: A): A
}

case class PairSet(
	one: A,
	another: A
) extends UnorderedPair[A]
{
	def elements = new Iterator[A] {
		def hasNext
	}
}

trait Edge[+V <: Vertex]
extends Iterable[V]
{
	// abstract
	type EdgeType <: Edge[V]
	
	// supplied
	def isLoop = (head == tail)
}

trait DirectedEdge[+V <: Vertex]
extends Edge[V]
with Product2[V,V]
{
	// abstract
	def head: V
	def tail: V
	
	def invert: EdgeType

	// supplied
	
	// product
	def _1: V = tail
	def _2: V = head
}

trait WeightedEdge[V <: Vertex]
extends Edge[V]
{
	def weight: Double
	def invert: WeightedEdgeType
}

