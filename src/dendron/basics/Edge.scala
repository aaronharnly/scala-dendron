package net.harnly.dendron
import net.harnly.dendron.datatypes.{UnorderedPair}

trait Edge[+V]
extends Iterable[V]
{
	// abstract
	type EdgeType <: Edge[V]
	def vertices: UnorderedPair[V]
	
	
	// supplied
	def elements = vertices.elements
	def oneVertex: V = vertices.oneItem
	def otherVertex(first: V2 forSome {type V2 <: V}): V = vertices.otherItem(first)
	def contains(vertex: V2 forSome {type V2 <: V}) = vertices.elements.contains(vertex)

	def isLoop = {
		val oneEnd = vertices.oneItem
		(oneEnd == vertices.otherItem(oneEnd))
	}
	def isProper = ! isLoop
}

trait DirectedEdge[+V]
extends Edge[V]
{
	// abstract
	def head: V
	def tail: V
	
	def invert: EdgeType

	// supplied
	
	// product
}

trait WeightedEdge[+V]
extends Edge[V]
{
	def weight: Double
}

