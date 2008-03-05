package net.harnly.dendron
import net.harnly.dendron.datatypes.{UnorderedPair}

trait Edge[+V]
extends Iterable[V]
{
	// abstract
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

