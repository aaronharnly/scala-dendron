package net.harnly.dendron.io.dot

abstract class EdgeFormatterDot[V, E <: Edge[V]](
	val separator: String
)
extends EdgeFormatterNoMetadata[V,E,String,String]
with EdgeFormatterTokenOnly[V,E,String,String,String,String]
{
	def formatWithFormattedVertices(v1: String, v2: String) = v1 + separator + v2
}

abstract class EdgeFormatterDotUndirected[V, E <: Edge[V]]
extends EdgeFormatterDot[V,E](" -- ")

abstract class EdgeFormatterDotDirected[V, E <: DirectedEdge[V]]
extends EdgeFormatterDot[V,E](" -> ")
