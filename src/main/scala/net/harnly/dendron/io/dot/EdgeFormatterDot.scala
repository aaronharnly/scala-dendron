package net.harnly.dendron.io.dot

abstract class EdgeFormatterDot[V, E <: Edge[V]](
	val separator: String
)
extends EdgeFormatterWithMetadata[
	V,E,
	Map[String,String],String,
	String,String
]
with EdgeFormatterTokenOnly[V,E,String,String,String,String]
{
	def formatWithFormattedVerticesAndMetadata(v1: String, v2: String, metadata: String): String =
	v1 + separator + v2 + metadata
	
	def formatMetadata(data: Map[String,String]) = DotMetadataRenderer(data)
}

abstract class EdgeFormatterDotUndirected[V, E <: Edge[V]]
extends EdgeFormatterDot[V,E](" -- ")

abstract class EdgeFormatterDotDirected[V, E <: DirectedEdge[V]]
extends EdgeFormatterDot[V,E](" -> ")

class EdgeFormatterDotDirectedTrivial[V, E <: DirectedEdge[V]]
extends EdgeFormatterDotDirected[V,E]
{
	val vertexFormatter = new VertexFormatterDotTrivial[V]
	def extractMetadata(input: E) = Map.empty[String,String]
}

object DotMetadataRenderer
{
	def apply(data: Map[String,String]): String = data.map( pair =>
		pair._1 + "=\"" + pair._2 + "\""
	).mkString("[ ",", "," ]")
}