package net.harnly.dendron.io.dot

abstract class GraphFormatterDot[V, E <: Edge[V], G <: Graph[V,E]]
extends GraphFormatterNoMetadata[V,E,G,String,String,String]
{
	val lineEnd = ";\n"
	
	def formatWithFormattedVerticesAndEdges(
		formattedVertices: Seq[String],
		formattedEdges: Seq[String]
	): String = 
	"digraph G {\n" +
	formattedVertices.mkString("",lineEnd,lineEnd) +
	formattedEdges.mkString("",lineEnd,lineEnd) +
	"}\n"
}

class GraphFormatterDotDirectedTrivial[V, E <: DirectedEdge[V], G <: DirectedGraph[V,E]]
extends GraphFormatterDot[V,E,G]
{
	val vertexFormatter = new VertexFormatterDotTrivial[V]
	val edgeFormatter = new EdgeFormatterDotDirectedTrivial[V,E]
}

class GraphFormatterDotDirectedTrivialWithVertexMetadata[V, E <: DirectedEdge[V], G <: DirectedGraph[V,E]](
	vertexMetadata: Map[V, Map[String,String]]
)
extends GraphFormatterDot[V,E,G]
{
	val vertexFormatter = new VertexFormatterDotTrivialWithMetadata[V](vertexMetadata)
	val edgeFormatter = new EdgeFormatterDotDirectedTrivial[V,E]
}
