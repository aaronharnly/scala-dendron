package net.harnly.dendron.io.dot

abstract class GraphFormatterDot[V, E <: Edge[V], G <: Graph[V,E]]
extends GraphFormatterNoMetadata[V,E,G,String,String,String]
{
	def formatWithFormattedVerticesAndEdges(
		formattedVertices: Seq[String],
		formattedEdges: Seq[String]
	): String = 
	"digraph G {\n" +
	formattedVertices.mkString(";\n") +
	formattedEdges.mkString(";\n") +
	"}\n"
}
