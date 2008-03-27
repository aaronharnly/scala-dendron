package net.harnly.dendron.io
import net.harnly.aaron.io.{Formatter}
import net.harnly.aaron.extensions.Function1RightAssociativeExtensions._

trait GraphFormatter[
	V,E <: Edge[V],G <: Graph[V,E],
	Tvertex,Tedge,Tgraph
] 
extends Formatter[G,Tgraph]
{
	// -- abstract --
	def vertexFormatter: VertexFormatter[V,Tvertex]
	def edgeFormatter: EdgeFormatter[V,E,Tvertex,Tedge]
}

trait GraphFormatterNoMetadata[
	V,E <: Edge[V], G <: Graph[V,E],
	Tvertex,Tedge,Tgraph
]
extends GraphFormatter[V,E,G,Tvertex,Tedge,Tgraph]
{
	// -- abstract --
	def formatWithFormattedVerticesAndEdges(
		formattedVertices: Seq[Tvertex],
		formattedEdges: Seq[Tedge]
	): Tgraph
	
	// -- provided --
	
	def apply(graph: G): Tgraph = {
		val formattedVertices = graph.vertices.toList.map(v => vertexFormatter.format(v) )
		val formattedEdges = graph.edges.toList.map(e => edgeFormatter.format(e))
		formatWithFormattedVerticesAndEdges(formattedVertices, formattedEdges)
	}
}

trait GraphFormatterWithMetadata[
	V,E <: Edge[V], G <: Graph[V,E], Mgraph,
	Tmetadata,Tvertex,Tedge,Tgraph
]
extends GraphFormatter[V,E,G,Tvertex,Tedge,Tgraph]
{
	// -- abstract --
	def formatWithFormattedVerticesEdgesAndMetadata(
		formattedVertices: Seq[Tvertex],
		formattedEdges: Seq[Tedge],
		formattedMetadata: Tmetadata
	): Tgraph
	
	def extractMetadata(graph: G): Mgraph
	def formatMetadata(metadata: Mgraph): Tmetadata
	
	// -- provided --
	def apply(graph: G): Tgraph = {
		val formattedVertices = graph.vertices.toList.map(v => vertexFormatter.format(v))
		val formattedEdges = graph.edges.toList.map(e => edgeFormatter.format(e))
		val formattedMetadata = graph ->: extractMetadata _ ->: formatMetadata _
		
		formatWithFormattedVerticesEdgesAndMetadata(
			formattedVertices,
			formattedEdges,
			formattedMetadata
		)
	}
}
