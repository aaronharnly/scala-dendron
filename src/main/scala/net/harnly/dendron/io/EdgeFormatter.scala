package net.harnly.dendron.io
import net.harnly.aaron.io.{Formatter}
import net.harnly.aaron.extensions.Function1RightAssociativeExtensions._

trait EdgeFormatter[V,E <: Edge[V],Tvertex,Tedge]
extends Formatter[E,Tedge]
{
	// -- abstract --
	def vertexFormatter: VertexFormatter[V,Tvertex]

	// -- supplied --
	def extractVertices(edge: E): (V,V) = edge.orderedVertices
	def formatVertices(vertices: (V, V)): (Tvertex, Tvertex) = 	
	(( vertexFormatter(vertices._1), vertexFormatter(vertices._2) ))
}

trait EdgeFormatterNoMetadata[V,E <: Edge[V],Tvertex,Tedge]
extends EdgeFormatter[V,E,Tvertex,Tedge]
{
	// -- abstract --
	def formatWithFormattedVertices(v1: Tvertex, v2: Tvertex): Tedge

	
	def apply(edge: E) = 
	edge ->: extractVertices _ ->: formatVertices _ ->: {x: (Tvertex,Tvertex) => formatWithFormattedVertices( x._1, x._2 )}
}

trait EdgeFormatterWithMetadata[V,E <: Edge[V],Medge,Tmetadata,Tvertex,Tedge]
extends EdgeFormatter[V,E,Tvertex,Tedge]
{
	// -- abstract --
	def formatWithFormattedVerticesAndMetadata(v1: Tvertex, v2: Tvertex, metadata: Tmetadata): Tedge
	def extractMetadata(input: E): Medge
	def formatMetadata(data: Medge): Tmetadata
	// -- supplied --

	def apply(edge: E) = {
		val formattedVertices = edge ->: extractVertices _ ->: formatVertices _
		val formattedMetadata = edge ->: extractMetadata _ ->: formatMetadata _
		formatWithFormattedVerticesAndMetadata(
			formattedVertices._1,
			formattedVertices._2,
			formattedMetadata
		)
	}
}

trait EdgeFormatterTokenOnly[V,E <: Edge[V],Ttoken,Tvertex,Tedge] 
extends EdgeFormatter[V,E,Tvertex,Tedge]
{
	def vertexFormatter: VertexFormatterTokenOnly[V,Ttoken,Tvertex]
}
