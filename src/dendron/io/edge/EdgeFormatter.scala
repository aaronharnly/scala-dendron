package net.harnly.dendron.io
import net.harnly.aaron.io.{Formatter}

trait EdgeFormatter[V,E <: Edge[V],Tvertex,Tedge] 
extends Formatter[E,Tedge]
{
	def defaultVertexFormatter: VertexFormatter[V,Tvertex]
	def vertexFormatters: Iterable[VertexFormatter[V,Tvertex]]
	def allVertexFormatters = vertexFormatters ++ List(defaultVertexFormatter)
	
	def formatterForVertex(input: V): VertexFormatter[V,Tvertex] = 
		vertexFormatters.find( _.canFormat(input)).getOrElse(defaultVertexFormatter)
}
