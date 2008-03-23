package net.harnly.dendron.io
import net.harnly.aaron.io.{Formatter}

trait IDOnlyEdgeFormatter[V,E <: Edge[V],Tid,Tvertex,Tedge] 
extends EdgeFormatter[V,E,Tvertex,Tedge]
{
	def defaultVertexFormatter: IDOnlyVertexFormatter[V,Tid,Tvertex]
	def vertexFormatters: Iterable[IDOnlyVertexFormatter[V,Tid,Tvertex]]
	override def allVertexFormatters = vertexFormatters ++ List(defaultVertexFormatter)
	
	override def formatterForVertex(input: V): IDOnlyVertexFormatter[V,Tid,Tvertex] = 
		vertexFormatters.find( _.canFormat(input)).getOrElse(defaultVertexFormatter)
}
