package net.harnly.dendron.io
import net.harnly.aaron.io.{Formatter}

trait IDOnlyEdgeParser[Tedge,Tid,Tvertex,V,E <: Edge[V]] 
extends EdgeParser[Tedge,Tvertex,V,E]
{
	def defaultVertexParser: IDOnlyVertexParser[Tvertex,Tid,V]
	def vertexParsers: Iterable[IDOnlyVertexParser[Tvertex,Tid,V]]
	override def allVertexParsers = vertexParsers ++ List(defaultVertexParser)
	
	override def parserForVertex(input: Tvertex): IDOnlyVertexParser[Tvertex,Tid,V] = 
		vertexParsers.find( _.canParse(input)).getOrElse(defaultVertexParser)
}
