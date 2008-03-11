package net.harnly.dendron.io
import net.harnly.aaron.io.{Parser}

trait EdgeParser[Tedge,Tvertex,V,E <: Edge[V]] 
extends Parser[Tedge,Option[E]]
{
	def defaultVertexParser: VertexParser[Tvertex,V]
	def vertexParsers: Iterable[VertexParser[Tvertex,V]]
	def allVertexParsers = vertexParsers ++ List(defaultVertexParser)
	
	def parserForVertex(input: Tvertex): VertexParser[Tvertex, V] = 
		vertexParsers.find( _.canParse(input)).getOrElse(defaultVertexParser)
}
