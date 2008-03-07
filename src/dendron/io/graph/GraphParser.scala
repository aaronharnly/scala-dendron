package net.harnly.dendron.io
import net.harnly.aaron.io.{Parser}

trait GraphParser[Tinput,Tedge,Tvertex,V,E <: Edge[V],G <: Graph[V,E]]
extends Parser[Tinput,Option[G]]
{
	def defaultEdgeParser: EdgeParser[Tedge,Tvertex,V,E]
	def edgeParsers: Iterable[EdgeParser[Tedge,Tvertex,V,E]]
	def allEdgeParsers = edgeParsers ++ List(defaultEdgeParser)
	
	def parserForEdge(input: Tedge): EdgeParser[Tedge,Tvertex,V,E] = 
		edgeParsers.find( _.canParse(input)).getOrElse(defaultEdgeParser)
	
}
