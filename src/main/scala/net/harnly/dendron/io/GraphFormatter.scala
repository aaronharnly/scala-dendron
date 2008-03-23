package net.harnly.dendron.io
import net.harnly.aaron.io.{Formatter}

trait GraphFormatter[V,E <: Edge[V],G <: Graph[V,E],Tvertex,Tedge,Tgraph] 
extends Formatter[G,Tgraph]
{
	def defaultEdgeFormatter: EdgeFormatter[V,E,Tvertex,Tedge]
	def edgeFormatters: Iterable[EdgeFormatter[V,E,Tvertex,Tedge]]
	def allEdgeFormatters = edgeFormatters ++ List(defaultEdgeFormatter)
	
	def formatterForEdge(input: E): EdgeFormatter[V,E,Tvertex,Tedge] = 
		edgeFormatters.find( _.canFormat(input)).getOrElse(defaultEdgeFormatter)
	
}

