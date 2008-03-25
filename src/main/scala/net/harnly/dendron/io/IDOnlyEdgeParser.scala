package net.harnly.dendron.io
import net.harnly.aaron.io.{Formatter}

trait IDOnlyEdgeParser[Tedge,Tvertex,Tid,V,E <: Edge[V]] 
extends EdgeParser[Tedge,Tvertex,V,E]
{
	def defaultVertexParser: IDOnlyVertexParser[Tvertex,Tid,V]
	def vertexParsers: Iterable[IDOnlyVertexParser[Tvertex,Tid,V]]
	override def allVertexParsers = vertexParsers ++ List(defaultVertexParser)
	
	override def parserForVertex(input: Tvertex): IDOnlyVertexParser[Tvertex,Tid,V] = 
		vertexParsers.find( _.canParse(input)).getOrElse(defaultVertexParser)
}


class IDOnlyVertexPairAndContextSplitterEdgeParser[Tedge,Tvertex,Tid,Tcontext,V,E <: Edge[V]](
	override val edgeFactory: ((V,V,Tcontext)) => Option[E],
	override val edgeSplitter: Tedge => Option[((Tvertex,Tvertex,Tcontext))],
	override val defaultVertexParser: IDOnlyVertexParser[Tvertex,Tid,V],
	override val vertexParsers: IDOnlyVertexParser[Tvertex,Tid,V]*
)
extends VertexPairAndContextSplitterEdgeParser[Tedge,Tvertex,Tcontext,V,E](
	edgeFactory,
	edgeSplitter,
	defaultVertexParser,
	vertexParsers : _*
)
with IDOnlyEdgeParser[Tedge,Tvertex,Tid,V,E]
