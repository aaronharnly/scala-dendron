package net.harnly.dendron.io

case class VertexPairAndContextEdgeParser[Tvertex,Tcontext,V,E <: Edge[V]](
	edgeFactory: ((V,V,Tcontext)) => Option[E],
	val defaultVertexParser: VertexParser[Tvertex,V],
	val vertexParsers: VertexParser[Tvertex,V]*
)
extends EdgeParser[((Tvertex,Tvertex,Tcontext)),Tvertex,V,E]
{
	override def isDefinedAt(input: ((Tvertex,Tvertex,Tcontext))) = allVertexParsers.exists( p =>
		p.canParse(input._1)
	) && allVertexParsers.exists( p =>
		p.canParse(input._2)
	)
	
	def apply(input: (Tvertex,Tvertex,Tcontext)): Option[E] = {
		val vertex1 = parserForVertex(input._1).parse(input._1)
		val vertex2 = parserForVertex(input._2).parse(input._2)
		val context = input._3
		
		vertex1 match {
			case Some(v1) => vertex2 match {
				case Some(v2) => edgeFactory(v1,v2,context) 
				case None => None
			}
			case None => None
		}
	}
}

class VertexPairAndContextSplitterEdgeParser[Tedge,Tvertex,Tcontext,V,E <: Edge[V]](
	val edgeFactory: ((V,V,Tcontext)) => Option[E],
	val edgeSplitter: Tedge => Option[((Tvertex,Tvertex,Tcontext))],
	val defaultVertexParser: VertexParser[Tvertex,V],
	val vertexParsers: VertexParser[Tvertex,V]*	
)
extends EdgeParser[Tedge,Tvertex,V,E]
{
	val tupleParser = VertexPairAndContextEdgeParser(
		edgeFactory,
		defaultVertexParser,
		vertexParsers : _*
	)
	override def isDefinedAt(input: Tedge) = edgeSplitter(input).map(
		tupleParser.canParse(_)
	).getOrElse(false)
	
	def apply(input: Tedge): Option[E] = edgeSplitter(input).flatMap( pair =>
		tupleParser.parse(pair)
	)
}



