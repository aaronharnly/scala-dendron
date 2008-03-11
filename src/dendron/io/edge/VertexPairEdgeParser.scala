package net.harnly.dendron.io

case class VertexPairEdgeParser[Tvertex,V,E <: Edge[V]](
	edgeFactory: ((V,V)) => E,
	val defaultVertexParser: VertexParser[Tvertex,V],
	val vertexParsers: VertexParser[Tvertex,V]*
)
extends EdgeParser[((Tvertex,Tvertex)),Tvertex,V,E]
{
	override def isDefinedAt(input: ((Tvertex,Tvertex))) = allVertexParsers.exists( p =>
		p.canParse(input._1)
	) && allVertexParsers.exists( p =>
		p.canParse(input._2)
	)
	
	def apply(input: (Tvertex,Tvertex)): Option[E] = {
		val vertex1 = parserForVertex(input._1).parse(input._1)
		val vertex2 = parserForVertex(input._2).parse(input._2)
		
		vertex1 match {
			case Some(v1) => vertex2 match {
				case Some(v2) => Some( edgeFactory(v1,v2) )
				case None => None
			}
			case None => None
		}
	}
}

case class VertexPairSplitterEdgeParser[Tedge,Tvertex,V,E <: Edge[V]](
	edgeFactory: ((V,V)) => E,
	edgeSplitter: Tedge => ((Tvertex,Tvertex)),
	val defaultVertexParser: VertexParser[Tvertex,V],
	val vertexParsers: VertexParser[Tvertex,V]*	
)
extends EdgeParser[Tedge,Tvertex,V,E]
{
	val pairParser = VertexPairEdgeParser(
		edgeFactory,
		defaultVertexParser,
		vertexParsers : _*
	)
	override def isDefinedAt(input: Tedge) = pairParser.canParse(
		edgeSplitter(input)
	)
	def apply(input: Tedge): Option[E] = pairParser.parse(
		edgeSplitter(input)
	)
}
