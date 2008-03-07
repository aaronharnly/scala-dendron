package net.harnly.dendron.io

case class VertexPairAndContextEdgeParser[Tvertex,Tcontext,V,E <: Edge[V]](
	edgeFactory: ((V,V,Tcontext)) => E,
	val defaultVertexParser: VertexParser[Tvertex,V],
	val vertexParsers: VertexParser[Tvertex,V]*
)
extends EdgeParser[((Tvertex,Tvertex,Tcontext)),Tvertex,V,E]
{
	def canParse(input: ((Tvertex,Tvertex,Tcontext))) = allVertexParsers.exists( p =>
		p.canParse(input._1)
	) && allVertexParsers.exists( p =>
		p.canParse(input._2)
	)
	
	def parse(input: V2 forSome {type V2 <: (Tvertex,Tvertex,Tcontext)}): Option[E] = {
		val vertex1 = parserForVertex(input._1).parse(input._1)
		val vertex2 = parserForVertex(input._2).parse(input._2)
		val context = input._3
		
		vertex1 match {
			case Some(v1) => vertex2 match {
				case Some(v2) => Some( edgeFactory(v1,v2,context) )
				case None => None
			}
			case None => None
		}
	}
}
