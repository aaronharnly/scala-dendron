package net.harnly.dendron.io
import net.harnly.aaron.io.{Parser}

trait VertexParser[Tvertex,V]
extends Parser[Tvertex,Option[V]]

trait CachedVertexParser[Tvertex,Ttoken,V]
extends Parser[(Tvertex, VertexCache[Ttoken,V]),Option[(V,VertexCache[Ttoken,V])]]
{
	// -- abstract --
	def vertexParser: VertexParser[Tvertex,V]
	def parseToken(input: Tvertex): Option[Ttoken]
	
	// -- supplied --
	def parseWithToken(input: Tvertex, token: Ttoken): Option[V] = vertexParser.parse(input)
	
	def apply(input: (Tvertex, VertexCache[Ttoken,V])): Option[(V,VertexCache[Ttoken,V])] = {
		val (vertexInput, cache) = input
		val maybeToken = parseToken(vertexInput)

		maybeToken match {
			case None => None
			case Some(token) =>
				cache.get(token) match {
					case Some(vertex) =>
						Some(vertex -> cache)
					case None => {
						vertexParser.parse(vertexInput) match {
							case None => None
							case Some( parsed ) => Some(parsed -> (cache + (token -> parsed)))
						}
					}
				}
		}
	}
}

trait CachedVertexParserNoToken[Tvertex,V]
extends CachedVertexParser[Tvertex,V,V]
{
	// -- supplied --
	def parseToken(input: Tvertex) = vertexParser.parse(input)
	override def parseWithToken(input: Tvertex, token: V) = Some(token)
}
