package net.harnly.dendron.io
import net.harnly.aaron.io.{Parser}

trait VertexParser[Tvertex,V]
extends Parser[Tvertex,Option[V]]

trait CachedVertexParser[Tvertex,Ttoken,Mtoken,V]
extends Parser[(Tvertex, VertexCache[Mtoken,V]),Option[(V,VertexCache[Mtoken,V])]]
{
	// -- abstract --
	def vertexParser: VertexParser[Tvertex,V]
	def extractToken(input: Tvertex): Option[Ttoken]
	def parseToken(input: Ttoken): Option[Mtoken]
	
	// -- supplied --
	def parseWithParsedToken(input: Tvertex, tokenInput: Mtoken): Option[V] = vertexParser.parse(input)
	
	def apply(input: (Tvertex, VertexCache[Mtoken,V])): Option[(V,VertexCache[Mtoken,V])] = {
		val (vertexInput, cache) = input
		val maybeTokenInput = extractToken(vertexInput)
		maybeTokenInput match {
			case None => None
			case Some(tokenInput) => {
				val maybeToken = parseToken(tokenInput)

				maybeToken match {
					case None => None
					case Some(token) =>
						cache.get(token) match {
							case Some(vertex) =>
								Some(vertex -> cache)
							case None => {
								parseWithParsedToken(vertexInput, token) match {
									case None => None
									case Some( parsed ) => Some(parsed -> (cache + (token -> parsed)))
								}
							}
						}
				}
			}
		}
	}
}

trait CachedVertexParserNoToken[Tvertex,V]
extends CachedVertexParser[Tvertex,Tvertex,V,V]
{
	// -- supplied --
	def extractToken(input: Tvertex) = Some(input)
	def parseToken(input: Tvertex) = vertexParser.parse(input)
	override def parseWithParsedToken(input: Tvertex, token: V) = Some(token)
}
