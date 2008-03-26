package net.harnly.dendron.io
import net.harnly.aaron.extensions.Function1RightAssociativeExtensions._

/** 
 * 
 */
trait VertexCache[V,Ttoken]
{
	// -- abstract --
	def newCache(token: Ttoken, vertex: V): VertexCache[V, Ttoken]
	def map: Map[Ttoken,V]
	def tokenExtractor(vertex: V): Ttoken

	// -- provided --
	def getOrCache(vertex: V): (V, VertexCache[V,Ttoken]) = {
		val token = tokenExtractor(vertex)
		map.get( token ) match {
			case Some(v) => ((v, this))
			case None => ((vertex, newCache(token, vertex)))
		}
	}
}

class DefaultVertexCache[V,Ttoken](
	val map: Map[Ttoken,V],
	extractor: V => Ttoken
) extends VertexCache[V,Ttoken]
{
	def newCache(token: Ttoken, vertex: V) = new DefaultVertexCache(
		map + (token -> vertex),
		extractor
	)
	
	def tokenExtractor(vertex: V) = extractor(vertex)
}

object VertexCache
{
	def emptyWithExtractor[V,Ttoken](extractor: V => Ttoken) = 
	new DefaultVertexCache[V,Ttoken](
		Map.empty[Ttoken,V],
		extractor
	)

	def emptyWithIdentity[V] = emptyWithExtractor[V,V](identity)
}