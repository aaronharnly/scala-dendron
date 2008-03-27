package net.harnly.dendron.io

/** 
 * 
 */
trait VertexCache[Ttoken,V]
{
	// -- abstract --
	def +(pair: (Ttoken,V)): VertexCache[Ttoken,V]
	def map: Map[Ttoken,V]

	// -- provided --
	def get(token: Ttoken): Option[V] = map.get(token)	
}

class DefaultVertexCache[Ttoken,V](
	val map: Map[Ttoken,V]
) extends VertexCache[Ttoken,V]
{
	def +(pair: (Ttoken,V)) = new DefaultVertexCache(
		map + pair
	)
}

object VertexCache
{
	def empty[Ttoken,V] = new DefaultVertexCache[Ttoken,V](
		Map.empty[Ttoken,V]
	)
}

