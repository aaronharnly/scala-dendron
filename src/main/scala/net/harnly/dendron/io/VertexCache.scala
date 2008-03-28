package net.harnly.dendron.io

/** 
 * 
 */
trait VertexCache[Mtoken,V]
{
	// -- abstract --
	def +(pair: (Mtoken,V)): VertexCache[Mtoken,V]
	def map: Map[Mtoken,V]

	// -- provided --
	def get(token: Mtoken): Option[V] = map.get(token)	
}

class DefaultVertexCache[Mtoken,V](
	val map: Map[Mtoken,V]
) extends VertexCache[Mtoken,V]
{
	def +(pair: (Mtoken,V)) = new DefaultVertexCache(
		map + pair
	)
}

object VertexCache
{
	def empty[Mtoken,V] = new DefaultVertexCache[Mtoken,V](
		Map.empty[Mtoken,V]
	)
}

