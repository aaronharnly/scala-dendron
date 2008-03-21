package net.harnly.dendron.io
import net.harnly.aaron.io.{Parser,Formatter}

// useful for storing edges compactly
trait VertexIDFormatter[-V,+Tid]
extends Formatter[V,Tid]

trait VertexIDParser[-Tvertex,+Tid]
extends Function1[Tvertex,Tid]

trait VertexIDCache[V,Tid]
{
	def formatter: VertexIDFormatter[V,Tid]
	def factory(ids: Map[Tid,V], vertices: Map[V,Tid]): VertexIDCache[V,Tid]
	
	def byID: Map[Tid,V]
	def byVertex: Map[V,Tid]
	
	def addVertex(vertex: V): VertexIDCache[V,Tid] = {
		val id = formatter(vertex)
		factory(
			byID + (id -> vertex),
			byVertex + (vertex -> id)
		)
	}
}

case class VertexIDHashCache[V,Tid](
	formatter: VertexIDFormatter[V,Tid],
	byID: Map[Tid,V],
	byVertex: Map[V,Tid]
)
{
	def factory(ids: Map[Tid,V], vertices: Map[V,Tid]) = VertexIDHashCache(
		formatter,
		ids,
		vertices
	)
}

trait IDOnlyVertexFormatter[V,Tid,Tvertex]
extends VertexFormatter[V,Tvertex]
{
	def vertexIDFormatter: VertexIDFormatter[V,Tid]
}

trait IDOnlyVertexParser[V,Tid,Tvertex]
extends VertexParser[Tvertex,V]
{
	def vertexIDCache
}