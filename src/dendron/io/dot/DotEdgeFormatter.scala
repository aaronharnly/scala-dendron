package net.harnly.dendron.io.dot

class DotEdgeFormatter[V, E <: Edge[V]](
	val separator: String,
	val defaultVertexFormatter: IDOnlyVertexFormatter[V,String,String],
	val vertexFormatters: IDOnlyVertexFormatter[V,String,String]*
)
extends IDOnlyEdgeFormatter[V,E,String,String,String]
{
	def apply(edge: E) = 
	formatterForVertex(edge.orderedVertices._1).format(edge.orderedVertices._1) + 
	" -- " +
	formatterForVertex(edge.orderedVertices._2).format(edge.orderedVertices._2)
}

class DotUndirectedEdgeFormatter[V, E <: Edge[V]](
	override val defaultVertexFormatter: IDOnlyVertexFormatter[V,String,String],
	override val vertexFormatters: IDOnlyVertexFormatter[V,String,String]*
)
extends DotEdgeFormatter[V,E](
	" -- ",
	defaultVertexFormatter,
	vertexFormatters : _*
)

class DotDirectedEdgeFormatter[V, E <: DirectedEdge[V]](
	override val defaultVertexFormatter: IDOnlyVertexFormatter[V,String,String],
	override val vertexFormatters: IDOnlyVertexFormatter[V,String,String]*
)
extends DotEdgeFormatter[V,E](
	" -> ",
	defaultVertexFormatter,
	vertexFormatters : _*
)
