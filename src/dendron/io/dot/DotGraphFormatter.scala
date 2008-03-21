package net.harnly.dendron.io.dot

class DotGraphFormatter[V, E <: Edge[V], G <: Graph[V,E]](
	val defaultEdgeFormatter: EdgeFormatter[V,E,String,String],
	val defaultVertexFormatter: VertexFormatter[V,String]
)
extends GraphFormatter[V,E,G,String,String,String]
{
	val edgeFormatters: Iterable[EdgeFormatter[V,E,String,String]] = List()

	def apply(graph: G) = "graph G {\n" + 
		graph.vertices.map( 
			defaultVertexFormatter.format(_)
		).mkString(";\n") +
		graph.edges.map(
			defaultEdgeFormatter.format(_)
		).mkString(";\n") +
	"}\n"
}
