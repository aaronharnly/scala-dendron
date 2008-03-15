package net.harnly.dendron.io.xml
import scala.xml.{Node}

class XMLGraphFormatter[
	V, 
	E <: Edge[V],
	G <: DirectedGraph[V,E]
](
	graphFormatter: G => Node,
	val defaultVertexFormatter: VertexFormatter[V,Node],
	val defaultEdgeFormatter: EdgeFormatter[V,E,Node,Node],
	val edgeFormatters: EdgeFormatter[V,E,Node,Node]*
)
extends GraphFormatter[
	V,E,G,
	Node, Node, Node
]
{
	def apply(graph: G): Node = 
<graph kind={ graph.getClass.getName }>
	<graphInfo>
		{ graphFormatter(graph) }
	</graphInfo>
	<vertices>
		{
			graph.vertices.toList.map( v =>
				defaultVertexFormatter.format(v)
			)
		}
	</vertices>
	<edges>
		{
			graph.edges.toList.map( e =>
				formatterForEdge(e).format(e)
			)
		}
	</edges>
</graph>
}

