package net.harnly.dendron.io.xml
import scala.xml.{Node}

class XMLGraphFormatter[
	V, 
	E <: Edge[V],
	G <: DirectedGraph[V,E]
](
	graphFormatter: G => Node,
	val defaultEdgeFormatter: EdgeFormatter[V,E,Node,Node],
	val edgeFormatters: EdgeFormatter[V,E,Node,Node]*
)
extends GraphFormatter[
	V,E,G,
	Node, Node, Node
]
{
	def apply(graph: G): Node = 
<graph kind="{ graph.getClass.getName }">
	<graphInfo>
		{ graphFormatter(graph) }
	</graphInfo>
	<vertices>
		{
			graph.verticesNotInEdges.map( v =>
				defaultEdgeFormatter.formatterForVertex(v).format(v)
			)
		}
	</vertices>
	<edges>
		{
			graph.edges.map( e =>
				formatterForEdge(e).format(e)
			)
		}
	</edges>
</graph>
}

