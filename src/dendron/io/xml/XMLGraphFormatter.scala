package net.harnly.dendron.io.xml
import scala.xml.{Node}

class XMLGraphFormatter[
	V, 
	E <: DirectedEdge[V],
	G <: DirectedGraph[V,E]
](
	graphFormatter: G => Node,
	edgeFormatter: EdgeFormatter[V,E,Node],
	vertexFormatter: VertexFormatter[V,Node]
)
extends GraphFormatter[
	V,E,G,
	Node
]
{
	def apply(graph: G): Node = 
<graph kind="{ graph.getClass.getName }">
	<graphInfo>
		{ graphFormatter(graph) }
	</graphInfo>
	<nodes>
		{
			graph.vertices.map( v =>
				vertexFormatter.format(v)
			)
		}
	</nodes>
	<edges>
		{
			graph.edges.map( e =>
				edgeFormatter.format(e)
			)
		}
	</edges>
</graph>
}

