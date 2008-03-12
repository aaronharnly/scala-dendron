package net.harnly.dendron.io.xml
import scala.xml.{Node}

class XMLGraphFormatter[
	V, 
	E <: Edge[V],
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
	<vertices>
		{
			graph.verticesNotInEdges.map( v =>
				vertexFormatter.format(v)
			)
		}
	</vertices>
	<edges>
		{
			graph.edges.map( e =>
				edgeFormatter.format(e)
			)
		}
	</edges>
</graph>
}

