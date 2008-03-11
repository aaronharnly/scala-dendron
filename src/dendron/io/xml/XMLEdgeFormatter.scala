package net.harnly.dendron.io.xml
import scala.xml.{Node}

class XMLUndirectedEdgeFormatter[V, E <: Edge[V]](
	vertexFormatter: VertexFormatter[V,Node]
)
extends EdgeFormatter[V,E,Node]
{
	def apply(edge: E): Node = {
		val oneVertex = edge.oneVertex
		val otherVertex = edge.otherVertex(oneVertex)
		<edge kind="{edge.getClass.getName}">
			<oneVertex>
				{vertexFormatter.format(oneVertex)}
			</oneVertex>
			<otherVertex>
				{vertexFormatter.format(otherVertex)}
			</otherVertex>
		</edge>
	}
}

class XMLDirectedEdgeFormatter[V, E <: DirectedEdge[V]](
	vertexFormatter: VertexFormatter[V,Node]
)
extends EdgeFormatter[V,E,Node]
{
	def apply(edge: E): Node = 
	<edge kind="{edge.getClass.getName}">
		<tail>
			{vertexFormatter.format(edge.tail)}
		</tail>
		<head>
			{vertexFormatter.format(edge.head)}
		</head>
	</edge>
}
