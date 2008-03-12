package net.harnly.dendron.io.xml
import scala.xml.{Node}

class XMLUndirectedEdgeFormatter[V, E <: Edge[V]](
	val defaultVertexFormatter: VertexFormatter[V,Node],
	val vertexFormatters: VertexFormatter[V,Node]*
)
extends EdgeFormatter[V,E,Node,Node]
{
	def apply(edge: E): Node = {
		val oneVertex = edge.oneVertex
		val otherVertex = edge.otherVertex(oneVertex)
		<edge kind="{edge.getClass.getName}">
			<oneVertex>
				{formatterForVertex(oneVertex).format(oneVertex)}
			</oneVertex>
			<otherVertex>
				{formatterForVertex(otherVertex).format(otherVertex)}
			</otherVertex>
		</edge>
	}
}

class XMLDirectedEdgeFormatter[V, E <: DirectedEdge[V]](
	val defaultVertexFormatter: VertexFormatter[V,Node],
	val vertexFormatters: VertexFormatter[V,Node]*
)
extends EdgeFormatter[V,E,Node,Node]
{
	def apply(edge: E): Node = 
	<edge kind="{edge.getClass.getName}">
		<tail>
			{formatterForVertex(edge.tail).format(edge.tail)}
		</tail>
		<head>
			{formatterForVertex(edge.head).format(edge.head)}
		</head>
	</edge>
}
