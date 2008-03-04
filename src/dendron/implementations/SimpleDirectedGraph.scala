package net.harnly.dendron

class SimpleDirectedGraph[V, E <: DirectedEdge[V]](
	override val vertices: Set[V],
	override val edges: Set[E]
)
extends SimpleGraph(vertices, edges)
with DirectedGraph[V,E]
{
	// mutators
	override def addVertex(vertex: V): SimpleDirectedGraph[V,E] = new SimpleDirectedGraph(
		vertices + vertex,
		edges
	)

	override def addEdge(edge: E): SimpleDirectedGraph[V,E] = {
		val oneVertex = edge.oneVertex
		new SimpleDirectedGraph(
			vertices ++ List(oneVertex, edge.otherVertex(oneVertex)),
			edges + edge
		)
	}

	override def removeVertex(vertex: V): SimpleDirectedGraph[V,E] = {
		val touchingEdges = edgesOf(vertex)
		new SimpleDirectedGraph(
			vertices - vertex,
			edges -- touchingEdges
		)
	}

	override def removeEdge(edge: E): SimpleDirectedGraph[V,E] = {
		// remove the edge, and also the vertices if this is the only edge with the vertex
		val newEdges = edges - edge
		val verticesToRemove = edge.filter{ v =>
			! newEdges.exists( e =>
				e.contains(v)
			)
		}
		new SimpleDirectedGraph(
			vertices -- verticesToRemove,
			newEdges
		)
	}
	
}

object SimpleDirectedGraph
{
	def empty[V,E <: DirectedEdge[V]] = new SimpleDirectedGraph(
		Set.empty[V],
		Set.empty[E]
	)
}	
