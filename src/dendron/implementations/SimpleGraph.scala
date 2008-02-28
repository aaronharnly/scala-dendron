package net.harnly.dendron

case class SimpleGraphs[V <: Vertex, E <: Edge[V]]
extends Graphs[V,E]
{
	type GraphType = SimpleGraph
	case class SimpleGraph(
		vertices: Set[V],
		edges: Set[E]
	)
	extends Graph
	{
		this: GraphType =>
		def this(vertexList: Seq[V], edgeList: Seq[E]) = this(
			collection.immutable.Set(vertexList : _*) ,
			collection.immutable.Set(edgeList : _*) 
		)

		def asGraphType = this

		// mutators
		def addVertex(vertex: V): GraphType = new SimpleGraph(
			vertices + vertex,
			edges
		)

		def addEdge(edge: E): GraphType = {
			val oneVertex = edge.oneVertex
			new SimpleGraph(
				vertices ++ List(oneVertex, edge.otherVertex(oneVertex)),
				edges + edge
			)
		}

		def removeVertex(vertex: V): GraphType = {
			val touchingEdges = edgesOf(vertex)
			new SimpleGraph(
				vertices - vertex,
				edges -- touchingEdges
			)
		}

		def removeEdge(edge: E): GraphType = {
			// remove the edge, and also the vertices if this is the only edge with the vertex
			val newEdges = edges - edge
			val verticesToRemove = edge.filter{ v =>
				! newEdges.exists( e =>
					e.contains(v)
				)
			}
			new SimpleGraph(
				vertices -- verticesToRemove,
				newEdges
			)
		}

	}
	
	object SimpleGraph
	{
		def empty = new SimpleGraph(
			Set.empty[V],
			Set.empty[E]
		)
	}	
}

