package net.harnly.dendron

case class SimpleGraph[V, E <: Edge[V]](
	vertices: Set[V],
	edges: Set[E]
)
extends Graph[V,E]
{
	override def self: SimpleGraph[V,E] = this
	
	def this(vertexList: Seq[V], edgeList: Seq[E]) = this(
		collection.immutable.Set(vertexList : _*) ,
		collection.immutable.Set(edgeList : _*) 
	)
	
	// mutators
	def addVertex(vertex: V): SimpleGraph[V,E] = new SimpleGraph(
		vertices + vertex,
		edges
	)

	def addEdge(edge: E): SimpleGraph[V,E] = {
		val oneVertex = edge.oneVertex
		new SimpleGraph(
			vertices ++ List(oneVertex, edge.otherVertex(oneVertex)),
			edges + edge
		)
	}

	def removeVertex(vertex: V): SimpleGraph[V,E] = {
		val touchingEdges = edgesOf(vertex)
		new SimpleGraph(
			vertices - vertex,
			edges -- touchingEdges
		)
	}

	def removeEdge(edge: E): SimpleGraph[V,E] = {
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
	def empty[V,E <: Edge[V]] = new SimpleGraph(
		Set.empty[V],
		Set.empty[E]
	)
}	

