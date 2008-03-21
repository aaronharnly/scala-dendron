package net.harnly.dendron

class SimpleDirectedGraph[V, E <: DirectedEdge[V]](
	override val vertices: Set[V],
	override val edges: Set[E]
)
extends SimpleGraph(vertices, edges)
with DirectedGraph[V,E]
{
	override def self: SimpleDirectedGraph[V,E] = this
	def this(vertexList: Seq[V], edgeList: Seq[E]) = this(
		collection.immutable.Set(vertexList : _*) ,
		collection.immutable.Set(edgeList : _*) 
	)

	// mutators
	override def addVertex(vertex: V): SimpleDirectedGraph[V,E] = new SimpleDirectedGraph(
		vertices + vertex,
		edges
	)

	override def addEdge(edge: E): SimpleDirectedGraph[V,E] = 
		new SimpleDirectedGraph(
			vertices ++ edge.vertices,
			edges + edge
		)

	override def removeVertex(vertex: V): SimpleDirectedGraph[V,E] = 
		new SimpleDirectedGraph(
			vertices - vertex,
			edges -- edgesOf(vertex)
		)

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

	override def addEdges(edges: E*): SimpleDirectedGraph[V,E] = 
	edges.foldLeft(self)( _.addEdge(_))
	
}

object SimpleDirectedGraph
{
	def empty[V,E <: DirectedEdge[V]] = new SimpleDirectedGraph(
		Set.empty[V],
		Set.empty[E]
	)
}	
