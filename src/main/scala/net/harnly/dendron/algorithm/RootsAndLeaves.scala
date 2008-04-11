package net.harnly.dendron.algorithm

object FindRoots
{
	def apply[V,E <: DirectedEdge[V],G <: DirectedGraph[V,E]](graph: G): Set[V] = graph.vertices.filter( v =>
		(graph.indegreeOf(v) == 0)
	)
}

object FindLeaves
{
	def apply[V,E <: DirectedEdge[V],G <: DirectedGraph[V,E]](graph: G): Set[V] = graph.vertices.filter( v =>
		(graph.outdegreeOf(v) == 0)
	)
}
