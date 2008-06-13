package net.harnly.dendron.algorithm

object Parents
{
	def parentsOf[V, E <: DirectedEdge[V], G <: DirectedGraph[V,E]](
		graph: G,
		vertex: V
	): Set[V] = {
		val set = collection.mutable.Set.empty[V]
		val queue = new collection.mutable.Queue[V]
		queue.enqueue( graph.incomingSourcesOf(vertex).toList : _* )
		
		while (! queue.isEmpty) {
			val parent = queue.dequeue
			set += parent
			queue.enqueue( graph.incomingSourcesOf(parent).toList : _* )
		}
		
		Set( set.toList : _* )
	}
	
	def selfAndParents[V, E <: DirectedEdge[V], G <: DirectedGraph[V,E]](
		graph: G,
		vertex: V
	): Set[V] = Set(vertex) ++ parentsOf[V,E,G](graph, vertex)
	
	def selvesAndParents[V, E <: DirectedEdge[V], G <: DirectedGraph[V,E]](
		graph: G,
		vertices: V*
	): Set[V] = vertices.foldLeft(Set.empty[V]) { (sofar, v) =>
		sofar ++ selfAndParents[V,E,G](graph, v)
	}
	
	def trimToParents[V, E <: DirectedEdge[V], G <: DirectedGraph[V,E]](
		graph: G,
		factory: (Set[V], Set[E]) => G,
		vertices: V*
	): G = {
		val verticesToKeep = selvesAndParents[V,E,G](graph, vertices : _*)
		val edgesToKeep = graph.edges.filter( e =>
			e.vertices.forall(v =>
				verticesToKeep.contains(v)
			)
		)
		factory(verticesToKeep, edgesToKeep)
	}
}
