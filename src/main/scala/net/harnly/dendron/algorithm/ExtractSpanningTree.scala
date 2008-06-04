package net.harnly.dendron.algorithm

object ExtractSpanningTree
{
	def pathsToRoot[V,E <: DirectedEdge[V],G <: DirectedGraph[V,E]](
		graph: G,
		vertex: V
	): Set[List[V]] = {
		val parents = graph.incomingSourcesOf(vertex)
		if (parents.size == 0)
			Set(vertex :: Nil)
		else {
			parents.flatMap( parent =>
				pathsToRoot[V,E,G](graph,parent).map( path =>
					vertex :: path
				)
			)
		}
	}
	
	def verticesInPaths[V](
		paths: Set[List[V]]
	): Set[V] = paths.foldLeft(Set.empty[V]) { (sofar, path) =>
		sofar + Set(path : _*)
	}
	
	def trimToRoot[V,E <: DirectedEdge[V],G <: DirectedGraph[V,E]](
		graph: G,
		vertices: V*
	): G = {
		val paths = vertices.foldLeft(Set.empty[List[V]])( (sofar, v) =>
			sofar ++ pathsToRoot[V,E,G](graph, v)
		)
		val verticesToKeep = verticesInPaths(paths)
		val verticesToRemove = graph.vertices -- verticesToKeep
		verticesToRemove.foldLeft(graph) { (g, v) =>
			g.removeVertex(v)
		}
	}
}
