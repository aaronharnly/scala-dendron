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

	def pathToRoot[V,E <: DirectedEdge[V],G <: DirectedGraph[V,E]](
		graph: G,
		vertex: V
	): Set[List[V]] = {
		val path = new collection.mutable.ListBuffer[V]
		path += vertex

		def parentList(v: V): List[V] = graph.incomingSourcesOf(v).toList
		var current = vertex
		var parents = parentList(current)
		
		while(parents.length > 0) {
			val oneParent = parents.head
			path += oneParent
			current = oneParent
			parents = parentList(current)
		}
		Set( path.toList )
	}
	
	def verticesInPaths[V](
		paths: Set[List[V]]
	): Set[V] = paths.foldLeft(Set.empty[V]) { (sofar, path) =>
		sofar ++ Set(path : _*)
	}

	def trimToPaths[V,E <: DirectedEdge[V],G <: DirectedGraph[V,E]](
		graph: G,
		pathExtractor: (G, V) => Set[List[V]],
		vertices: V*
	): DirectedGraph[V,E] = {
		val paths = vertices.foldLeft(Set.empty[List[V]])( (sofar, v) =>
			sofar ++ pathExtractor(graph, v)
		)
		val verticesToKeep = verticesInPaths(paths)
		val verticesToRemove = graph.vertices -- verticesToKeep
		verticesToRemove.foldLeft(graph: DirectedGraph[V,E]) { (g, v) =>
			g.removeVertex(v)
		}
	}
	
	def trimToRoot[V,E <: DirectedEdge[V],G <: DirectedGraph[V,E]](
		graph: G,
		vertices: V*
	): DirectedGraph[V,E] = 
	trimToPaths[V,E,G](graph, pathsToRoot[V,E,G], vertices : _*)

	def trimAggressivelyToRoot[V,E <: DirectedEdge[V],G <: DirectedGraph[V,E]](
		graph: G,
		vertices: V*
	): DirectedGraph[V,E] = 
	trimToPaths[V,E,G](graph, pathToRoot[V,E,G], vertices : _*)
}
