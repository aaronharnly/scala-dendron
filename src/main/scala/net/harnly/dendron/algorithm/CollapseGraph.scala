package net.harnly.dendron.algorithm

abstract class CollapseGraph[V, E <: DirectedEdge[V], G <: DirectedGraph[V,E], H <: DirectedGraph[V,E]]
extends GraphAlgorithm[V,E,G,H]
{
	def createGraph(v: Set[V], e: Set[E]): H
	def shouldKeepVertex(vertex: V): Boolean
	def createEdge(v1: V, v2: V): E

	// --- Finding vertices ---
	def getNearestKeepersFrom(graph: G, verticesToKeep: Set[V], vertex: V): Set[V] = 
		getDirectKeepersFrom(graph, verticesToKeep, vertex) ++
		getIndirectKeepersFrom(graph, verticesToKeep, vertex)
	
	def getDirectKeepersFrom(graph: G, verticesToKeep: Set[V], vertex: V): Set[V] = 
	graph.outgoingEdgesOf(vertex).map(e => e.head).filter(v =>
		verticesToKeep.contains(v)
	)
	
	def getDirectLosersFrom(graph: G, verticesToKeep: Set[V], vertex: V): Set[V] = 
	graph.outgoingEdgesOf(vertex).map(e => e.head).filter(v =>
		! verticesToKeep.contains(v)
	)
	
	def getIndirectKeepersFrom(graph: G, verticesToKeep: Set[V], vertex: V): Set[V] = for(
		loser <- getDirectLosersFrom(graph, verticesToKeep, vertex);
		indirect <- getNearestKeepersFrom(graph, verticesToKeep, loser)
	) yield indirect

	// --- Finding edges ---
	def getEdgesForKeeper(graph: G, verticesToKeep: Set[V], vertex: V): Set[E] = 
	getDirectEdgesForKeeper(graph, verticesToKeep, vertex) ++
	getIndirectEdgesForKeeper(graph, verticesToKeep, vertex)

	// existing edges from this keeper to another keeper
	def getDirectEdgesForKeeper(graph: G, verticesToKeep: Set[V], vertex: V): Set[E] =
	getDirectKeepersFrom(graph, verticesToKeep, vertex).map(v =>
		createEdge(vertex, v)
	)
	
	// edges from this keeper, via non-keepers, to another keeper. 
	def getIndirectEdgesForKeeper(graph: G, verticesToKeep: Set[V], vertex: V): Set[E] = 
	getIndirectKeepersFrom(graph, verticesToKeep, vertex).map(v =>
		createEdge(vertex, v)
	)

	def apply(graph: G): H = {
		val verticesToKeep = graph.vertices.filter(shouldKeepVertex(_))
		
		// from each keeper, find its first keeper neighbors.
		// add an edge for each.
		val edges = verticesToKeep.toList.foldLeft(Set.empty[E]) { (set, keeper) =>
			set ++ getEdgesForKeeper(graph, verticesToKeep, keeper)
		}
		createGraph(verticesToKeep, edges)
	}
	
}