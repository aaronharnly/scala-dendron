package net.harnly.dendron.algorithm

abstract class CollapseGraph[V, E <: DirectedEdge[V], G <: DirectedGraph[V,E], H <: DirectedGraph[V,E]]
extends GraphAlgorithm[V,E,G,H]
{
	def createGraph(v: Set[V], e: Set[E]): H
	def shouldKeepVertex(vertex: V): Boolean
	def createEdge(v1: V, v2: V): E

	def apply(graph: G): H = {
		val verticesToKeep = graph.vertices.filter(shouldKeepVertex(_))
		val keepList = verticesToKeep.toList
		
		def getFirstKeepersFrom(root: V): Set[V] = {
			val outgoingEdges = graph.outgoingEdgesOf(root)
			val targets = outgoingEdges.map(e => e.head)
			val (keeperTargets, loserTargets) = targets.partition(verticesToKeep.contains(_))

			// with the keepers targets, we'll add them to our set, and not explore them further
			val directKeepers = Set(keeperTargets.toList : _*)
			
			// with the losers targets, we'll just add all of *those* losers' keepers, etc.
			val indirectKeepers = for(
				loser <- loserTargets;
				indirect <- getFirstKeepersFrom(loser)
			) yield indirect
			
			directKeepers ++ indirectKeepers
		}

		// from each keeper, find its first keeper neighbors.
		// add an edge for each.
		val edges = keepList.foldLeft(Set.empty[E]) { (set, keeper) =>
			val targetsToAdd = getFirstKeepersFrom(keeper)
			val edgesToAdd = targetsToAdd.map( t =>
				createEdge(keeper, t)
			)
			set ++ edgesToAdd
		}
		createGraph(verticesToKeep, edges)
	}
	
}