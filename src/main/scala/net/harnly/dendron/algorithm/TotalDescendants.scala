package net.harnly.dendron.algorithm

class TotalDescendants[V,E <: DirectedEdge[V],G <: DirectedGraph[V,E]]
{
	import MapMerge._
	
	def emptyMap = Map.empty[V,Set[V]]
	
	def descendantsFrom(graph: G, root: V): Set[V] =
	graph.outgoingTargetsOf(root).foldLeft(Set.empty[V]) { (sofar, child) =>
		sofar ++ Set(child) ++ descendantsFrom(graph, child)
	}

	def descendantMapFrom(graph: G, root: V): Map[V,Set[V]] =
	descendantMapFrom(graph, root, emptyMap)
	
	def descendantMapFrom(graph: G, root: V, already: Map[V,Set[V]]): Map[V,Set[V]] =
	mergeMaps(
		Map(root -> Set.empty[V]),
		graph.outgoingTargetsOf(root).foldLeft(already) { (sofar, child) =>
			val childResult = descendantMapFrom(graph, child, sofar)
			val childDescendants = childResult.getOrElse(child,Set.empty[V])

			val thisNodeSoFar = sofar.getOrElse(root,Set.empty[V])
			val thisNodeWithChild = thisNodeSoFar ++ Set(child) ++ childDescendants

			mergeMaps(childResult, sofar + (root -> thisNodeWithChild))
		}
	)
	
	
	def apply(graph: G): Map[V,Int] = {
		val roots = DirectedGraph.rootsOf[V,E,G](graph)
		// merge the results...
		val descendantMaps = roots.map( root => descendantMapFrom(graph,root)  )
		val combinedMap = descendantMaps.foldLeft( emptyMap ) { (combined, thisMap) =>
			mergeMaps(combined, thisMap)
		}
		Map(
			combinedMap.map( pair =>
				(pair._1 -> pair._2.size)
			).toList : _*
		)
	}
}

object MapMerge
{
	def mergeMaps[K,V](m1: Map[K,Set[V]], m2: Map[K,Set[V]]): Map[K,Set[V]] = {
		m1 ++ m2.map { pair =>
			val fromM1 = m1.getOrElse(pair._1,Set.empty[V])
			(pair._1 -> (fromM1 ++ pair._2 ))
		}
	}
}
