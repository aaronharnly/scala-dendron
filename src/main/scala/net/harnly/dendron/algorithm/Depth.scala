package net.harnly.dendron.algorithm
import scala.collection.mutable.{Map => MMap, HashMap => MHashMap, Set => MSet, HashSet => MHashSet}
import scala.collection.immutable.{Map => IMap, HashMap => IHashMap}

object Depth
{
	def minDepth[V,E <: DirectedEdge[V],G <: DirectedGraph[V,E]](
		graph: G, 
		foo: V,
		bar: E
	): IMap[V,Option[Int]] = depth[V,E,G](
		graph,
		Dijkstra.minOption,
		foo, bar
	)

	def maxDepth[V,E <: DirectedEdge[V],G <: DirectedGraph[V,E]](
		graph: G, 
		foo: V,
		bar: E
	): IMap[V,Option[Int]] = depth[V,E,G](
		graph,
		Dijkstra.maxOption,
		foo, bar
	)

	def depth[V,E <: DirectedEdge[V],G <: DirectedGraph[V,E]](
		graph: G, 
		bestDistanceFinder: (Option[Int],Option[Int]) => Option[Int],
		foo: V,
		bar: E
	): IMap[V,Option[Int]] = Dijkstra.multiDijkstra(
		graph,
		bestDistanceFinder,
		{ g: G => FindRoots[V,E,G](g) },
		{ (g: G,v: V) => g.outgoingEdgesOf(v) }
	)
	
}
