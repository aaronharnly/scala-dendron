package net.harnly.dendron.algorithm
import scala.collection.mutable.{Map => MMap, HashMap => MHashMap, Set => MSet, HashSet => MHashSet}
import scala.collection.immutable.{Map => IMap, HashMap => IHashMap}

object Height
{
	def minHeight[V,E <: DirectedEdge[V],G <: DirectedGraph[V,E]](
		graph: G, 
		foo: V,
		bar: E
	): IMap[V,Option[Int]] = height[V,E,G](
		graph,
		OptionMath.min,
		foo, bar
	)

	def maxHeight[V,E <: DirectedEdge[V],G <: DirectedGraph[V,E]](
		graph: G, 
		foo: V,
		bar: E
	): IMap[V,Option[Int]] = height[V,E,G](
		graph,
		OptionMath.max,
		foo, bar
	)

	def height[V,E <: DirectedEdge[V],G <: DirectedGraph[V,E]](
		graph: G, 
		bestDistanceFinder: (Option[Int],Option[Int]) => Option[Int],
		foo: V,
		bar: E
	): IMap[V,Option[Int]] = Dijkstra.multiDijkstra(
		graph,
		bestDistanceFinder,
		DirectedGraph.leavesOf[V,E,G],
		{ (g: G,v: V) => g.incomingEdgesOf(v) }
	)
	
}
