package net.harnly.dendron.algorithm
import scala.collection.mutable.{Map => MMap, HashMap => MHashMap, Set => MSet, HashSet => MHashSet}
import scala.collection.immutable.{Map => IMap, HashMap => IHashMap}
import net.harnly.dendron.datatypes.EdgeMap._

object FloydWarshall
{
	def undirectedAllPairsShortestPath[V, E <: Edge[V], G <: Graph[V,E]](
		graph: G
	): DoubleMap[V,Option[Int]] = allPairsShortestPath(
		graph,
		OptionMath.min,
		Graph.neighbors[V,E,G],
		Graph.edgeBetween[V,E,G]
	)

	def directedAllPairsShortestPath[V, E <: DirectedEdge[V], G <: DirectedGraph[V,E]](
		graph: G
	): DoubleMap[V,Option[Int]] = allPairsShortestPath(
		graph,
		OptionMath.min,
		DirectedGraph.neighbors[V,E,G],
		DirectedGraph.edgeBetween[V,E,G]
	)
	
	def allPairsShortestPath[V, E <: Edge[V], G <: Graph[V,E]](
		graph: G,
		bestDistanceFinder: (Option[Int],Option[Int]) => Option[Int],
		neighborFinder: (G,V) => Set[E],
		edgeFinder: (G,V,V) => Option[E]
	): DoubleMap[V,Option[Int]] = {

		// Initialize the distance map
		var distances = emptyDoubleMap[V,Option[Int]]
		System.err.println("FloydWarshall: Initializing matrix...")
		for(
			v1 <- graph.vertices;
			v2 <- graph.vertices
		) {
			distances = addToDoubleMap(
				distances,
				v1,
				if (v2 == v1)
					v2 -> Some(0)
				else
					v2 -> edgeFinder(graph, v1, v2).map( e => 1 )
			)
		}
		System.err.println("FloydWarshall: done.")
		val totalVertices = graph.vertices.size
		var i = 1
		
		for (
			k <- graph.vertices
		){
				System.err.println("Refining: Step " + i + " of " + totalVertices)
				for (
					v1 <- graph.vertices;
					v2 <- graph.vertices
				) {
					distances = addToDoubleMap(
						distances,
						v1,
						v2 -> bestDistanceFinder( 
							getSubvalue(distances,v1, v2).flatMap(x => x), 
							OptionMath.add(
								getSubvalue(distances,v1,k).flatMap(x => x),
								getSubvalue(distances,k,v2).flatMap(x => x)
							)
						)
					)
				}
				i += 1
		}
		distances
	}	
}