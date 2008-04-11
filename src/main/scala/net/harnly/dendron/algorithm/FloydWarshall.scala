package net.harnly.dendron.algorithm
import scala.collection.mutable.{Map => MMap, HashMap => MHashMap, Set => MSet, HashSet => MHashSet}
import scala.collection.immutable.{Map => IMap, HashMap => IHashMap}
import net.harnly.dendron.datatypes.EdgeMap._

object FloydWarshall
{
	def allPairsShortestPath[V, E <: Edge[V], G <: Graph[V,E]](
		graph: G,
		bestDistanceFinder: (Option[Int],Option[Int]) => Option[Int],
		neighborFinder: (G,V) => Set[E],
		edgeFinder: (G,V,V) => Option[E]
	): DoubleMap[V,Option[Int]] = {

		// Initialize the distance map
		var distances = emptyDoubleMap[V,Option[Int]]
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
		
		for (
			k <- graph.vertices;
			v1 <- graph.vertices;
			v2 <- graph.vertices
		) {
			distances = addToDoubleMap(
				distances,
				v1,
				v2 -> minOption( 
					getSubvalue(distances,v1, v2).map(x => x.get), 
					optionAdd(
						getSubvalue(distances,v1,k).map(x => x.get),
						getSubvalue(distances,k,v2).map(x => x.get)
					)
				)
			)
		}
		distances
	}

	def optionAdd(a: Option[Int], b: Option[Int]): Option[Int] = for(
		x <- a;
		y <- b
	) yield (x + y)
	
	def minOption(a: Option[Int], b: Option[Int]): Option[Int] = a match {
		case None => b
		case Some(x) => b match {
			case None => Some(x)
			case Some(y) => if (y < x) Some(y) else Some(x)
		}
	}
	
}