package net.harnly.dendron.algorithms
import scala.collection.mutable.{Map => MMap, HashMap => MHashMap, Set => MSet, HashSet => MHashSet}
import scala.collection.immutable.{Map => IMap, HashMap => IHashMap}

object Dijkstra
{	
	def multiDijkstra[V,E <: Edge[V],G <: Graph[V,E]](
		graph: G, 
		bestDistanceFinder: (Option[Int],Option[Int]) => Option[Int],
		sourceFinder: G => Set[V],
		neighborFinder: (G,V) => Set[E]
	): IMap[V,Option[Int]] = {
		
		val sources = sourceFinder(graph)
		val distancesFromSources = sources.map(dijkstra(graph,_,bestDistanceFinder,neighborFinder))
		
		val initialMap = IMap.empty[V,Option[Int]] ++ graph.vertices.map( v =>
			(v -> None)
		)
		
		distancesFromSources.foldLeft(initialMap) { (result, distances) =>
			IMap.empty[V,Option[Int]] ++ distances.map( pair =>
				(pair._1 -> bestDistanceFinder(pair._2, result(pair._1)))
			)
		}
	}
	
	def dijkstra[V,E <: Edge[V], G <: Graph[V,E]](
		graph: G, 
		source: V,
		bestDistanceFinder: (Option[Int],Option[Int]) => Option[Int],
		neighborFinder: (G,V) => Set[E]
	): IMap[V,Option[Int]] = {
		val distances = MMap.empty[V,Option[Int]]
		val visited = scala.collection.mutable.Set.empty[V]
		
		// Wikicode 2,3,4
		graph.vertices.foreach(v =>
			distances += (v -> None)
		)
		
		// Wikicode 5
		distances += (source -> Some(0))
		
		val Q = MHashSet( graph.vertices.toList : _*)
		while (! Q.isEmpty) {
			val u = extractBest[V](Q, distances, bestDistanceFinder)
			neighborFinder(graph, u).map( _.otherVertex(u)).filter(! visited.contains(_)).foreach { v =>
				val alternateLength = distances(u).map( x => x + 1 )
				distances += (v -> bestDistanceFinder(alternateLength, distances(v)))
			}
		}
		IMap.empty[V,Option[Int]] ++ (distances)
	}
	
	def extractBest[V](
		set: MSet[V], 
		distances: MMap[V,Option[Int]],
		bestDistanceFinder: (Option[Int],Option[Int]) => Option[Int]
	): V = {
		val bestDistance = distances.values.toList.foldLeft(None: Option[Int])( (best, item) =>
			bestDistanceFinder(best, item)
		)
		val nextNode = set.find( v => 
			distances(v) == bestDistance
		).getOrElse(set.toList(0))
		set -= nextNode
		nextNode
	}
	
	def minOption(a: Option[Int], b: Option[Int]): Option[Int] = a match {
		case None => b
		case Some(x) => b match {
			case None => Some(x)
			case Some(y) => if (y < x) Some(y) else Some(x)
		}
	}
	def maxOption(a: Option[Int], b: Option[Int]): Option[Int] = a match {
		case None => b
		case Some(x) => b match {
			case None => Some(x)
			case Some(y) => if (y > x) Some(y) else Some(x)
		}
	}
	
	
}
