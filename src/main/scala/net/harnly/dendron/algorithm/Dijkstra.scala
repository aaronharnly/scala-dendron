package net.harnly.dendron.algorithm
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
//		System.err.println("\n\n====== dijkstra ======")
//		System.err.println(graph + "\n")
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
//			System.err.println("Have queue: " + Q)
			val u = extractBest[V](Q, distances, bestDistanceFinder)
//			System.err.println("Extracted best vertex: " + u)
			neighborFinder(graph, u).map( _.otherVertex(u)).filter(! visited.contains(_)).foreach { v =>
//				System.err.println("\tConsidering neighbor " + v)
				val alternateLength = distances(u).map( x => x + 1 )
				val bestDistance = bestDistanceFinder(alternateLength, distances(v))
//				System.err.println("\t\t-Between known distance " + distances(v) + " and alternative " + alternateLength + ", chose " + bestDistance)
				distances += (v -> bestDistance)
			}
		}
//		System.err.println("----- done ------\n")
		IMap.empty[V,Option[Int]] ++ (distances)
	}
	
	def extractBest[V](
		set: MSet[V], 
		distances: MMap[V,Option[Int]],
		bestDistanceFinder: (Option[Int],Option[Int]) => Option[Int]
	): V = {
//		System.err.println("\tKnown distances: " + distances)
		val startingTuple: (Option[V],Option[Int]) = ((None,None))
		val bestNodeAndDistance = set.foldLeft( startingTuple ){ (best, v) =>
			val thisNodesDistance = distances(v)
			val bestDistance = bestDistanceFinder(best._2, thisNodesDistance)
			if (thisNodesDistance == bestDistance)
				((Some(v), thisNodesDistance))
			else
				best
		}
		
		val nextNode = bestNodeAndDistance._1.get
		set -= nextNode
//		System.err.println("\tBest distance is: " + bestNodeAndDistance._2 + " at " + nextNode)
		nextNode
	}
	
	
}
