package net.harnly.dendron.algorithm
import scala.collection.mutable.{Map => MMap, HashMap => MHashMap, Set => MSet, HashSet => MHashSet}
import scala.collection.immutable.{Map => IMap, HashMap => IHashMap}
import net.harnly.dendron.datatypes.{IndexedArray2}

object FloydWarshallWithArray
{
	def undirectedAllPairsShortestPath[V, E <: Edge[V], G <: Graph[V,E]](
		graph: G
	): IndexedArray2[V,Int] = allPairsShortestPath(
		graph,
		Math.min,
		Graph.neighbors[V,E,G],
		Graph.edgeBetween[V,E,G]
	)

	def directedAllPairsShortestPath[V, E <: DirectedEdge[V], G <: DirectedGraph[V,E]](
		graph: G
	): IndexedArray2[V,Int] = allPairsShortestPath(
		graph,
		Math.min,
		DirectedGraph.neighbors[V,E,G],
		DirectedGraph.edgeBetween[V,E,G]
	)
	
	def allPairsShortestPath[V, E <: Edge[V], G <: Graph[V,E]](
		graph: G,
		bestDistanceFinder: (Int,Int) => Int,
		neighborFinder: (G,V) => Set[E],
		edgeFinder: (G,V,V) => Option[E]
	): IndexedArray2[V,Int] = {
		
		// Initialize the index
		val vertices = graph.vertices.toList
		val vertexIndex = Map( vertices.zipWithIndex : _* )
		val totalVertices = vertices.length
		
		// Initialize the distance array
		val distances = new Array[Array[Int]](totalVertices,totalVertices)
		System.err.println("Array-backed FloydWarshall: Initializing matrix...")
		for (v1 <- vertices)
			for (v2 <- vertices) {
				val i1 = vertexIndex(v1)
				val i2 = vertexIndex(v2)
				distances(i1)(i2) = 
				if (i1 == i2 || v1 == v2) 
					0 
				else 
					edgeFinder(graph, v1, v2) match {
						case None => Integer.MAX_VALUE
						case Some(e) => 1
					}
			}
		System.err.println("FloydWarshall: done.")
		dumpArray(distances)

		var i = 1
		
		for (
			k <- vertices
		){
				val i_k = vertexIndex(k)
				System.err.println("Refining: Step " + i + " of " + totalVertices)
				for (
					v1 <- vertices;
					v2 <- vertices
				) {
					val i1 = vertexIndex(v1)
					val i2 = vertexIndex(v2)
					
					distances(i1)(i2) = bestDistanceFinder( 
							distances(i1)(i2), 
							addDistances(distances(i1)(i_k), distances(i_k)(i2))
					)
				}
//				dumpArray(distances)
//				System.err.println("---------")
				i += 1
		}
		IndexedArray2[V,Int](distances,vertexIndex)
	}	
	
	def addDistances(a: Int, b: Int): Int = 
	if (a == Integer.MAX_VALUE || b == Integer.MAX_VALUE)
		Integer.MAX_VALUE
	else
		a + b
	
	def dumpArray[A](input: Array[Array[A]]) {
		input.foreach( r => 
			System.err.println(r.mkString(",")) 
		)
	}
}