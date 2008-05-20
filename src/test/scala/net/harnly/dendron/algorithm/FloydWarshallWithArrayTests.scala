package net.harnly.dendron.algorithm

import org.specs._
import org.specs.runner.JUnit4
import org.specs.matcher.ScalacheckParameters._

class floydWarshallWithArrayTest extends JUnit4(floydWarshallWithArrayTest)
object floydWarshallWithArrayTest extends Specification
{
	import GraphGenerators._
	import GraphTypes._

	"Floyd Warshall with array" should {
		"calculate shortest-path with two undirected nodes" in {
			val g: IntUnGraph = new SimpleGraph(
				List(1,2),
				List( new SimpleEdge(1,2) )
			)
			val r = FloydWarshallWithArray.undirectedAllPairsShortestPath[
				Int,Edge[Int],Graph[Int,Edge[Int]]
			](g)

			r(1,1) must_== 0
			r(1,2) must_== 1
			r(2,1) must_== 1
			r(2,2) must_== 0
		}

		"calculate shortest-path with two directed nodes" in {
			val g: IntGraph = new SimpleDirectedGraph(
				List(1,2),
				List( new SimpleDirectedEdge(1,2) )
			)
			val r = FloydWarshallWithArray.directedAllPairsShortestPath[
				Int,DirectedEdge[Int],IntGraph
			](g)
			
			r(1,1) must_== 0
			r(1,2) must_== 1
			r(2,1) must_== Integer.MAX_VALUE
			r(2,2) must_== 0
		}

		"calculate shortest-path with multiple directed parents" in {
			val g: IntGraph = new SimpleDirectedGraph(
				List(1,2,3,4,5),
				List( 
					new SimpleDirectedEdge(1,2),
					new SimpleDirectedEdge(1,3),
					new SimpleDirectedEdge(3,5),
					new SimpleDirectedEdge(4,5) 
				)
			)
			val r = FloydWarshallWithArray.directedAllPairsShortestPath[Int,DirectedEdge[Int],IntGraph](g)

			r(1,1) must_== 0
			r(1,2) must_== 1
			r(1,3) must_== 1
			r(1,4) must_== Integer.MAX_VALUE
			r(1,5) must_== 2

			r(2,1) must_== Integer.MAX_VALUE
			r(2,2) must_== 0
			r(2,3) must_== Integer.MAX_VALUE
			r(2,4) must_== Integer.MAX_VALUE
			r(2,5) must_== Integer.MAX_VALUE

			r(3,1) must_== Integer.MAX_VALUE
			r(3,2) must_== Integer.MAX_VALUE
			r(3,3) must_== 0
			r(3,4) must_== Integer.MAX_VALUE
			r(3,5) must_== 1

			r(4,1) must_== Integer.MAX_VALUE
			r(4,2) must_== Integer.MAX_VALUE
			r(4,3) must_== Integer.MAX_VALUE
			r(4,4) must_== 0
			r(4,5) must_== 1

			r(5,1) must_== Integer.MAX_VALUE
			r(5,2) must_== Integer.MAX_VALUE
			r(5,3) must_== Integer.MAX_VALUE
			r(5,4) must_== Integer.MAX_VALUE
			r(5,5) must_== 0
		}


		"calculate shortest-path with multiple directed parents and a shortcut" in {
			val g: IntGraph = new SimpleDirectedGraph(
				List(1,2,3,4,5),
				List( 
					new SimpleDirectedEdge(1,2),
					new SimpleDirectedEdge(1,3),
					new SimpleDirectedEdge(2,4),
					new SimpleDirectedEdge(3,5), 
					new SimpleDirectedEdge(5,4) 
				)
			)
			val r = FloydWarshallWithArray.directedAllPairsShortestPath[Int,DirectedEdge[Int],IntGraph](g)

			r(1,1) must_== 0
			r(1,2) must_== 1
			r(1,3) must_== 1
			r(1,4) must_== 2
			r(1,5) must_== 2

			r(2,1) must_== Integer.MAX_VALUE
			r(2,2) must_== 0
			r(2,3) must_== Integer.MAX_VALUE
			r(2,4) must_== 1
			r(2,5) must_== Integer.MAX_VALUE

			r(3,1) must_== Integer.MAX_VALUE
			r(3,2) must_== Integer.MAX_VALUE
			r(3,3) must_== 0
			r(3,4) must_== 2
			r(3,5) must_== 1

			r(4,1) must_== Integer.MAX_VALUE
			r(4,2) must_== Integer.MAX_VALUE
			r(4,3) must_== Integer.MAX_VALUE
			r(4,4) must_== 0
			r(4,5) must_== Integer.MAX_VALUE

			r(5,1) must_== Integer.MAX_VALUE
			r(5,2) must_== Integer.MAX_VALUE
			r(5,3) must_== Integer.MAX_VALUE
			r(5,4) must_== 1
			r(5,5) must_== 0
		}

	}
}
