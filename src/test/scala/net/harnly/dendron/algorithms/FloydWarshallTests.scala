package net.harnly.dendron.algorithm

import org.specs._
import org.specs.runner.JUnit4
import org.specs.matcher.ScalacheckParameters._

class floydWarshallTest extends JUnit4(floydWarshallTest)
object floydWarshallTest extends Specification
{
	"Floyd Warshall" should {
		"calculate shortest-path with two undirected nodes" in {
			val g: Graph[Int,Edge[Int]] = new SimpleGraph(
				List(1,2),
				List( new SimpleEdge(1,2) )
			)
			val r = FloydWarshall.undirectedAllPairsShortestPath[
				Int,Edge[Int],Graph[Int,Edge[Int]]
			](g)

			r must_== Map(
				1 -> Map(
					1 -> Some(0),
					2 -> Some(1)
				), 
				2 -> Map(
					1 -> Some(1),
					2 -> Some(0)
				)
			) 
		}

		"calculate shortest-path with two directed nodes" in {
			val g: DirectedGraph[Int,DirectedEdge[Int]] = new SimpleDirectedGraph(
				List(1,2),
				List( new SimpleDirectedEdge(1,2) )
			)
			val r = FloydWarshall.directedAllPairsShortestPath[
				Int,DirectedEdge[Int],DirectedGraph[Int,DirectedEdge[Int]]
			](g)

			r must_== Map(
				1 -> Map(
					1 -> Some(0),
					2 -> Some(1)
				), 
				2 -> Map(
					1 -> None,
					2 -> Some(0)
				)
			) 
		}

	}
}
