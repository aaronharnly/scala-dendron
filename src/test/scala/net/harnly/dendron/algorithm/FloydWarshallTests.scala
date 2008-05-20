package net.harnly.dendron.algorithm

import org.specs._
import org.specs.runner.JUnit4
import org.specs.matcher.ScalacheckParameters._

class floydWarshallTest extends JUnit4(floydWarshallTest)
object floydWarshallTest extends Specification
{
	import GraphGenerators._
	import GraphTypes._

	
	"Floyd Warshall with array" should {
		"calculate shortest-path with two undirected nodes" in {
			val g: IntUnGraph = new SimpleGraph(
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
			val g: IntGraph = new SimpleDirectedGraph(
				List(1,2),
				List( new SimpleDirectedEdge(1,2) )
			)
			val r = FloydWarshall.directedAllPairsShortestPath[
				Int,DirectedEdge[Int],IntGraph
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
			val r = FloydWarshall.directedAllPairsShortestPath[Int,DirectedEdge[Int],IntGraph](g)

			r must_== Map(
				1 -> Map(
					1 -> Some(0),
					2 -> Some(1),
					3 -> Some(1),
					4 -> None,
					5 -> Some(2)
				), 
				2 -> Map(
					1 -> None,
					2 -> Some(0),
					3 -> None,
					4 -> None,
					5 -> None
				), 
				3 -> Map(
					1 -> None,
					2 -> None,
					3 -> Some(0),
					4 -> None,
					5 -> Some(1)
				), 
				4 -> Map(
					1 -> None,
					2 -> None,
					3 -> None,
					4 -> Some(0),
					5 -> Some(1)
				), 
				5 -> Map(
					1 -> None,
					2 -> None,
					3 -> None,
					4 -> None,
					5 -> Some(0)
				)
			) 
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
			val r = FloydWarshall.directedAllPairsShortestPath[Int,DirectedEdge[Int],IntGraph](g)

			r must_== Map(
				1 -> Map(
					1 -> Some(0),
					2 -> Some(1),
					3 -> Some(1),
					4 -> Some(2),
					5 -> Some(2)
				), 
				2 -> Map(
					1 -> None,
					2 -> Some(0),
					3 -> None,
					4 -> Some(1),
					5 -> None
				), 
				3 -> Map(
					1 -> None,
					2 -> None,
					3 -> Some(0),
					4 -> Some(2),
					5 -> Some(1)
				), 
				4 -> Map(
					1 -> None,
					2 -> None,
					3 -> None,
					4 -> Some(0),
					5 -> None
				), 
				5 -> Map(
					1 -> None,
					2 -> None,
					3 -> None,
					4 -> Some(1),
					5 -> Some(0)
				)
			) 
		}

	}
}
