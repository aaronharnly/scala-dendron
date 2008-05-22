package net.harnly.dendron.algorithm

import org.specs._
import org.specs.runner.JUnit4
import org.specs.matcher.ScalacheckParameters._

class totalDescendantsTest extends JUnit4(totalDescendantsTest)

object totalDescendantsTest extends Specification
{
	import GraphGenerators._
	import GraphTypes._
	
	"Total descendants" should {
		val descender = new TotalDescendants[Int,SimpleDirectedEdge[Int],SimpleIntGraph]

		"be correct with 1 node" in {
			graphsWithOneIntAndNoEdges must pass { g: SimpleIntGraph =>
				descender(g) must_== Map(g.vertices.toList.head -> 0)
			}
		}
		"be correct with 2 nodes" in {
			graphsWithTwoIntsAndOneEdge must pass { tuple: (Int,Int,SimpleIntGraph) => tuple match { 
				case (source, target, g) =>
					descender(g) must_== Map(source -> 1, target -> 0)
				}
			}
		}
		"be correct with multiple parents" in {
			val g = SimpleDirectedGraph.fromPairs[Int](
				Set(1,2,3,4,5,6),
				Set(
					1 -> 2,
					1 -> 3,
					2 -> 4,
					3 -> 4,
					4 -> 5,
					4 -> 6
				)
			)
			
			descender(g) must_== Map(
				1 -> 5,
				2 -> 3,
				3 -> 3,
				4 -> 2,
				5 -> 0,
				6 -> 0
			)
		}
	}
}