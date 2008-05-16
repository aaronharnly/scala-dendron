package net.harnly.dendron
import net.harnly.dendron.datatypes.EdgeMap._
import net.harnly.dendron.algorithm.{Depth,Height}
import org.specs._
import org.specs.runner.JUnit4
import org.specs.matcher.ScalacheckParameters._


/** 
 * Basic graph specifications.
 */
class GraphSpecTest extends JUnit4(graphSpec)

object graphSpec extends Specification {
	import GraphGenerators._
	import GraphTypes._
	import org.scalacheck._

	"2.2.1 Directed Graph Model" should {
		"2.2.1.1 Have zero vertices" in {
			emptyGraph must pass { g: SimpleIntGraph =>
				g.vertices.size must be(0)
			}
		}
		
		"2.2.1.1 Have one vertex" in {
			Gen.oneOf(
				graphsWithOneIntAndOneEdge,
				graphsWithOneIntAndNoEdges
			) must pass { g: SimpleIntGraph =>
				g.vertices.size must be(1)
			}
		}

		"2.2.1.1 Have two vertices" in {
			Gen.oneOf(
				graphsWithTwoIntsAndOneEdge,
				graphsWithTwoIntsAndTwoEdges
			) must pass { tuple: (Int,Int,SimpleIntGraph) =>
				val (x,y,g) = tuple
				g.vertices.size must be(2)
			}
		}

		"2.2.1.2 Have zero edges" in {
			graphsWithOneIntAndNoEdges must pass { g: SimpleIntGraph =>
				g.edges.size must be(0)
			}
		}
		
		"2.2.1.2 Have one edge" in {
			graphsWithOneIntAndOneEdge must pass { g: SimpleIntGraph =>
				g.edges.size must be(1)
			}
		}

		"2.2.1.2 Have two edges" in {
			graphsWithTwoIntsAndTwoEdges must pass { tuple: (Int,Int,SimpleIntGraph) =>
				val (x,y,g) = tuple
				g.edges.size must be(2)
			}
		}

		"2.2.1.3 List outgoing edges from a vertex" in {
			graphsWithTwoIntsAndOneEdge must pass { tuple: (Int,Int,SimpleIntGraph) =>
				val (x,y,g) = tuple
				g.outgoingEdgesOf(x).size must be(1)
			}
			graphsWithTwoIntsAndOneEdge must pass { tuple: (Int,Int,SimpleIntGraph) =>
				val (x,y,g) = tuple
				g.outgoingEdgesOf(y).size must be(0)
			}
			graphsWithTwoIntsAndOneEdge must pass { tuple: (Int,Int,SimpleIntGraph) =>
				val (x,y,g) = tuple
				g.outgoingEdgesOf(x) must contain(SimpleDirectedEdge(x,y))
			}
		}

		"2.2.1.4 List incoming edges to a vertex" in {
			graphsWithTwoIntsAndOneEdge must pass { tuple: (Int,Int,SimpleIntGraph) =>
				val (x,y,g) = tuple
				g.incomingEdgesOf(x).size must be(0)
			}
			graphsWithTwoIntsAndOneEdge must pass { tuple: (Int,Int,SimpleIntGraph) =>
				val (x,y,g) = tuple
				g.incomingEdgesOf(y).size must be(1)
			}
			graphsWithTwoIntsAndOneEdge must pass { tuple: (Int,Int,SimpleIntGraph) =>
				val (x,y,g) = tuple
				g.incomingEdgesOf(y) must contain(SimpleDirectedEdge(x,y))
			}
		}

		"2.2.1.5 Add a vertex" in {
			graphsWithTwoIntsAndOneEdge must pass { tuple: (Int,Int,SimpleIntGraph) =>
				val (x,y,g) = tuple
				g.incomingEdgesOf(x).size must be(0)
			}
			graphsWithTwoIntsAndOneEdge must pass { tuple: (Int,Int,SimpleIntGraph) =>
				val (x,y,g) = tuple
				g.incomingEdgesOf(y).size must be(1)
			}
			graphsWithTwoIntsAndOneEdge must pass { tuple: (Int,Int,SimpleIntGraph) =>
				val (x,y,g) = tuple
				g.incomingEdgesOf(y) must contain(SimpleDirectedEdge(x,y))
			}
		}
		
		"3.4.1 Find one root" in {
			treeGen must pass { g: IntGraph =>
				val roots = DirectedGraph.rootsOf[Int,DirectedEdge[Int],IntGraph](g)
				roots.size must be(1)
			}
		}

		"3.4.1 Find one root" in {
			treeGen must pass { g: IntGraph =>
				val roots = DirectedGraph.rootsOf[Int,DirectedEdge[Int],IntGraph](g)
				roots.size must be(1)
			}
		}

		"3.4.1 Max height calculator" in {
			treeGen must pass { g: IntGraph =>
				val heights = Height.maxHeight(g, 1, dummyIntEdge: DirectedEdge[Int])
				heights must notHaveValue[Option[Int]](None)
			}
		}

		"3.4.2 Min height calculator" in {
			treeGen must pass { g: IntGraph =>
				val heights = Height.minHeight(g, 1, dummyIntEdge: DirectedEdge[Int])
				heights must notHaveValue[Option[Int]](None)
			}
		}

		"3.4.3 Max depth calculator" in {
			treeGen must pass { g: IntGraph =>
				val depths = Depth.maxDepth(g, 1, dummyIntEdge: DirectedEdge[Int])
				depths must notHaveValue[Option[Int]](None)
			}
		}

		"3.4.3 Min depth calculator" in {
			treeGen must pass { g: IntGraph =>
				val depths = Depth.minDepth(g, 1, dummyIntEdge: DirectedEdge[Int])
				depths must notHaveValue[Option[Int]](None)
			}
		}
	}
}