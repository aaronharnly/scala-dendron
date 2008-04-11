package net.harnly.dendron.test
import net.harnly.dendron._
import net.harnly.dendron.datatypes.EdgeMap._
import net.harnly.dendron.algorithm.{Depth,Height,FindLeaves,FindRoots}
import org.scalacheck._
import Arbitrary._
import Prop._
import org.specs._
import org.specs.runner.JUnit4
import org.specs.matcher.ScalacheckParameters._

object GraphTypes
{
	type IntGraph = DirectedGraph[Int,DirectedEdge[Int]]
	type SimpleIntGraph = SimpleDirectedGraph[Int,SimpleDirectedEdge[Int]]	
}

object GraphGenerators
{
	import GraphTypes._
	
	val emptyGraph = Gen.elements(new SimpleIntGraph(
		List(),
		List()
	))
	
	val graphsWithOneIntAndNoEdges = for(
		x <- arbitrary[Int]
	) yield new SimpleIntGraph(
		List(x),
		List()
	)

	val graphsWithOneIntAndOneEdge = for(
		x <- arbitrary[Int]
	) yield new SimpleIntGraph(
		List(x),
		List(SimpleDirectedEdge(x,x))
	)
	
	val graphsWithTwoIntsAndTwoEdges = for(
		x <- arbitrary[Int];
		y <- arbitrary[Int] if x != y
	) yield ((x, y, new SimpleIntGraph(
		List(x,y),
		List(
			SimpleDirectedEdge(x,y),
			SimpleDirectedEdge(y,x)
		)
	)))

	val graphsWithTwoIntsAndOneEdge = for(
		x <- arbitrary[Int];
		y <- arbitrary[Int] if x != y
	) yield ((x, y, new SimpleIntGraph(
		List(x,y),
		List(
			SimpleDirectedEdge(x,y)
		)
	)))

	def makeATree[T](gen: Gen[T])(p: Gen.Params): Option[DirectedGraph[T,DirectedEdge[T]]] = 
	p.size match { 
		case 0 => None

		case 1 => for(
			val v <- gen.apply(p)
		) yield new SimpleDirectedGraph(
			List(v), 
			List[SimpleDirectedEdge[T]]() 
		)

		case _ => for(
			val tree <- makeATree[T](gen)( Gen.Params( p.size - 1, p.rand ) );
			val v <- gen.apply(p);
			val source <- Gen.elements( tree.vertices.toList : _* ).sample
		) yield tree.addVertex(v).addEdge(
			new SimpleDirectedEdge[T]( 
				source, 
				v
			)
		)
	}
	def makeAnIntTree(p: Gen.Params) = makeATree[Int](Gen.choose(1,10000000))(p)
	val treeGen = new Gen(makeAnIntTree)
	
	val dummyIntEdge = new SimpleDirectedEdge(1,1)
}

/** 
 * Basic graph specifications.
 */
class GraphSpecTest extends JUnit4(graphSpec)

object graphSpec extends Specification {
	import GraphGenerators._
	import GraphTypes._

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
				val roots = FindRoots[Int,DirectedEdge[Int],IntGraph](g)
				roots.size must be(1)
			}
		}

		"3.4.1 Find one root" in {
			treeGen must pass { g: IntGraph =>
				val roots = FindRoots[Int,DirectedEdge[Int],IntGraph](g)
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