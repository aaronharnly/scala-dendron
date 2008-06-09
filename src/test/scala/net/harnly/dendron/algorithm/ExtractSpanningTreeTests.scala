package net.harnly.dendron.algorithm

import org.specs._
import org.specs.runner.JUnit4
import org.specs.matcher.ScalacheckParameters._

import net.harnly.aaron.logging.{LazyLogging}

class extractSpanningTreeTest extends JUnit4(extractSpanningTreeSpec)

object extractSpanningTreeSpec extends Specification with LazyLogging
{
	import GraphGenerators._
	import GraphTypes._
	
	"pathsToRoot" should {
		"be just a single node for single-node graph" in {
			graphsWithOneIntAndNoEdges must pass { g: SimpleIntGraph =>
				val vertex = g.vertices.toList.head
				ExtractSpanningTree.pathsToRoot[Int,SimpleDirectedEdge[Int],SimpleIntGraph](
					g, vertex
				) must_== Set(List(vertex))
			}
		}
		"include both nodes in a two-node graph" in {
			graphsWithTwoIntsAndOneEdge must pass { tuple: (Int,Int,SimpleIntGraph) => tuple match { 
				case (source, target, g) =>
					ExtractSpanningTree.pathsToRoot[Int,SimpleDirectedEdge[Int],SimpleIntGraph](
						g, target
					) must_== Set(List(target,source))
			}}
		}
	}
	
	"pathToRoot" should {
		"be just a single node for single-node graph" in {
			graphsWithOneIntAndNoEdges must pass { g: SimpleIntGraph =>
				val vertex = g.vertices.toList.head
				ExtractSpanningTree.pathToRoot[Int,SimpleDirectedEdge[Int],SimpleIntGraph](
					g, vertex
				) must_== Set(List(vertex))
			}
		}
		"include both nodes in a two-node graph" in {
			graphsWithTwoIntsAndOneEdge must pass { tuple: (Int,Int,SimpleIntGraph) => tuple match { 
				case (source, target, g) =>
					ExtractSpanningTree.pathToRoot[Int,SimpleDirectedEdge[Int],SimpleIntGraph](
						g, target
					) must_== Set(List(target,source))
			}}
		}
	}
	
	
	"trimToRoot" should {
		"be just a single node for single-node graph" in {
			graphsWithOneIntAndNoEdges must pass { g: SimpleIntGraph =>
				val vertex = g.vertices.toList.head
				ExtractSpanningTree.trimToRoot[Int,SimpleDirectedEdge[Int],SimpleIntGraph](
					g, vertex
				) must_== SimpleDirectedGraph.fromPairs(
					Set(vertex),
					Set.empty[(Int,Int)]
				)
			}
		}
		"be both nodes for two-node graph, starting from leaf" in {
			graphsWithTwoIntsAndOneEdge must pass { tuple: (Int,Int,SimpleIntGraph) => tuple match { 
				case (source, target, g) =>
					ExtractSpanningTree.trimToRoot[Int,SimpleDirectedEdge[Int],SimpleIntGraph](
						g, target
					) must_== SimpleDirectedGraph.fromPairs(
						Set(source,target),
						Set(source -> target)
					)
			}}
		}
		"be just root for two-node graph, starting from root" in {
			graphsWithTwoIntsAndOneEdge must pass { tuple: (Int,Int,SimpleIntGraph) => tuple match { 
				case (source, target, g) =>
					ExtractSpanningTree.trimToRoot[Int,SimpleDirectedEdge[Int],SimpleIntGraph](
						g, source
					) must_== SimpleDirectedGraph.fromPairs(
						Set(source),
						Set.empty[(Int,Int)]
					)
			}}
		}
		"be full graph for diamond graph" in {
			val g = SimpleDirectedGraph.fromPairs(
				Set(1,2,3,4),
				Set(
					1 -> 2,
					1 -> 3,
					2 -> 4,
					3 -> 4
				)
			)
			ExtractSpanningTree.trimToRoot[Int,SimpleDirectedEdge[Int],SimpleIntGraph](
				g, 4
			) must_== SimpleDirectedGraph.fromPairs(
				Set(1,2,3,4),
				Set(
					1 -> 2,
					1 -> 3,
					2 -> 4,
					3 -> 4
				)
			)
		}

		"be three nodes for diamond graph with flap" in {
			val g = SimpleDirectedGraph.fromPairs(
				Set(1,2,3,4,5),
				Set(
					1 -> 2,
					1 -> 3,
					2 -> 4,
					3 -> 4,
					3 -> 5
				)
			)
			ExtractSpanningTree.trimToRoot[Int,SimpleDirectedEdge[Int],SimpleIntGraph](
				g, 5
			) must_== SimpleDirectedGraph.fromPairs(
				Set(1,3,5),
				Set(
					1 -> 3,
					3 -> 5
				)
			)
		}
	}
	
	"trimAggressivelyToRoot" should {
		"be just a single node for single-node graph" in {
			graphsWithOneIntAndNoEdges must pass { g: SimpleIntGraph =>
				val vertex = g.vertices.toList.head
				ExtractSpanningTree.trimAggressivelyToRoot[Int,SimpleDirectedEdge[Int],SimpleIntGraph](
					g, vertex
				) must_== SimpleDirectedGraph.fromPairs(
					Set(vertex),
					Set.empty[(Int,Int)]
				)
			}
		}
		"be both nodes for two-node graph, starting from leaf" in {
			graphsWithTwoIntsAndOneEdge must pass { tuple: (Int,Int,SimpleIntGraph) => tuple match { 
				case (source, target, g) =>
					ExtractSpanningTree.trimAggressivelyToRoot[Int,SimpleDirectedEdge[Int],SimpleIntGraph](
						g, target
					) must_== SimpleDirectedGraph.fromPairs(
						Set(source,target),
						Set(source -> target)
					)
			}}
		}
		"be just root for two-node graph, starting from root" in {
			graphsWithTwoIntsAndOneEdge must pass { tuple: (Int,Int,SimpleIntGraph) => tuple match { 
				case (source, target, g) =>
					ExtractSpanningTree.trimAggressivelyToRoot[Int,SimpleDirectedEdge[Int],SimpleIntGraph](
						g, source
					) must_== SimpleDirectedGraph.fromPairs(
						Set(source),
						Set.empty[(Int,Int)]
					)
			}}
		}
		"be just one side for diamond graph" in {
			val g = SimpleDirectedGraph.fromPairs(
				Set(1,2,3,4),
				Set(
					1 -> 2,
					1 -> 3,
					2 -> 4,
					3 -> 4
				)
			)
			ExtractSpanningTree.trimAggressivelyToRoot[Int,SimpleDirectedEdge[Int],SimpleIntGraph](
				g, 4
			) must_== SimpleDirectedGraph.fromPairs(
				Set(1,2,4),
				Set(
					1 -> 2,
					2 -> 4
				)
			)
		}
		"Scale to large sizes" in {
			lineGen must pass { g: IntGraph =>
				import org.apache.log4j.{BasicConfigurator,Logger,Level}
				BasicConfigurator.configure
				Logger.getRootLogger.setLevel(Level.INFO)
				
				val leaves = DirectedGraph.leavesOf[Int,DirectedEdge[Int],IntGraph](g)
				leaves.foreach { leaf =>
					val trimmed = ExtractSpanningTree.trimAggressivelyToRoot[Int,DirectedEdge[Int],IntGraph](g,leaf)
					val str = g.toString // i have no idea why this line helps us pass the test
					trimmed must_== g
				}
				val roots = DirectedGraph.rootsOf[Int,DirectedEdge[Int],IntGraph](g)
				roots.foreach { root =>
					ExtractSpanningTree.trimToRoot[Int,DirectedEdge[Int],IntGraph](g,root) must_== SimpleDirectedGraph.fromPairs(
						Set(root),
						Set.empty[(Int,Int)]
					)
				}
				true
			}(set(minSize -> 0, maxSize -> 3000))
		}
		
	}
}