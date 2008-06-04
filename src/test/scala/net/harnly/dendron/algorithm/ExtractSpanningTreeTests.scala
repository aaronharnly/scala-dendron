package net.harnly.dendron.algorithm

import org.specs._
import org.specs.runner.JUnit4
import org.specs.matcher.ScalacheckParameters._

class extractSpanningTreeTest extends JUnit4(extractSpanningTreeSpec)

object extractSpanningTreeSpec extends Specification
{
	import GraphGenerators._
	import GraphTypes._
	
	"Path to root" should {
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
	
	"Trimmed tree" should {
		"be just a single node for single-node graph" in {
			graphsWithOneIntAndNoEdges must pass { g: SimpleIntGraph =>
				val vertex = g.vertices.toList.head
				ExtractSpanningTree.trimToRoot[Int,SimpleDirectedEdge[Int],SimpleIntGraph](
					g, vertex
				) must_== SimpleDirectedGraph.fromPairs(
					Set(vertex),
					Set()
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
						Set()
					)
			}}
		}
		
	}
}