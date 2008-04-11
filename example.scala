#!/usr/bin/env scala
!#

import net.harnly.dendron._
import net.harnly.dendron.algorithm._
import net.harnly.dendron.io.dot._
import net.harnly.aaron.io._

val failureGraphs = List(
	SimpleDirectedGraph.fromPairs(Set(1098623, 247207, 8595841, 982604, 6308521, 4725962, 9461356, 645131),Set((9461356 -> 4725962), (247207 -> 6308521), (9461356 -> 8595841), (4725962 -> 982604), (1098623 -> 645131), (8595841 -> 1098623), (4725962 -> 247207))),
	SimpleDirectedGraph.fromPairs(Set(8368067, 6895290, 8552876, 3888906, 9888112, 8348262, 7884260, 3233611),Set((7884260 -> 3888906), (3233611 -> 8348262), (6895290 -> 7884260), (3888906 -> 8552876), (6895290 -> 8368067), (8368067 -> 9888112), (8368067 -> 3233611))),
	SimpleDirectedGraph.fromPairs(Set(959766, 2064381, 3640856, 1046311, 3834351, 4446699, 8031368, 8028715, 9662494),Set((9662494 -> 959766), (9662494 -> 3834351), (9662494 -> 8028715), (959766 -> 2064381), (3640856 -> 4446699), (959766 -> 1046311), (9662494 -> 8031368), (8028715 -> 3640856))),
	SimpleDirectedGraph.fromPairs(Set(6707106, 1397588, 2438909, 3207220, 5318982, 72252, 4070532, 3479124, 1537355),Set((1537355 -> 2438909), (3207220 -> 4070532), (1397588 -> 5318982), (3479124 -> 3207220), (1397588 -> 72252), (3479124 -> 1537355), (3479124 -> 6707106), (1537355 -> 1397588))),
	SimpleDirectedGraph.fromPairs(Set(9113659, 6981884, 3277194, 4801819, 2148911, 1459425, 2175920, 2592278, 2964088),Set((4801819 -> 1459425), (2175920 -> 2592278), (2148911 -> 9113659), (3277194 -> 2964088), (2964088 -> 2148911), (2148911 -> 6981884), (3277194 -> 2175920), (6981884 -> 4801819)))
)

var i = 1
failureGraphs.foreach { g =>
	val depth = Depth.maxDepth(g, 1, new SimpleDirectedEdge[Int](0,0))
	val vertexMetadata = Map(
		depth.map( pair =>
			pair._1 -> (Map("label" -> (pair._1.toString + " (" + pair._2 + ")")))
		).toList : _*
	)
	val formatter = new GraphFormatterDotDirectedTrivialWithVertexMetadata[Int, SimpleDirectedEdge[Int], SimpleDirectedGraph[Int,SimpleDirectedEdge[Int]]](
		vertexMetadata
	)
	val filename = "target/test-output/check." + i + ".dot"
	StringToFileWriter(filename)(formatter.format(g))
	i += 1
}

