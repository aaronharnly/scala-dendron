package net.harnly.dendron.io.edgelist

object DirectedEdgelistParser
extends GraphParser[
	Seq[String],
	String,
	DirectedEdge[String],
	DirectedGraph[String,DirectedEdge[String]]
]
{
	def parse(input: A2 forSome {type A2 <: Seq[String]}): DirectedGraph[String,DirectedEdge[String]] = {
		SimpleDirectedGraph.empty[String,DirectedEdge[String]]
	}
}


