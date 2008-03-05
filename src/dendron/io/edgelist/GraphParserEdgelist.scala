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
		val edges = input.flatMap { line =>
			val linePieces = line.trim.split("\t")
			if (linePieces.length == 2)
				Some(
					SimpleDirectedEdge(
						linePieces(0),
						linePieces(1)
					)
				)
			else
				None
		}
		
		SimpleDirectedGraph.empty[String,DirectedEdge[String]].addEdges(
			edges : _*
		)
	}
}


