package net.harnly.dendron.io.edgelist

class StringEdgelistGraphParser[
	V, 
	E <: DirectedEdge[V],
	G <: DirectedGraph[V,E]
](
	graphFactory: (Set[V],Set[E]) => G,
	edgeFactory: (V,V) => E,
	vertexFactory: String => V
)
extends EdgeSequenceGraphParser[
	Seq[String],(String,String),String,
	V,E,G
](
	graphFactory,
	{ lines: Seq[String] => lines.map{ line =>
		val pieces = line.trim.split("\t")
		((pieces(0),pieces(1)))
	}},
	new VertexPairEdgeParser[String,V,E](
		edgeFactory,
		SimpleVertexParser[String,V](
			vertexFactory
		)
	)
){
	
}

