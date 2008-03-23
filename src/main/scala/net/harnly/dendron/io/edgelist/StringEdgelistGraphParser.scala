package net.harnly.dendron.io.edgelist

class StringEdgelistGraphParser[
	V, 
	E <: DirectedEdge[V],
	G <: DirectedGraph[V,E]
](
	graphFactory: (Set[V],Set[E]) => G,
	edgeFactory: ((V,V)) => Option[E],
	vertexFactory: String => V
)
extends EdgeIterableGraphParser[
	Iterable[String],((String,String)),String,
	V,E,G
](
	graphFactory,
	{ lines: Iterable[String] => lines.flatMap{ line =>
		val pieces = line.trim.split("\t")
		if (pieces.length == 2)
			Some(
				((pieces(0),pieces(1)))
			)
		else
			None
	}},
	new VertexPairEdgeParser[String,V,E](
		edgeFactory,
		SimpleVertexParser[String,V](
			vertexFactory
		)
	)
)


object SimpleDirectedStringEdgelistGraphParser
extends StringEdgelistGraphParser[
	String,
	DirectedEdge[String],
	DirectedGraph[String,DirectedEdge[String]]
](
	{
		(v: Set[String], e: Set[DirectedEdge[String]]) =>
			new SimpleDirectedGraph(v, e)
	},
	{
		vpair: (String,String) =>
			Some(SimpleDirectedEdge(vpair._1, vpair._2))
	},
	identity[String]
)
