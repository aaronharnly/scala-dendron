package net.harnly.dendron.io

class EdgeSequenceGraphParser[
	Tgraph,
	Tedge,
	Tvertex,
	V, 
	E <: DirectedEdge[V],
	G <: DirectedGraph[V,E]
](
	graphFactory: (Set[V],Set[E]) => G,
	edgeExtractor: Tgraph => Iterable[Tedge],
	val defaultEdgeParser: EdgeParser[Tedge,Tvertex,V,E],
	val edgeParsers: EdgeParser[Tedge,Tvertex,V,E]*
)
extends GraphParser[Tgraph,Tedge,Tvertex,V,E,G]
{
	def parse(input: T2 forSome {type T2 <: Tgraph}): Option[G] = {
		val edges = edgeExtractor(input).flatMap( e =>
			parserForEdge(e).parse(e)
		)
		val edgeSet = Set.empty[E] ++ edges
		
		val vertices = edges.foldLeft(Set.empty[V])( (set,edge) => {
			val oneVertex = edge.oneVertex
			val otherVertex = edge.otherVertex(oneVertex)
			set + oneVertex + otherVertex
		})
		Some(
			graphFactory(vertices, edgeSet)
		)
	}
}
