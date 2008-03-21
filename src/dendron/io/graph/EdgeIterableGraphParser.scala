package net.harnly.dendron.io

case class EdgeIterableGraphParser[
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
	def apply(input: Tgraph): Option[G] = {
		val edges = edgeExtractor(input).flatMap( e =>
			parserForEdge(e).parse(e)
		)
		val edgeSet = Set.empty[E] ++ edges
		
		val vertices = edges.foldLeft(Set.empty[V])( (set,edge) => 
			set ++ edge.vertices
		)
		Some(
			graphFactory(vertices, edgeSet)
		)
	}
}
