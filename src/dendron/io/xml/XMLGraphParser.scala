package net.harnly.dendron.io.xml
import scala.xml.{Node}
import net.harnly.aaron.extensions.SequenceOptionExtensions._

class XMLGraphParser[
	V, 
	E <: Edge[V],
	G <: DirectedGraph[V,E]
](
	graphFactory: (Set[V],Set[E]) => G,
	val defaultEdgeParser: EdgeParser[Node,Node,V,E],
	val edgeParsers: EdgeParser[Node,Node,V,E]*
)
extends GraphParser[
	Node, Node, Node,
	V,E,G
]
{
	def apply(input: Node): Option[G] = {
		val graphInfo = seq2Option(input \ "graphInfo")
		graphInfo.map { info =>
			val verticesNotInEdges = Set(
				seq2Option(input \ "vertices").map( vNode =>
					vNode.child.flatMap( v =>
						defaultEdgeParser.parserForVertex(v).parse(v)
						)
					).getOrElse(Nil) : _*
				)

			val edges = Set(
				seq2Option(input \ "edges").map( eNode =>
						eNode.child.flatMap( e =>
							parserForEdge(e).parse(e)
						)
				).getOrElse(Nil) : _*
			)

			val verticesInEdges = edges.foldLeft(Set.empty[V])( (set, edge) =>
				set ++ edge
			)

			val allVertices = verticesNotInEdges ++ verticesInEdges

			graphFactory(allVertices, edges)
		}
	}
}

