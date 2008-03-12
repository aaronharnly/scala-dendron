package net.harnly.dendron.io.xml
import scala.xml.{Node}
import net.harnly.aaron.extensions.SequenceOptionExtensions._

class XMLEdgeParser[V, E <: Edge[V]](
	firstVertexLabel: String,
	secondVertexLabel: String,
	edgeFactory: ((V,V)) => Option[E],
	defaultVertexParser: VertexParser[Node,V],
	vertexParsers: VertexParser[Node,V]*
)
extends VertexPairSplitterEdgeParser[Node,Node,V,E](
	edgeFactory,
	{ input: Node =>
		val oneVertex = seq2Option(input \ firstVertexLabel)
		val otherVertex = seq2Option(input \ secondVertexLabel)
		oneVertex match {
			case None => None
			case Some(v1) => otherVertex match {
				case None => None
				case Some(v2) => Some(
					((v1, v2))
				)
			}
		}
	},
	defaultVertexParser,
	vertexParsers : _*
)

class XMLUndirectedEdgeParser[V, E <: DirectedEdge[V]](
	edgeFactory: ((V,V)) => Option[E],
	defaultVertexParser: VertexParser[Node,V],
	vertexParsers: VertexParser[Node,V]*
)
extends XMLEdgeParser[V,E](
	"oneVertex","otherVertex",
	edgeFactory,
	defaultVertexParser,
	vertexParsers : _*
)

class XMLDirectedEdgeParser[V, E <: DirectedEdge[V]](
	edgeFactory: ((V,V)) => Option[E],
	defaultVertexParser: VertexParser[Node,V],
	vertexParsers: VertexParser[Node,V]*
)
extends XMLEdgeParser[V,E](
	"tail","head",
	edgeFactory,
	defaultVertexParser,
	vertexParsers : _*
)
