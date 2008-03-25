package net.harnly.dendron.io.xml
import scala.collection.{Map}
import scala.xml.{Node}
import net.harnly.aaron.extensions.SequenceOptionExtensions._
import net.harnly.aaron.utilities.XMLUtilities._

class XMLEdgeParser[V, E <: Edge[V]](
	firstVertexLabel: String,
	secondVertexLabel: String,
	override val edgeFactory: ((V,V,Map[String,String])) => Option[E],
	override val defaultVertexParser: VertexParser[Node,V],
	override val vertexParsers: VertexParser[Node,V]*
)
extends VertexPairAndContextSplitterEdgeParser[Node,Node,Map[String,String],V,E](
	edgeFactory,
	EdgeNodeSplitter(firstVertexLabel, secondVertexLabel),
	defaultVertexParser,
	vertexParsers : _*
)

case class EdgeNodeSplitter(
	firstVertexLabel: String,
	secondVertexLabel: String
)
extends Function1[Node, Option[((Node,Node,Map[String,String]))]]
{
	def apply(input: Node): Option[((Node,Node,Map[String,String]))] =
	{
		val metadataMap = metadataToMap(input.attributes)
		val oneVertex = seq2Option(input \ firstVertexLabel)
		val otherVertex = seq2Option(input \ secondVertexLabel)
		oneVertex match {
			case None => None
			case Some(v1) => otherVertex match {
				case None => None
				case Some(v2) => Some(
					((v1, v2, metadataMap))
				)
			}
		}
	}	
}

class XMLUndirectedEdgeParser[V, E <: DirectedEdge[V]](
	edgeFactory: ((V,V,Map[String,String])) => Option[E],
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
	edgeFactory: ((V,V,Map[String,String])) => Option[E],
	defaultVertexParser: VertexParser[Node,V],
	vertexParsers: VertexParser[Node,V]*
)
extends XMLEdgeParser[V,E](
	"tail","head",
	edgeFactory,
	defaultVertexParser,
	vertexParsers : _*
)
