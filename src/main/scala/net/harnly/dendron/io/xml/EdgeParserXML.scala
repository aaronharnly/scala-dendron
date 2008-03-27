package net.harnly.dendron.io.xml
import scala.collection.{Map}
import scala.xml.{Node,MetaData}
import net.harnly.aaron.extensions.SequenceOptionExtensions._
import net.harnly.aaron.utilities.XMLUtilities._
import net.harnly.aaron.extensions.Function1RightAssociativeExtensions._

abstract class EdgeParserXML[V,E <: Edge[V]](
	firstVertexLabel: String,
	secondVertexLabel: String
)
extends EdgeParserWithMetadata[Node,Node,String,MetaData,Map[String,String],V,E]
{
	def parseMetadata(input: MetaData): Option[Map[String,String]] = Some(metadataToMap(input))

	def splitEdge(edgeInput: Node): Option[(Node,Node,MetaData)] = for(
		v1 <- seq2Option(edgeInput \ firstVertexLabel);
		v2 <- seq2Option(edgeInput \ secondVertexLabel)
	) yield ((v1, v2, edgeInput.attributes))
	
}

abstract class EdgeParserXMLUndirected[V, E <: DirectedEdge[V]]
extends EdgeParserXML[V,E](
	"oneVertex","otherVertex"
)

abstract class EdgeParserXMLDirected[V, E <: DirectedEdge[V]]
extends EdgeParserXML[V,E](
	"tail","head"
)
