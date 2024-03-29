package net.harnly.dendron.io.xml
import scala.xml.{Elem,Node,MetaData,Null,TopScope}
import net.harnly.aaron.utilities.XMLUtilities
import net.harnly.aaron.extensions.Function1RightAssociativeExtensions._

abstract class EdgeFormatterXML[V, E <: Edge[V]](
	firstVertexLabel: String,
	secondVertexLabel: String
)
extends EdgeFormatterWithMetadata[V,E,Map[String,String],MetaData,Node,Node]
with EdgeFormatterTokenOnly[V,E,String,MetaData,Node,Node] 
{
	import XMLUtilities._
	// -- supplied --
	def extractMetadata(input: E): Map[String,String] = Map(
		"kind" -> input.getClass.getSimpleName
	)

	def formatMetadata(metadata: Map[String,String]): MetaData = mapToMetadata(metadata)

	def formatWithFormattedVerticesAndMetadata(v1: Node, v2: Node, formattedMetadata: MetaData): Node = new Elem(
		null,
		"edge",
		formattedMetadata,
		TopScope,
		simpleNode(firstVertexLabel, v1),
		simpleNode(secondVertexLabel, v2)
	)
}

abstract class EdgeFormatterXMLUndirected[V, E <: Edge[V]]
extends EdgeFormatterXML[V,E]("oneVertex","otherVertex")

