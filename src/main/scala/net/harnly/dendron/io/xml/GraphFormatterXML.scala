package net.harnly.dendron.io.xml
import scala.collection.{Map}
import scala.xml.{Node,Elem,MetaData,TopScope}
import net.harnly.aaron.utilities.XMLUtilities

abstract class GraphFormatterXML[V, E <: Edge[V], G <: Graph[V,E]]
extends GraphFormatterWithMetadata[
	V,E,G, Map[String,String],
	MetaData,Node, Node, Node
]
{
	import XMLUtilities._
	
	def formatWithFormattedVerticesEdgesAndMetadata(
		formattedVertices: Seq[Node],
		formattedEdges: Seq[Node],
		formattedMetadata: MetaData
	): Node = new Elem(
		null,
		"graph",
		formattedMetadata,
		TopScope,
		<vertices>{
			formattedVertices
		}</vertices>,
		<edges>{
			formattedEdges
		}</edges>
	)
	
	def formatMetadata(map: Map[String,String]) = mapToMetadata(map)
}