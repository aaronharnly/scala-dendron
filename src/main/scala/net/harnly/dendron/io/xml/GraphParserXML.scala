package net.harnly.dendron.io.xml
import scala.collection.{Map}
import scala.xml.{Node,MetaData}
import net.harnly.dendron._
import net.harnly.dendron.io._
import net.harnly.aaron.extensions.SequenceOptionExtensions._
import net.harnly.aaron.utilities.XMLUtilities._

abstract class GraphParserXML[V, E <: net.harnly.dendron.Edge[V], G <: net.harnly.dendron.Graph[V,E]]
extends net.harnly.dendron.io.GraphParserWithMetadata[
	Node, Node, Node,
	String, MetaData,
	Map[String,String], V, E, G
]
{
	// -- supplied --
	def freshCache: VertexCache[String,V] = VertexCache.empty[String,V]
	def parseMetadata(input: MetaData) = Some(metadataToMap(input))

	def splitGraph(input: Node): Option[(Seq[Node], Seq[Node], MetaData)] = for(
		vertexContainer <- seq2Option( input \ "vertices" );
		edgeContainer <- seq2Option( input \ "edges" )
	) yield
		(( vertexContainer.child, edgeContainer.child, input.attributes ))
}

