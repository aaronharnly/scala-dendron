package net.harnly.dendron.io.xml
import scala.xml.{Elem,Node,MetaData,Null,TopScope}
import net.harnly.aaron.utilities.XMLUtilities
import net.harnly.aaron.extensions.Function1RightAssociativeExtensions._

abstract class XMLEdgeFormatter[V, E <: Edge[V]](
	val metadataFactory: (E) => Map[String,String],
	val defaultVertexFormatter: VertexFormatter[V,Node],
	val vertexFormatters: VertexFormatter[V,Node]*
)
extends EdgeFormatter[V,E,Node,Node]
{
	def firstVertexLabel: String
	def secondVertexLabel: String
	def firstVertexExtractor(edge: E): V
	def secondVertexExtractor(edge: E, first: V): V
	
	def metadataForEdge(
		metadataFactory: (E) => Map[String,String],
		edge: E
	): MetaData = edge ->: metadataFactory ->: 
		{ m: Map[String,String] => m + ("kind" -> edge.getClass.getSimpleName)} ->: 
		XMLUtilities.mapToMetadata _
	
	def vertex(v: V, label: String): Node = new Elem(
		null,
		label,
		Null,
		TopScope,
		formatterForVertex(v).format(v)
	)
	
	def apply(edge: E): Node = {
		val first = firstVertexExtractor(edge)
		val second = secondVertexExtractor(edge, first)

		new Elem(
			null,
			"edge",
			metadataForEdge(metadataFactory, edge),
			TopScope,
			vertex(first, firstVertexLabel), 
			vertex(second, secondVertexLabel)
		)
	}
}

trait DirectedEdgeFormatter[V, E <: DirectedEdge[V]]
{
	val firstVertexLabel = "tail"
	val secondVertexLabel = "head"
	
	def firstVertexExtractor(edge: E) = edge.tail
	def secondVertexExtractor(edge: E, first: V) = edge.head	
}

trait UndirectedEdgeFormatter[V, E <: Edge[V]]
{
	val firstVertexLabel = "oneVertex"
	val secondVertexLabel = "otherVertex"
	
	def firstVertexExtractor(edge: E) = edge.oneVertex
	def secondVertexExtractor(edge: E, first: V) = edge.otherVertex(first)
}

class XMLDirectedEdgeFormatter[V, E <: DirectedEdge[V]](
	metadataFactory: (E) => Map[String,String],
	defaultVertexFormatter: VertexFormatter[V,Node],
	vertexFormatters: VertexFormatter[V,Node]*
)
extends XMLEdgeFormatter[V,E](
	metadataFactory,
	defaultVertexFormatter,
	vertexFormatters : _*
) with DirectedEdgeFormatter[V,E]

class XMLUndirectedEdgeFormatter[V, E <: Edge[V]](
	metadataFactory: (E) => Map[String,String],
	defaultVertexFormatter: VertexFormatter[V,Node],
	vertexFormatters: VertexFormatter[V,Node]*
)
extends XMLEdgeFormatter[V,E](
	metadataFactory,
	defaultVertexFormatter,
	vertexFormatters : _*
) with UndirectedEdgeFormatter[V,E]
