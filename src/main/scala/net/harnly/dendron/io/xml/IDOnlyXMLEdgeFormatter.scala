package net.harnly.dendron.io.xml
import scala.xml.{Elem,Node,MetaData,Null,TopScope}
import net.harnly.aaron.utilities.XMLUtilities
import net.harnly.aaron.extensions.Function1RightAssociativeExtensions._

abstract class IDOnlyXMLEdgeFormatter[V, E <: Edge[V]](
	metadataFactory: (E) => Map[String,String],
	override val defaultVertexFormatter: IDOnlyVertexFormatter[V,MetaData,Node],
	override val vertexFormatters: IDOnlyVertexFormatter[V,MetaData,Node]*
)
extends XMLEdgeFormatter[V,E](
	metadataFactory,
	defaultVertexFormatter,
	vertexFormatters: _*
)
with IDOnlyEdgeFormatter[V,E,MetaData,Node,Node]

class IDOnlyXMLDirectedEdgeFormatter[V, E <: DirectedEdge[V]](
	metadataFactory: (E) => Map[String,String],
	defaultVertexFormatter: IDOnlyVertexFormatter[V,MetaData,Node],
	vertexFormatters: IDOnlyVertexFormatter[V,MetaData,Node]*
)
extends IDOnlyXMLEdgeFormatter[V,E](
	metadataFactory,
	defaultVertexFormatter,
	vertexFormatters : _*
) with DirectedEdgeFormatter[V,E]

class IDOnlyXMLUndirectedEdgeFormatter[V, E <: Edge[V]](
	metadataFactory: (E) => Map[String,String],
	defaultVertexFormatter: IDOnlyVertexFormatter[V,MetaData,Node],
	vertexFormatters: IDOnlyVertexFormatter[V,MetaData,Node]*
)
extends IDOnlyXMLEdgeFormatter[V,E](
	metadataFactory,
	defaultVertexFormatter,
	vertexFormatters : _*
) with UndirectedEdgeFormatter[V,E]
