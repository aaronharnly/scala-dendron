package net.harnly.dendron.io.xml
import scala.xml.{Elem,Node,MetaData,Null,TopScope}
import net.harnly.aaron.utilities.XMLUtilities
import net.harnly.aaron.extensions.Function1RightAssociativeExtensions._

abstract class IDOnlyXMLEdgeParser[V, E <: Edge[V]](
	richEdgeFactory: (V,V,Map[String,String]) => E,
	override val defaultVertexParser: IDOnlyVertexParser[V,MetaData,Node],
	override val vertexParsers: IDOnlyVertexParser[V,MetaData,Node]*
)
extends XMLEdgeParser[V,E](
	metadataFactory,
	defaultVertexParser,
	vertexParsers: _*
)
with IDOnlyEdgeParser[V,E,MetaData,Node,Node]
