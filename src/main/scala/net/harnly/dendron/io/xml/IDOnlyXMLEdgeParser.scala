package net.harnly.dendron.io.xml
import scala.collection.{Map}
import scala.xml.{Elem,Node,MetaData,Null,TopScope}
import net.harnly.aaron.utilities.XMLUtilities
import net.harnly.aaron.extensions.Function1RightAssociativeExtensions._

class IDOnlyXMLEdgeParser[V, E <: Edge[V]](
	firstVertexLabel: String,
	secondVertexLabel: String,
	override val edgeFactory: ((V,V,Map[String,String])) => Option[E],
	override val defaultVertexParser: IDOnlyVertexParser[Node,String,V],
	override val vertexParsers: IDOnlyVertexParser[Node,String,V]*
)
extends IDOnlyVertexPairAndContextSplitterEdgeParser[Node,Node,String,Map[String,String],V,E](
	edgeFactory,
	EdgeNodeSplitter(firstVertexLabel, secondVertexLabel),
	defaultVertexParser,
	vertexParsers: _*
)

class IDOnlyXMLDirectedEdgeParser[V, E <: Edge[V]](
	override val edgeFactory: ((V,V,Map[String,String])) => Option[E],
	override val defaultVertexParser: IDOnlyVertexParser[Node,String,V],
	override val vertexParsers: IDOnlyVertexParser[Node,String,V]*
)
extends IDOnlyXMLEdgeParser[V,E](
	"tail","head",
	edgeFactory,
	defaultVertexParser,
	vertexParsers: _*
)

class IDOnlyXMLUndirectedEdgeParser[V, E <: Edge[V]](
	override val edgeFactory: ((V,V,Map[String,String])) => Option[E],
	override val defaultVertexParser: IDOnlyVertexParser[Node,String,V],
	override val vertexParsers: IDOnlyVertexParser[Node,String,V]*
)
extends IDOnlyXMLEdgeParser[V,E](
	"oneVertex","otherVertex",
	edgeFactory,
	defaultVertexParser,
	vertexParsers: _*
)

