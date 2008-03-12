package net.harnly.dendron.io.xml
import scala.xml.{Node,Text}

class XMLFromStringVertexFormatter[V](
	val stringFormatter: VertexFormatter[V,String]
)
extends VertexFormatter[V,Node]
{
	def apply(input: V) = Text(stringFormatter(input))
}
