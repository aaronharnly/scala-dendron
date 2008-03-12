package net.harnly.dendron.io.xml
import scala.xml.{Node,Text}

class XMLFromStringVertexParser[V](
	stringParser: VertexParser[String,V]
)
extends VertexParser[Node,V]
{
	def apply(input: Node) = stringParser(input.text)
}
