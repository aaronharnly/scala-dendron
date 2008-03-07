package net.harnly.dendron.io
import net.harnly.aaron.io.{Parser}

trait VertexParser[Tvertex,V] 
extends Parser[Tvertex,Option[V]]
{
	def canParse(input: Tvertex): Boolean
}
