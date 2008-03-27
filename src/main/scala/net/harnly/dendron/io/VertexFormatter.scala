package net.harnly.dendron.io
import net.harnly.aaron.io.{Formatter}

trait VertexFormatter[V,Tvertex] 
extends Formatter[V,Tvertex]

trait VertexFormatterTokenOnly[V,Ttoken,Tvertex]
extends VertexFormatter[V,Tvertex]
{
	def extractToken(vertex: V): Ttoken
}
