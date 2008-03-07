package net.harnly.dendron.io
import net.harnly.aaron.io.{Formatter}

trait EdgeFormatter[V,E <: Edge[V],A] 
extends Formatter[E,A]
{
	def canFormat(input: E): Boolean
}
