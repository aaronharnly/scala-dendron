package net.harnly.dendron.io

case class SimpleVertexParser[Tvertex,V](
	vertexFactory: Tvertex => V
)
extends VertexParser[Tvertex,V]
{
	def canParse(input: Tvertex) = true
	def parse(input: A2 forSome {type A2 <: Tvertex}): Option[V] = Some(
		vertexFactory(input)
	)
}

