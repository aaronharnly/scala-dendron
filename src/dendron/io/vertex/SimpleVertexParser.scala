package net.harnly.dendron.io

case class SimpleVertexParser[Tvertex,V](
	vertexFactory: Tvertex => V
)
extends VertexParser[Tvertex,V]
{
	def apply(input: Tvertex): Option[V] = Some(
		vertexFactory(input)
	)
}

