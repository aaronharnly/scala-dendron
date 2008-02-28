package net.harnly.dendron

case class SimpleVertex[A](label: A)
extends Vertex
{
	type VertexType = SimpleVertex[A]
}

