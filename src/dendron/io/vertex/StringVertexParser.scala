package net.harnly.dendron.io.string

object SimpleStringVertexParser
extends VertexParser[String,String]
{
	def canParse(input: String) = true
	def parse(input: A2 forSome {type A2 <: String}): Option[String] = Some(input)
}

