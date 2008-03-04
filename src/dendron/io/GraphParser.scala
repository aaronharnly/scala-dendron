package net.harnly.dendron.io
import net.harnly.aaron.io.{Parser}

trait GraphParser[A,V,E <: Edge[V],G <: Graph[V,E]]
extends Parser[A,G]

