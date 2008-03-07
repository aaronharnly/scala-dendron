package net.harnly.dendron.io
import net.harnly.aaron.io.{Formatter}

trait GraphFormatter[V,E <: Edge[V],G <: Graph[V,E],A] 
extends Formatter[G,A]

