package net.harnly.dendron.io.xml

abstract class EdgeFormatterXMLDirected[V, E <: DirectedEdge[V]]
extends EdgeFormatterXML[V,E]("tail","head")

