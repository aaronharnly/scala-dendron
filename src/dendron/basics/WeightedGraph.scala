package net.harnly.dendron

trait WeightedEdge[+V]
extends Edge[V]
{
	def weight: Double
}

trait WeightedGraph[V, E <: WeightedEdge[V]]
extends Graph[V,E]
{
	// -- abstract --
	
	// -- supplied --
}
