package net.harnly.dendron

trait Graph[V <: Vertex, E <: Edge[V]]
{
	// -- abstract --
	type GraphType <: Graph[V,E]

	def vertices: Set[V]
	def edges: Set[E]
	
	// mutators	
	def addVertex(vertex: V2 forSome {type V2 <: V}): GraphType
	def addEdge(edge: E2 forSome {type E2 <: E} ): GraphType
	
	// -- supplied --
	def +(vertex: V2 forSome {type V2 <: V}) = addVertex(vertex)
	def +(edge: E2 forSome {type E2 <: E}) = addEdge(edge)
}

