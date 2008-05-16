package net.harnly.dendron

object GraphTypes
{
	type UndirectedGraphT[V] = Graph[V,Edge[V]]
	type DirectedGraphT[V] = DirectedGraph[V,DirectedEdge[V]]
	
	type IntUnGraph = Graph[Int,Edge[Int]]
	type IntGraph = DirectedGraph[Int,DirectedEdge[Int]]
	type SimpleIntGraph = SimpleDirectedGraph[Int,SimpleDirectedEdge[Int]]	
}

