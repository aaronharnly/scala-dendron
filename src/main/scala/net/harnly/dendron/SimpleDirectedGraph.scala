package net.harnly.dendron

import net.harnly.dendron.datatypes.EdgeMap._

case class SimpleDirectedGraph[V, E <: DirectedEdge[V]](
	override val vertices: Set[V],
	override val edges: Set[E],
	private val undirectedEdgeMap: EdgeMap[V,E],
	private val outgoingEdgeMap: DirectedEdgeMap[V,E],
	private val incomingEdgeMap: DirectedEdgeMap[V,E]
)
extends DirectedGraph[V,E]
{
	override def self: SimpleDirectedGraph[V,E] = this

	// information overrides
	override def incomingEdgesOf(vertex: V): Set[E] = getSubvaluesAsSet(incomingEdgeMap, vertex)
	override def outgoingEdgesOf(vertex: V): Set[E] = getSubvaluesAsSet(outgoingEdgeMap, vertex)

	// mutators
	def addVertex(vertex: V): SimpleDirectedGraph[V,E] = new SimpleDirectedGraph(
		vertices + vertex,
		edges,
		undirectedEdgeMap,
		outgoingEdgeMap,
		incomingEdgeMap
	)

	def addEdge(edge: E): SimpleDirectedGraph[V,E] = 
		new SimpleDirectedGraph(
			vertices ++ edge.vertices,
			edges + edge,
			addUndirectedEdge(undirectedEdgeMap, edge),
			addDirectedOutgoingEdge(outgoingEdgeMap, edge),
			addDirectedIncomingEdge(incomingEdgeMap, edge)
		)

	def removeVertex(vertex: V): SimpleDirectedGraph[V,E] = {
		val touchingEdges = edgesOf(vertex)
	
		new SimpleDirectedGraph(
			vertices - vertex,
			edges -- edgesOf(vertex),
			removeUndirectedEdges(undirectedEdgeMap, touchingEdges.toList : _*),
			removeDirectedOutgoingEdges(outgoingEdgeMap, outgoingEdgesOf(vertex).toList : _*),
			removeDirectedIncomingEdges(incomingEdgeMap, incomingEdgesOf(vertex).toList : _*),
		)
	}

	def removeEdge(edge: E): SimpleDirectedGraph[V,E] = 
		new SimpleDirectedGraph(
			vertices,
			edges - edge,
			removeUndirectedEdge(undirectedEdgeMap, edge),
			removeDirectedOutgoingEdge(outgoingEdgeMap, edge),
			removeDirectedIncomingEdge(incomingEdgeMap, edge)
		)
	

	override def addEdges(edges: E*): SimpleDirectedGraph[V,E] = edges.foldLeft(self)( _.addEdge(_))
	
}

object SimpleDirectedGraph
{
	def empty[V,E <: DirectedEdge[V]] = new SimpleDirectedGraph(
		Set.empty[V],
		Set.empty[E],
		Map.empty[V, Map[V,E]],
		Map.empty[V, Map[V,E]],
		Map.empty[V, Map[V,E]]
	)	
}	
