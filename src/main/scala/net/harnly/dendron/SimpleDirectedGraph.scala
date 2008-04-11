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

	def this(vertices: Seq[V], edges: Seq[E]) = this(
		Set( vertices : _*),
		Set( edges : _*),
		createUndirectedWithEdges(edges),
		createDirectedOutgoingWithEdges(edges),
		createDirectedIncomingWithEdges(edges)		
	)

	def this(vertices: Set[V], edges: Set[E]) = this(
		vertices,
		edges,
		createUndirectedWithEdges(edges.toList),
		createDirectedOutgoingWithEdges(edges.toList),
		createDirectedIncomingWithEdges(edges.toList)
	)	
	
	// information overrides
	override def incomingEdgesOf(vertex: V): Set[E] = getSubvaluesAsSet(incomingEdgeMap, vertex)
	override def outgoingEdgesOf(vertex: V): Set[E] = getSubvaluesAsSet(outgoingEdgeMap, vertex)
	override def getEdge(vertex1: V, vertex2: V): Option[E] = getSubvalue(outgoingEdgeMap, vertex1, vertex2)

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
			removeDirectedIncomingEdges(incomingEdgeMap, incomingEdgesOf(vertex).toList : _*)
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
	override def toString = "SimpleDirectedGraph.fromPairs(" + vertices + "," + edges + ")"
	
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
	
	def fromPairs[V](
		vertices: Set[V],
		edges: Set[(V,V)]
	) = {
		val edgeSet = edges.map(pair => new SimpleDirectedEdge(pair._1, pair._2))
		create(vertices, edgeSet)
	}
	
	def create[V, E <: DirectedEdge[V]](vertices: Set[V], edges: Set[E]) = {
		val edgeList = edges.toList

		new SimpleDirectedGraph(
			vertices,
			edges,
			createUndirectedWithEdges[V,E](edgeList),
			createDirectedOutgoingWithEdges[V, E](edgeList),
			createDirectedIncomingWithEdges[V, E](edgeList)
		)	
	}

	def create[V, E <: DirectedEdge[V]](vertices: Seq[V], edges: Seq[E]) = new SimpleDirectedGraph(
		Set( vertices : _*),
		Set( edges : _*),
		createUndirectedWithEdges[V,E](edges),
		createDirectedOutgoingWithEdges[V,E](edges),
		createDirectedIncomingWithEdges[V,E](edges)
	)
}	
