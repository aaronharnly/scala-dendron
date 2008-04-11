package net.harnly.dendron
import net.harnly.dendron.datatypes.EdgeMap._

case class SimpleGraph[V, E <: Edge[V]](
	vertices: Set[V],
	edges: Set[E],
	private val undirectedEdgeMap: EdgeMap[V,E]
)
extends Graph[V,E]
{
	override def self: SimpleGraph[V,E] = this
	
	def this(vertices: Seq[V], edges: Seq[E]) = this(
		Set( vertices : _* ),
		Set( edges : _* ),
		createUndirectedWithEdges(edges)
	)
	
	// information overrides
	override def edgesOf(vertex: V): Set[E] = getSubvaluesAsSet(undirectedEdgeMap, vertex)
	override def edgesTouching(vertex1: V, vertex2: V): Set[E] = getSubvalueAsSet(undirectedEdgeMap, vertex1, vertex2)
	override def getEdgeTouching(vertex1: V, vertex2: V): Option[E] = getSubvalue(undirectedEdgeMap, vertex1, vertex2)
	override def verticesInEdges: Set[V] = getKeysAsSet(undirectedEdgeMap)
	override def neighborsOf(vertex: V): Set[V] = getSubkeysAsSet(undirectedEdgeMap, vertex)
	
	// mutators
	def addVertex(vertex: V): SimpleGraph[V,E] = new SimpleGraph(
		vertices + vertex,
		edges,
		undirectedEdgeMap
	)

	def addEdge(edge: E): SimpleGraph[V,E] = 
		new SimpleGraph(
			vertices ++ edge.vertices,
			edges + edge,
			addUndirectedEdge(undirectedEdgeMap, edge)
		)

	def removeVertex(vertex: V): SimpleGraph[V,E] = {
		val touchingEdges = edgesOf(vertex)
		
		new SimpleGraph(
			vertices - vertex,
			edges -- touchingEdges,
			removeUndirectedEdges(undirectedEdgeMap, touchingEdges.toList : _*)
		)
	}

	def removeEdge(edge: E): SimpleGraph[V,E] = 
		new SimpleGraph(
			vertices,
			edges - edge,
			removeUndirectedEdge(undirectedEdgeMap, edge)
		)

}

object SimpleGraph
{
	def empty[V,E <: Edge[V]] = new SimpleGraph(
		Set.empty[V],
		Set.empty[E],
		Map.empty[V,Map[V,E]]
	)
	
	def create[V, E <: Edge[V]](vertices: Seq[V], edges: Seq[E]): SimpleGraph[V,E] = new SimpleGraph(
		Set( vertices : _* ),
		Set( edges : _* ),
		createUndirectedWithEdges(edges)
	)
}	

