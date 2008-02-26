package net.harnly.dendron

case class SimpleVertex[A](label: A)
extends Vertex
{
	type VertexType = SimpleVertex[A]
}

case class SimpleEdge[V <: Vertex](
	tail: V,
	head: V
)
extends Edge[V]
{
	type EdgeType = SimpleEdge[V]
	def invert = new SimpleEdge(head, tail)
}

case class SimpleWeightedEdge[V <: Vertex](
	override val tail: V,
	override val head: V,
	weight: Double
)
extends SimpleEdge(tail, head)
with WeightedEdge[V]

case class SimpleGraph[V <: Vertex,E <: Edge[V]](
	vertices: Set[V],
	edges: Set[E]
)
extends Graph[V,E]
{
	def this(vertexList: Seq[V], edgeList: Seq[E]) = this(
		collection.immutable.Set(vertexList : _*) ,
		collection.immutable.Set(edgeList : _*) 
	)
	
	type GraphType = SimpleGraph[V,E]

	def addVertex(vertex: V2 forSome {type V2 <: V}): GraphType = new SimpleGraph(
		vertices ++ List(vertex),
		edges
	)
	
	def addEdge(edge: E2 forSome {type E2 <: E}): GraphType = new SimpleGraph(
		vertices ++ List(edge.head, edge.tail),
		edges ++ List(edge)
	)
}

object SimpleGraph
{
	def empty[V <: Vertex,E <: Edge[V]] = new SimpleGraph[V,E](
		Set.empty[V],
		Set.empty[E]
	)
}