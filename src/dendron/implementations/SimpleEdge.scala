package net.harnly.dendron
import net.harnly.dendron.datatypes.{UnorderedPair,PairSet}

case class SimpleEdge[V](
	vertices: UnorderedPair[V]
)
extends Edge[V]
{
	def this(a: V, b: V) = this(
		PairSet(a,b)
	)
	type EdgeType = SimpleEdge[V]
}

case class SimpleDirectedEdge[V](
	tail: V,
	head: V
)
extends SimpleEdge(tail, head) 
with DirectedEdge[V]
{
	def invert = new SimpleDirectedEdge(head, tail)
}

case class SimpleWeightedEdge[V](
	override val vertices: UnorderedPair[V],
	weight: Double
)
extends SimpleEdge(vertices)
with WeightedEdge[V]
{
	def this(a: V, b: V, weight: Double) = this(
		PairSet(a,b),
		weight
	)	
}

case class SimpleWeightedDirectedEdge[V](
	override val tail: V,
	override val head: V,
	weight: Double
)
extends SimpleDirectedEdge(tail, head)
with WeightedEdge[V]
{
	override def invert = new SimpleWeightedDirectedEdge(
		head, tail, weight
	)
}
