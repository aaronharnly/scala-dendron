package net.harnly.dendron
import net.harnly.dendron.datatypes.{UnorderedPair,PairSet}

case class SimpleEdge[V](
	unorderedVertices: UnorderedPair[V]
)
extends Edge[V]
{
	def this(a: V, b: V) = this(
		PairSet(a,b)
	)
}

case class SimpleDirectedEdge[V](
	tail: V,
	head: V
)
extends DirectedEdge[V]
{
	val unorderedVertices = PairSet(tail, head)
	def invert = new SimpleDirectedEdge(head, tail)
}

case class SimpleWeightedEdge[V](
	override val unorderedVertices: UnorderedPair[V],
	weight: Double
)
extends SimpleEdge(unorderedVertices)
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
