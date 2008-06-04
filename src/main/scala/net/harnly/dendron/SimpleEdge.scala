package net.harnly.dendron
import scala.collection.immutable.{Set2}

case class SimpleEdge[V](
	unorderedVertices: Set2[V]
)
extends Edge[V]
{
	def this(a: V, b: V) = this(
		new Set2(a,b)
	)
}

case class SimpleDirectedEdge[V](
	tail: V,
	head: V
)
extends DirectedEdge[V]
{
	override val unorderedVertices = new Set2(tail, head)
	def invert = new SimpleDirectedEdge(head, tail)
}

case class SimpleWeightedEdge[V](
	override val unorderedVertices: Set2[V],
	weight: Double
)
extends SimpleEdge(unorderedVertices)
with WeightedEdge[V]
{
	def this(a: V, b: V, weight: Double) = this(
		new Set2(a,b),
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
