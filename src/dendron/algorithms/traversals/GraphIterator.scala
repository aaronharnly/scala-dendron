package net.harnly.dendron.algorithms
import scala.collection.mutable.{Set => MSet, HashSet}
import net.harnly.dendron.datatypes._

trait GraphIterator[V,E <: DirectedEdge[V], G <: DirectedGraph[V,E]]
extends Iterator[V]
{
	def graph: G
	def start: V
}

trait GraphIteratorWithContainer[V,E <: DirectedEdge[V], G <: DirectedGraph[V,E]]
extends GraphIterator[V,E,G]
{
	// -- abstract --
	protected val container: MutableContainer[V]
	protected val visited: MSet[V]
	
	// -- provided --
	def init = container.insert(start)
	def hasNext = ! container.isEmpty
	def edgesFromVertex(vertex: V): Set[E] = graph.outgoingEdgesOf(vertex)
}

trait Preorder[V,E <: DirectedEdge[V], G <: DirectedGraph[V,E]]
extends GraphIteratorWithContainer[V,E,G]
{
	def next = {
		val v = container.remove
		visited += v
		edgesFromVertex(v).foreach{ e =>
			val otherVertex = e.otherVertex(v)
			if (! visited.contains(otherVertex))
				container.insert(otherVertex)
		}
		v
	}
}

case class PreorderDFSIterator[V,E <: DirectedEdge[V], G <: DirectedGraph[V,E]](
	start: V
)(
	val graph: G
)
extends Preorder[V,E,G]
{
	val container = new MutableStack[V]
	val visited = new HashSet[V]
	init
}

case class BFSIterator[V,E <: DirectedEdge[V], G <: DirectedGraph[V,E]](
	start: V
)(
	val graph: G
)
extends Preorder[V,E,G]
{
	val container = new MutableQueue[V]
	val visited = new HashSet[V]
	init
}
