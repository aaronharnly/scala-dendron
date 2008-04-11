package net.harnly.dendron.datatypes
import net.harnly.dendron.{Edge,DirectedEdge}

object EdgeMap
{
	type DoubleMap[A,B] = Map[A,Map[A,B]]
	type EdgeMap[V,E <: Edge[V]] = DoubleMap[V,E]
	type DirectedEdgeMap[V,E <: DirectedEdge[V]] = EdgeMap[V,E]
	
	// --- Undirected ---
	def createUndirectedWithEdges[V, E <: Edge[V]](edges: Seq[E]) = 
		addUndirectedEdges(Map.empty[V,Map[V,E]], edges : _*)
	
	def addUndirectedEdge[V, E <: Edge[V]](edgeMap: EdgeMap[V,E], edge: E): EdgeMap[V,E] = {
		val (v1, v2) = edge.orderedVertices
		addToDoubleMap(
			addToDoubleMap(
				edgeMap,
				v1,
				v2 -> edge
			),
			v2,
			v1 -> edge
		)
	}
	
	def removeUndirectedEdge[V, E <: Edge[V]](edgeMap: EdgeMap[V,E], edge: E): EdgeMap[V,E] = {
		val (v1, v2) = edge.orderedVertices
		removeFromDoubleMap(
			removeFromDoubleMap(
				edgeMap,
				v1,
				v2
			),
			v2,
			v1
		)
	}
	
	def addUndirectedEdges[V, E <: Edge[V]](edgeMap: EdgeMap[V,E], edges: E*): EdgeMap[V,E] =
		applyRepeatedly(edgeMap, addUndirectedEdge[V,E], edges : _*)

	def removeUndirectedEdges[V, E <: Edge[V]](edgeMap: EdgeMap[V,E], edges: E*): EdgeMap[V,E] =
		applyRepeatedly(edgeMap, removeUndirectedEdge[V,E], edges: _*)

	// --- Directed ---
	// -- out
	def createDirectedOutgoingWithEdges[V, E <: DirectedEdge[V]](edges: Seq[E]) = 
		addDirectedOutgoingEdges(Map.empty[V,Map[V,E]], edges : _*)

	def addDirectedOutgoingEdge[V, E <: DirectedEdge[V]](edgeMap: DirectedEdgeMap[V,E], edge: E): DirectedEdgeMap[V,E] = 
		addToDoubleMap(edgeMap, edge.tail, edge.head -> edge)
	
	def removeDirectedOutgoingEdge[V, E <: DirectedEdge[V]](edgeMap: DirectedEdgeMap[V,E], edge: E): DirectedEdgeMap[V,E] =
		removeFromDoubleMap(edgeMap, edge.tail, edge.head)

	def addDirectedOutgoingEdges[V, E <: DirectedEdge[V]](edgeMap: DirectedEdgeMap[V,E], edges: E*): DirectedEdgeMap[V,E] =
		applyRepeatedly(edgeMap, addDirectedOutgoingEdge[V,E], edges : _*)

	def removeDirectedOutgoingEdges[V, E <: DirectedEdge[V]](edgeMap: DirectedEdgeMap[V,E], edges: E*): DirectedEdgeMap[V,E] =
		applyRepeatedly(edgeMap, removeDirectedOutgoingEdge[V,E], edges : _*)

	// -- in
	def createDirectedIncomingWithEdges[V, E <: DirectedEdge[V]](edges: Seq[E]) = 
		addDirectedIncomingEdges(Map.empty[V,Map[V,E]], edges : _*)

	def addDirectedIncomingEdge[V, E <: DirectedEdge[V]](edgeMap: DirectedEdgeMap[V,E], edge: E): DirectedEdgeMap[V,E] = 
		addToDoubleMap(edgeMap, edge.head, edge.tail -> edge)

	def addDirectedIncomingEdges[V, E <: DirectedEdge[V]](edgeMap: DirectedEdgeMap[V,E], edges: E*): DirectedEdgeMap[V,E] =
		applyRepeatedly(edgeMap, addDirectedIncomingEdge[V,E], edges : _*)

	def removeDirectedIncomingEdge[V, E <: DirectedEdge[V]](edgeMap: DirectedEdgeMap[V,E], edge: E): DirectedEdgeMap[V,E] =
		removeFromDoubleMap(edgeMap, edge.head, edge.tail)

	def removeDirectedIncomingEdges[V, E <: DirectedEdge[V]](edgeMap: DirectedEdgeMap[V,E], edges: E*): DirectedEdgeMap[V,E] =
		applyRepeatedly(edgeMap, removeDirectedIncomingEdge[V,E], edges : _*)


	// -- general --
	def emptyDoubleMap[A,B] = Map.empty[A, Map[A, B]]
	
	def applyRepeatedly[A, B, X](
		input: DoubleMap[A,B], 
		method: (DoubleMap[A,B], X) => DoubleMap[A,B],
		args: X*
	) = args.foldLeft(input) { (m, x) =>
		method(m, x)
	}
	
	
	def addToDoubleMap[A,B](input: DoubleMap[A,B], key: A, value: (A,B)): DoubleMap[A,B] = {
		val existing = input.get(key).getOrElse(Map.empty[A,B])
		input + (key -> (existing + value))
	}

	def removeFromDoubleMap[A,B](input: DoubleMap[A,B], key: A, subkey: A): DoubleMap[A,B] = {
		val existing = input.get(key).getOrElse(Map.empty[A,B])
		input + (key -> (existing - subkey))
	}
	
	def getKeysAsSet[A,B](input: DoubleMap[A,B]): Set[A] =
		Set( input.keys.toList : _* )
	
	def getSubkeysAsSet[A,B](input: DoubleMap[A,B], key: A): Set[A] = 
		input.get(key).map( m => Set(m.keys.toList : _*) ).getOrElse(Set.empty)
	
	def getSubvaluesAsSet[A,B](input: DoubleMap[A,B], key: A): Set[B] =
		input.get(key).map(m => Set(m.values.toList : _*)).getOrElse(Set.empty)
	
	def getSubvalue[A,B](input: DoubleMap[A,B], key: A, subkey: A): Option[B] = 
		input.get(key).flatMap(_.get(subkey))
	
	def getSubvalueAsSet[A,B](input: DoubleMap[A,B], key: A, subkey: A): Set[B] =
		getSubvalue(input, key, subkey).map(value => Set(value)).getOrElse(Set.empty)
}
