package net.harnly.dendron.io.dot
/*
object DotTabulae
{
	import net.harnly.tabula.Tabulae._
	import net.harnly.tabula.StringPrimitiveTabulae._
	import GraphTypes._

	class DotTabula[V](
		implicit vertexTabula: Tabula[V,String]
	)
	extends Tabula[Graph[V,Edge[V]],String]
	{
		def encode(graph: Graph[V,Edge[V]]): String = 
		<_>{graphLabel(graph)} G {{
{ graph.vertices.toList.map( encodeVertex(_) + "\n" ) }
{ graph.edges.toList.map encodeVertex }
}}</_>.text
		
		def graphLabel(directed: Boolean) = 
		def graphLabel(graph: Graph[V,Edge[V]]) = graph match {
			case DirectedGraph[_,_] => "digraph"
			case _ => "graph"
		}
		
		def encodeVertex(v: V): String = 
			"\"" + vertexTabula.encode(v) + "\""

		def encodeEdge(e: Edge[V]): String = {
			val ordered = e.orderedVertices
<_>{encodeVertex(ordered._1)} {edgeArrow(e)} {encodeVertex(ordered._2)}
</_>.text
		}
		
		def edgeArrow(e: Edge[V]): String = edgeArrow( e.isInstanceOf[DirectedEdge[V]] )
		def edgeArrow(directed: Boolean) = if (directed) "->" else "--"

		def decode(formatted: String): Result[Graph[V,Edge[V]]] = 
		{
			val lines = LinesFromStringReader(formatted).map(_.trim).filter(_.length != 0).toList
			val firstLine = lines.head
			val isDirected = firstLines.contains("digraph")
			val empty = createSimple(isDirected)
			
			
		}
		
		def createSimple(directed: Boolean): Graph[V,Edge[V]] = if (directed)
			SimpleDirectedGraph.empty[V,SimpleDirectedEdge[V]]
		else
			SimpleGraph.empty[V,SimpleEdge[V]]
	}
}
*/