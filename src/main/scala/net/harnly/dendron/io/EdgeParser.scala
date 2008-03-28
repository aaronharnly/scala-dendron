package net.harnly.dendron.io
import net.harnly.aaron.io.{Parser}

trait EdgeParser[Tedge,Tvertex,Ttoken,Mtoken,V,E <: Edge[V]] 
extends Parser[(Tedge,VertexCache[Mtoken,V]),Option[(E,VertexCache[Mtoken,V])]]
{
	def vertexParser: CachedVertexParser[Tvertex,Ttoken,Mtoken,V]
}

trait EdgeParserNoMetadata[Tedge,Tvertex,Ttoken,Mtoken,V,E <: Edge[V]]
extends EdgeParser[Tedge,Tvertex,Ttoken,Mtoken,V,E]
{
	def createEdge(v1: V, v2: V): Option[E]
	def splitEdge(edgeInput: Tedge): Option[(Tvertex,Tvertex)]
	
	def apply(edgeInput: (Tedge, VertexCache[Mtoken,V])): Option[(E,VertexCache[Mtoken,V])] = 
		for (
			split <- splitEdge(edgeInput._1);
			v1Tuple <- vertexParser.parse( split._1 -> edgeInput._2 );
			v2Tuple <- vertexParser.parse( split._2 -> v1Tuple._2 );
			edge <- createEdge(v1Tuple._1, v2Tuple._1)
		) yield (edge -> v2Tuple._2)
}

trait EdgeParserWithMetadata[
	Tedge,Tvertex,
	Ttoken,Tmetadata,
	Medge, Mtoken, 
	V, E <: Edge[V]
]
extends EdgeParser[Tedge,Tvertex,Ttoken,Mtoken,V,E]
{
	def createEdge(v1: V, v2: V, metadata: Medge): Option[E]
	def splitEdge(edgeInput: Tedge): Option[(Tvertex,Tvertex,Tmetadata)]
	def parseMetadata(input: Tmetadata): Option[Medge]
	
	def apply(edgeInput: (Tedge, VertexCache[Mtoken,V])): Option[(E,VertexCache[Mtoken,V])] = 
		for (
			split <- splitEdge(edgeInput._1);
			v1Tuple <- vertexParser.parse( split._1 -> edgeInput._2 );
			v2Tuple <- vertexParser.parse( split._2 -> v1Tuple._2 );
			metadata <- parseMetadata(split._3);
			edge <- createEdge(v1Tuple._1, v2Tuple._1, metadata)
		) yield (edge -> v2Tuple._2)
}
