package net.harnly.dendron.io
import net.harnly.aaron.io.{Parser}

trait GraphParser[Tgraph,Tedge,Tvertex,Ttoken,Mtoken,V,E <: Edge[V],G <: Graph[V,E]]
extends Parser[Tgraph,Option[G]]
{
	// -- abstract --
	def edgeParser: EdgeParser[Tedge,Tvertex,Ttoken,Mtoken,V,E]
	def vertexParser: CachedVertexParser[Tvertex,Ttoken,Mtoken,V]
	
	// -- supplied --
	def freshCache: VertexCache[Mtoken,V] = VertexCache.empty[Mtoken,V]
}

trait GraphParserNoMetadata[
	Tgraph, Tedge, Tvertex,
	Ttoken, Mtoken,
	V, E <: Edge[V], G <: Graph[V,E]
]
extends GraphParser[Tgraph,Tedge,Tvertex,Ttoken,Mtoken,V,E,G]
{
	def createGraph(vertices: Set[V], edges: Set[E]): Option[G]
	def splitGraph(input: Tgraph): Option[(Seq[Tvertex], Seq[Tedge])]
	
	def apply(input: Tgraph): Option[G] = splitGraph(input).flatMap { splitTuple =>
		val (vertexInputs, edgeInputs) = splitTuple
		var cache = freshCache

		val verticesNotInEdges = vertexInputs.flatMap { vInput =>
			vertexParser.parse(vInput -> cache).map { result =>
				cache = result._2
				result._1
			}
		}

		val edges = edgeInputs.flatMap { eInput =>
			edgeParser.parse(eInput -> cache).map { result =>
				cache = result._2
				result._1
			}
		}
		val verticesInEdges = edges.foldLeft(Set.empty[V])( (set, edge) =>
			set ++ edge
		)
		val allVertices = verticesNotInEdges ++ verticesInEdges
		val vertexSet = Set(allVertices : _*)
		val edgeSet = Set(edges : _*)
		
		createGraph(vertexSet, edgeSet)
	}
}

trait GraphParserWithMetadata[
	Tgraph, Tedge, Tvertex,
	Ttoken, Tmetadata,
	Mgraph, Mtoken,
	V, E <: Edge[V], G <: Graph[V,E]
]
extends GraphParser[Tgraph,Tedge,Tvertex,Ttoken,Mtoken,V,E,G]
{
	def createGraph(vertices: Set[V], edges: Set[E], metadata: Mgraph): Option[G]
	def splitGraph(input: Tgraph): Option[(Seq[Tvertex], Seq[Tedge], Tmetadata)]
	def parseMetadata(input: Tmetadata): Option[Mgraph]
	
	def apply(input: Tgraph): Option[G] = splitGraph(input).flatMap { splitTuple =>
		val (vertexInputs, edgeInputs, metadataInput) = splitTuple
		parseMetadata(metadataInput).flatMap { metadata =>
			var cache = freshCache

			val verticesNotInEdges = vertexInputs.flatMap { vInput =>
				vertexParser.parse(vInput -> cache).map { result =>
					cache = result._2
					result._1
				}
			}

			val edges = edgeInputs.flatMap { eInput =>
				edgeParser.parse(eInput -> cache).map { result =>
					cache = result._2
					result._1
				}
			}
			val verticesInEdges = edges.foldLeft(Set.empty[V])( (set, edge) =>
				set ++ edge
			)
			val allVertices = verticesNotInEdges ++ verticesInEdges
			val vertexSet = Set(allVertices : _*)
			val edgeSet = Set(edges : _*)

			createGraph(vertexSet, edgeSet, metadata)
		}
	}
}
