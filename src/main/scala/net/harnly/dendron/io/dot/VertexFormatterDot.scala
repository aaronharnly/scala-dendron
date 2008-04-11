package net.harnly.dendron.io.dot

abstract class VertexFormatterDotTokenOnly[V]
extends VertexFormatterTokenOnly[V,String,String,String]
{
//	def extractToken(input: V): String
	def formatToken(tokenInput: String) = DotVertexRenderer.formatToken(tokenInput)
	def apply(input: V): String = formatToken(extractToken(input))
}

abstract class VertexFormatterDot[V]
extends VertexFormatter[V,String]
{
	// -- abstract --
	def extractToken(input: V): String
	def extractMetadata(input: V): Map[String,String]

	// -- supplied --
	def formatToken(tokenInput: String) = DotVertexRenderer.formatToken(tokenInput)
	def formatMetadata(data: Map[String,String]): String = DotMetadataRenderer(data)

	def apply(input: V): String = 
	formatToken(extractToken(input)) + " " + formatMetadata(extractMetadata(input))
}

class VertexFormatterDotTrivial[V]
extends VertexFormatterDotTokenOnly[V]
{
	def extractToken(input: V) = input.toString
}

class VertexFormatterDotTrivialWithMetadata[V](
	metadataMap: Map[V, Map[String,String]]
)
extends VertexFormatterDot[V]
{
	def extractToken(input: V) = input.toString
	def extractMetadata(input: V): Map[String,String] = metadataMap.get(input).getOrElse(Map.empty[String,String])
}

object DotVertexRenderer
{
	def formatToken(tokenInput: String) = "\"" + tokenInput + "\""
}
