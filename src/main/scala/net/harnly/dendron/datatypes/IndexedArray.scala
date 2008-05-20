package net.harnly.dendron.datatypes

import net.harnly.tabula.Tabulae._

case class IndexedArray2[K,V](array: Array[Array[V]], index: Map[K,Int])
{
	lazy val length1 = array.length
	lazy val length2 = array(0).length

	def apply(k1: K, k2: K) = array( index(k1) )( index(k2) )
	def apply(k1: K): Map[K,V] = Map( 
		index.keys.toList.flatMap { k => 
			val index2 = index(k)
			if (index2 < length2)
				Some( k -> apply(k1, k) )
			else
				None
		} : _* 
	) 
}

object IndexedArray2
{
	def createTabula[K,V](
		labeler: K => String,
		finder: String => Option[K],
		maker: Int => Array[Array[V]]
	)(
		implicit valueTabula: Tabula[V,String]
	): Tabula[IndexedArray2[K,V],Iterable[String]] = new IndexedArrayTabula[K,V](
		labeler,
		finder,
		maker
	)
}

// this thing kind of assumes square arrays
class IndexedArrayTabula[K,V](
	labeler: K => String,
	finder: String => Option[K],
	maker: Int => Array[Array[V]]
)(
	implicit valueTabula: Tabula[V,String]
)
extends Tabula[IndexedArray2[K,V],Iterable[String]]
{
	def encode(input: IndexedArray2[K,V]): Iterable[String] = {
		val keysUnsorted = input.index.keys.toList
		val keysSorted = keysUnsorted.sort( (a,b) =>
			labeler(a) < labeler(b)
		)
		val labels = keysSorted.map(labeler)
		val headerRow = "IndexedArray" + "\t" + labels.mkString("\t")

		List(headerRow) ++ keysSorted.map( k_row =>
			labeler(k_row) + "\t" + keysSorted.map( k_col => 
				valueTabula.encode( input(k_row)(k_col) )
			).mkString("\t")
		)
	}
	
	def decode(formatted: Iterable[String]): Result[IndexedArray2[K,V]] = {
		val lineList = formatted.toList
		val (firstLine, lines) = ((lineList.head.trim, lineList.tail))
		val keysSorted: List[K] = firstLine.split("\t").toList.tail.flatMap(s => finder(s))
		val index = Map( keysSorted.zipWithIndex : _* )
		val reverseIndex = Map( index.map(p => ((p._2,p._1)) ).toList : _*)
		val keysTotal = keysSorted.length
		
		val array = maker(keysTotal)
		lines.foreach { line =>
			val fields = line.trim.split("\t").toList
			val (rowKeyString, valueFields) = ((fields.head, fields.tail))
			val rowKeyOpt = finder(rowKeyString)
			if (! rowKeyOpt.isDefined) 
				return Left(new Exception("Unknown row key: " + rowKeyString))
			val rowKey = rowKeyOpt.get

			if (valueFields.length != keysTotal)
				return Left(new Exception("Encountered row with " + valueFields.length + " entries instead of expected " + keysTotal + ": " + line))
			
			val valueResults = valueFields.map( v => valueTabula.decode(v) )
			valueResults.toList.zipWithIndex.foreach { pair =>
				val (vr, columnIndex) = pair
				if (vr.isLeft)
					return Left( vr.left.get )
				val value = vr.right.get
				array( index(rowKey) )( columnIndex ) = value
			}
		}
		
		Right(IndexedArray2(array, index))
	}
}
