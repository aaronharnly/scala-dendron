package net.harnly.dendron.datatypes

case class IndexedArray2[K,V](array: Array[Array[V]], index: Map[K,Int])
{
	lazy val length1 = array.length
	lazy val length2 = array(0).length

	def apply(x1: K, x2: K) = array( index(x1) )( index(x2) )
}
