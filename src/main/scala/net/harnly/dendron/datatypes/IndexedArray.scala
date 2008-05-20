package net.harnly.dendron.datatypes

case class IndexedArray2[K,V](array: Array[Array[V]], index: Map[K,Int])
{
	lazy val length1 = array.length
	lazy val length2 = array(0).length

	def apply(k1: K, k2: K) = array( index(k1) )( index(k2) )
	def apply(k1: K) = array(index(k1))
}
