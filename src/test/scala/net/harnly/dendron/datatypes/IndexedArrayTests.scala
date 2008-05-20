package net.harnly.dendron.datatypes

import org.specs._
import org.specs.runner.JUnit4
import org.specs.matcher.ScalacheckParameters._

class indexedArrayTest extends JUnit4(indexedArrayTest)
object indexedArrayTest extends Specification
{
	"Indexed array 2" should {
		"Retrieve properly" in {
			val indexed = ExampleIndexedArray.create
			
			indexed("a","a") must_== 0
			indexed("a","b") must_== 1
			indexed("b","a") must_== 3
			indexed("b","b") must_== 4
			indexed("c","a") must_== 6
			indexed("c","b") must_== 7
			indexed("c","c") must_== 8
			
			indexed("b") must_== Map( "a" -> 3, "b" -> 4, "c" -> 5)
			
		}
		
		"Encode" in {
			import net.harnly.tabula.Tabulae._
			import net.harnly.tabula.StringPrimitiveTabulae._

			val indexed = ExampleIndexedArray.create
			implicit val tabula: Tabula[IndexedArray2[String,Int],Iterable[String]] = new IndexedArrayTabula[String,Int](
				identity,
				{k: String => Some(k)},
				{size: Int => new Array[Array[Int]](size,size)}
			)
			indexed.encodeAs[Iterable[String]].toList must_== List(
				"IndexedArray	a	b	c",
				"a	0	1	2",
				"b	3	4	5",
				"c	6	7	8"
			)
		}
		
		"Decode" in {
			import net.harnly.tabula.Tabulae._
			import net.harnly.tabula.StringPrimitiveTabulae._
			implicit val tabula: Tabula[IndexedArray2[String,Int],Iterable[String]]  = new IndexedArrayTabula[String,Int](
				identity,
				{k: String => Some(k)},
				{size: Int => new Array[Array[Int]](size,size)}
			)

			val text: Iterable[String] = """IndexedArray	a	b	c
a	0	1	2
b	3	4	5
c	6	7	8
""".split("\n")
			val indexedResult = text.decodeAs[IndexedArray2[String,Int]]
			indexedResult.isRight must_== true
			val indexed = indexedResult.right.get
			
			indexed("a","a") must_== 0
			indexed("a","b") must_== 1
			indexed("b","a") must_== 3
			indexed("b","b") must_== 4
			indexed("c","a") must_== 6
			indexed("c","b") must_== 7
			indexed("c","c") must_== 8
		}
	}
}

object ExampleIndexedArray
{
	def create: IndexedArray2[String,Int] = {
		val index = Map( "a" -> 0, "b" -> 1, "c" -> 2 )
		val a = new Array[Array[Int]](3,3)
		a(0)(0) = 0
		a(0)(1) = 1
		a(0)(2) = 2
		a(1)(0) = 3
		a(1)(1) = 4
		a(1)(2) = 5
		a(2)(0) = 6
		a(2)(1) = 7
		a(2)(2) = 8
		
		IndexedArray2(a, index)
	}
}