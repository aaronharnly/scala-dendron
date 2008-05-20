package net.harnly.dendron.datatypes

import org.specs._
import org.specs.runner.JUnit4
import org.specs.matcher.ScalacheckParameters._

class indexedArrayTest extends JUnit4(indexedArrayTest)
object indexedArrayTest extends Specification
{
	"Indexed array 2" should {
		"Retrieve properly" in {
			val index = Map( "a" -> 0, "b" -> 1, "c" -> 2 )
			val a = new Array[Array[Int]](3,2)
			a(0)(0) = 0
			a(0)(1) = 1
			a(1)(0) = 2
			a(1)(1) = 3
			a(2)(0) = 4
			a(2)(1) = 5
			
			val indexed = new IndexedArray2(a, index)
			
			indexed("a","a") must_== 0
			indexed("a","b") must_== 1
			indexed("b","a") must_== 2
			indexed("b","b") must_== 3
			indexed("c","a") must_== 4
			indexed("c","b") must_== 5
			
			indexed("b").toList must_== List(2,3)
			
		}
	}
}