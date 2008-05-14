package net.harnly.dendron.algorithm

object OptionMath
{
	def add(a: Option[Int], b: Option[Int]): Option[Int] = for(
		x <- a;
		y <- b
	) yield (x + y)
	
	def min(a: Option[Int], b: Option[Int]): Option[Int] = a match {
		case None => b
		case Some(x) => b match {
			case None => Some(x)
			case Some(y) => if (y < x) Some(y) else Some(x)
		}
	}
	
	def max(a: Option[Int], b: Option[Int]): Option[Int] = {
		val best = a match {
			case None => b
			case Some(x) => b match {
				case None => Some(x)
				case Some(y) => if (y > x) Some(y) else Some(x)
			}
		}
		best
	}	
}