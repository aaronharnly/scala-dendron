package net.harnly.dendron

import org.scalacheck._
import Arbitrary._
import Prop._

object GraphGenerators
{
	import GraphTypes._
	
	val emptyGraph = Gen.elements(new SimpleIntGraph(
		List(),
		List()
	))
	
	val graphsWithOneIntAndNoEdges = for(
		x <- arbitrary[Int]
	) yield new SimpleIntGraph(
		List(x),
		List()
	)

	val graphsWithOneIntAndOneEdge = for(
		x <- arbitrary[Int]
	) yield new SimpleIntGraph(
		List(x),
		List(SimpleDirectedEdge(x,x))
	)
	
	val graphsWithTwoIntsAndTwoEdges = for(
		x <- arbitrary[Int];
		y <- arbitrary[Int] if x != y
	) yield ((x, y, new SimpleIntGraph(
		List(x,y),
		List(
			SimpleDirectedEdge(x,y),
			SimpleDirectedEdge(y,x)
		)
	)))

	val graphsWithTwoIntsAndOneEdge = for(
		x <- arbitrary[Int];
		y <- arbitrary[Int] if x != y
	) yield ((x, y, new SimpleIntGraph(
		List(x,y),
		List(
			SimpleDirectedEdge(x,y)
		)
	)))

	def uniqueList[T](implicit gen: Gen[T], p: Gen.Params): Option[List[T]] = for(
		list <- Gen.listOf[T](gen).apply(p)
	) yield list.removeDuplicates

	def makeATree[T](gen: Gen[T])(p: Gen.Params): Option[DirectedGraph[T,DirectedEdge[T]]] = 
	p.size match { 
		case 0 => None

		case 1 => for(
			val v <- gen.apply(p)
		) yield new SimpleDirectedGraph(
			List(v), 
			List[SimpleDirectedEdge[T]]() 
		)

		case _ => for(
			val tree <- makeATree[T](gen)( Gen.Params( p.size - 1, p.rand ) );
			val v <- gen.apply(p);
			val source <- Gen.elements( tree.vertices.toList : _* ).sample
		) yield tree.addVertex(v).addEdge(
			new SimpleDirectedEdge[T]( 
				source, 
				v
			)
		)
	}
	
	def makeALine[T](gen: Gen[T])(p: Gen.Params): Option[DirectedGraph[T,DirectedEdge[T]]] = 
	p.size match { 
		case 0 => None
		
		case _ => 
			for(
				vList <- uniqueList[T](gen,p)
			) yield {
				val vertices = Set( vList : _* )
				val edges = Set( edgesBetween( vList ) : _* )
				new SimpleDirectedGraph(
					vertices,
					edges
				)
			}
	}
	
	def edgesBetween[V](list: List[V]): List[DirectedEdge[V]] = {
		list match {
			case Nil => Nil
			case x :: Nil => Nil
			case x :: y :: xs => new SimpleDirectedEdge[V](x, y) :: edgesBetween(y :: xs)
		}
	}
	
	def makeAnIntTree(p: Gen.Params) = makeATree[Int](Gen.choose(1,10000000))(p)
	val treeGen = new Gen(makeAnIntTree)

	def makeAnIntLine(p: Gen.Params) = makeALine[Int](Gen.choose(1,10000000))(p)
	val lineGen = new Gen(makeAnIntLine)
	
	val dummyIntEdge = new SimpleDirectedEdge(1,1)
}
