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
	def makeAnIntTree(p: Gen.Params) = makeATree[Int](Gen.choose(1,10000000))(p)
	val treeGen = new Gen(makeAnIntTree)
	
	val dummyIntEdge = new SimpleDirectedEdge(1,1)
}
