package net.harnly.dendron.datatypes

trait ImmutableContainer[+A]
extends Seq[A]
{
	def insert[B >: A](item: B): ImmutableContainer[B]
	def remove: (A, ImmutableContainer[A])
}

case class ImmutableStack[+A](
	stack: scala.collection.immutable.Stack[A]
)
extends ImmutableContainer[A]
{
	def this() = this(
		new scala.collection.immutable.Stack[A]()
	)
	
	// MutableContainer methods
	def insert[B >: A](item: B) = ImmutableStack[B](
		stack.push(item)
	)
	def remove = {
		val item = stack.top
		(item, ImmutableStack(stack.pop))
	}
	
	// Seq methods
	def elements = stack.elements
	def length = stack.length
	def apply(n: Int) = stack.apply(n)
}

case class ImmutableQueue[+A](
	queue: scala.collection.immutable.Queue[A]
)
extends ImmutableContainer[A]
{
	def this() = this(
		new scala.collection.immutable.Queue[A]()
	)
	
	// MutableContainer methods
	def insert[B >: A](item: B) = ImmutableQueue[B](
		queue.enqueue(item)
	)
	def remove = {
		val dequeued = queue.dequeue
		(dequeued._1, ImmutableQueue(dequeued._2))
	}
	
	// Seq methods
	def elements = queue.elements
	def length = queue.length
	def apply(n: Int) = queue.apply(n)
}


