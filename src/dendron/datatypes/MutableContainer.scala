package net.harnly.dendron.datatypes

trait MutableContainer[A]
extends Seq[A]
{
	def insert(item: A)
	def remove: A
}

class MutableStack[A]
extends MutableContainer[A]
{
	private val stack = new scala.collection.mutable.Stack[A]
	
	// MutableContainer methods
	def insert(item: A) = stack.push(item)
	def remove = {
		val item = stack.top
		stack.pop
		item
	}
	
	// Seq methods
	def elements = stack.elements
	def length = stack.length
	def apply(n: Int) = stack.apply(n)
}

class MutableQueue[A]
extends MutableContainer[A]
{
	private val queue = new scala.collection.mutable.Queue[A]
	
	// MutableContainer methods
	def insert(item: A) = queue.enqueue(item)
	def remove = queue.dequeue
	
	// Seq methods
	def elements = queue.elements
	def length = queue.length
	def apply(n: Int) = queue.apply(n)
}


