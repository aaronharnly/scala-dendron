package net.harnly.dendron.datatypes

trait UnorderedPair[+A]
extends Iterable[A]
{
	def oneItem: A
	def otherItem(first: A2 forSome {type A2 <: A}): A
}

case class PairSet[+A](
	one: A,
	another: A
) extends UnorderedPair[A]
{
	def elements = new Iterator[A] {
		// ruthlessly unordered!
		val first = if (Math.random < 0.5) one else another
		val second = if (first == one) another else one
		var givenFirst = false
		var givenSecond = false

		def hasNext = ! givenSecond
		def next = if (! givenFirst) {
			givenFirst = true
			first
		} else if (! givenSecond) {
			givenSecond = true
			second
		} else
			throw new RuntimeException("I'm outta elements")
	}
	
	def oneItem: A = elements.first

	def otherItem(first: A2 forSome {type A2 <: A}): A = 
	if (first == one) another else one
}

