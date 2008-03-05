package net.harnly.dendron.algorithms

trait GraphAlgorithm[V,E <: Edge[E],G <: Graph[V,E],B]
extends Function1[G,B]
