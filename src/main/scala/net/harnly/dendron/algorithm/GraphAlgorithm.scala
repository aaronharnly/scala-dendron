package net.harnly.dendron.algorithm

trait GraphAlgorithm[V,E <: Edge[E],G <: Graph[V,E],B]
extends Function1[G,B]
