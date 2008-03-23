package net.harnly.dendron.algorithm

trait EdgeAlgorithm[V,E <: Edge[E],G <: Graph[V,E],B]
extends Function1[G,Map[E,B]]
