package net.harnly.dendron.algorithms

trait VertexAlgorithm[V,E <: Edge[E],G <: Graph[V,E],B]
extends Function1[G,Map[V,B]]
