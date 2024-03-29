package net.harnly.dendron.algorithm

trait VertexAlgorithm[V,E <: Edge[E],G <: Graph[V,E],B]
extends Function1[G,Map[V,B]]
