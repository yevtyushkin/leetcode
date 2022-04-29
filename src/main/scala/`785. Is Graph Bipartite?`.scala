package com.yevtyushkin.leetcode

import scala.annotation.tailrec

object `785. Is Graph Bipartite?` {
  object Solution {
    type NodeSet     = Set[Int]
    type NodeRoute   = List[Int]
    type ColorSets   = (NodeSet, NodeSet)
    type Graph       = Array[Array[Int]]
    type PaintResult = (Boolean, Set[Int])

    def isBipartite(graph: Graph): Boolean = {
      val nodeCount = graph.length
      val allNodes  = Set.tabulate(nodeCount)(identity)
      canPaintAllNodes(graph = graph, nodesLeft = allNodes)
    }

    @tailrec
    def canPaintAllNodes(graph: Graph, nodesLeft: NodeSet): Boolean =
      if (nodesLeft.isEmpty) true
      else {
        val initialNode                = nodesLeft.head
        val (couldPaint, visitedNodes) = paintGraph(
          currentNode = initialNode,
          redNodes = Set(initialNode),
          blackNodes = Set.empty,
          visitedNodes = Set.empty,
          nodesToVisit = List.empty,
          graph = graph
        )

        if (couldPaint)
          canPaintAllNodes(
            graph = graph,
            nodesLeft = nodesLeft.filterNot(visitedNodes)
          )
        else false
      }

    @tailrec
    def paintGraph(
      currentNode: Int,
      redNodes: NodeSet,
      blackNodes: NodeSet,
      visitedNodes: NodeSet,
      nodesToVisit: NodeRoute,
      graph: Graph
    ): PaintResult = {
      val adjacentNodes = graph(currentNode)

      val (currentNodeSet, oppositeNodeSet) =
        if (redNodes.contains(currentNode))
          (redNodes, blackNodes)
        else
          (blackNodes, redNodes)

      if (adjacentNodes.exists(currentNodeSet)) (false, visitedNodes)
      else {
        val newOppositeNodeSet = oppositeNodeSet ++ adjacentNodes
        val newNodesToVisit    = adjacentNodes.filterNot(visitedNodes).toList ::: nodesToVisit
        val newVisitedNodes    = visitedNodes + currentNode

        newNodesToVisit match {
          case nextNode :: nextNodesToVisit =>
            paintGraph(
              currentNode = nextNode,
              redNodes = currentNodeSet,
              blackNodes = newOppositeNodeSet,
              visitedNodes = newVisitedNodes,
              nodesToVisit = nextNodesToVisit,
              graph
            )
          case Nil                          => (true, newVisitedNodes)
        }
      }
    }
  }
}
