package com.yevtyushkin.leetcode

object `743. Network Delay Time` {
  object Solution {
    type NumberOfNodes     = Int
    type NodeKey           = Int
    type DelayTime         = Int
    type Graph             = Map[NodeKey, Map[NodeKey, DelayTime]]
    type RouteEntry        = (NodeKey, DelayTime)
    type Route             = List[RouteEntry]
    type Visited           = Set[NodeKey]
    type MinimalDelayTimes = Map[NodeKey, DelayTime]

    def networkDelayTime(
      times: Array[Array[Int]],
      n: NumberOfNodes,
      k: NodeKey
    ): Int = {
      val graph = times.foldLeft(Map.empty: Graph) { (graph, delayTimeInfo) =>
        val from      = delayTimeInfo(0)
        val to        = delayTimeInfo(1)
        val delayTime = delayTimeInfo(2)

        graph + (from -> (graph.getOrElse(from, Map.empty) + (to -> delayTime)))
      }

      val minDelayTimes = minimalDelayTimes(
        currentNode = k,
        currentDelay = 0,
        graph = graph,
        acc = Map(k -> 0)
      )

      if (minDelayTimes.size != n) -1 else minDelayTimes.values.maxOption.getOrElse(-1)
    }

    @annotation.tailrec
    def minimalDelayTimes(
      currentNode: NodeKey,
      currentDelay: DelayTime,
      graph: Graph,
      acc: MinimalDelayTimes = Map.empty,
      visited: Visited = Set.empty,
      route: Route = List.empty
    ): MinimalDelayTimes = {
      val adjacent = graph.getOrElse(currentNode, Map.empty)

      val (newAcc, newRoute) = adjacent.foldLeft((acc, route)) {
        case ((acc, route), (nodeKey, delayTime)) =>
          val worstCaseDelay       = currentDelay + delayTime
          val previousMinimalDelay = acc.getOrElse(nodeKey, worstCaseDelay)
          val newMinimalDelay      = Math.min(previousMinimalDelay, worstCaseDelay)
          val needsRecalculation   = previousMinimalDelay > newMinimalDelay
          val newRoute             =
            if (visited(nodeKey) && !needsRecalculation) route
            else (nodeKey, newMinimalDelay) :: route
          val newAcc               = acc + (nodeKey -> newMinimalDelay)
          (newAcc, newRoute)
      }

      newRoute match {
        case (nodeKey, delay) :: tail =>
          minimalDelayTimes(
            currentNode = nodeKey,
            currentDelay = delay,
            graph = graph,
            acc = newAcc,
            visited + currentNode,
            route = tail
          )
        case Nil                      => newAcc
      }
    }
  }
}
