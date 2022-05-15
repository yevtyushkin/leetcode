package com.yevtyushkin.leetcode

import scala.annotation.tailrec

object `1302. Deepest Leaves Sum` {
  // Given Definition
  class TreeNode(
    _value: Int = 0,
    _left: TreeNode = null,
    _right: TreeNode = null
  ) {
    var value: Int      = _value
    var left: TreeNode  = _left
    var right: TreeNode = _right
  }

  object Solution {
    type Depth      = Int
    type Sum        = Int
    type RouteEntry = (TreeNode, Depth)
    type Route      = List[RouteEntry]

    def deepestLeavesSum(root: TreeNode): Sum =
      collectLeavesSum(root)
        .maxByOption { case (depth, _) => depth }
        .map { case (_, sum) => sum }
        .getOrElse(0)

    @annotation.tailrec
    def collectLeavesSum(
      currentNode: TreeNode,
      currentDepth: Depth = 0,
      acc: Map[Depth, Sum] = Map.empty,
      route: Route = List.empty
    ): Map[Depth, Sum] = {
      val childrenRouteEntries = {
        val childDepth = currentDepth + 1
        Option(currentNode.left).map((_, childDepth)).toList ++ Option(currentNode.right).map((_, childDepth))
      }

      val (newAcc, newRoute) = childrenRouteEntries match {
        case Nil      =>
          val value  = currentNode.value
          val newAcc = acc + (currentDepth -> acc.get(currentDepth).map(_ + value).getOrElse(value))
          (newAcc, route)
        case nonEmpty =>
          (acc, nonEmpty ::: route)
      }

      newRoute match {
        case (node, depth) :: tail =>
          collectLeavesSum(
            currentNode = node,
            currentDepth = depth,
            acc = newAcc,
            route = tail
          )
        case Nil                   => newAcc
      }
    }
  }
}
