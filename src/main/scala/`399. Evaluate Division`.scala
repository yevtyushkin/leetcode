package com.yevtyushkin.leetcode

import scala.annotation.tailrec

object `399. Evaluate Division` {
  object Solution {
    type Variable         = String
    type Equation         = List[Variable]
    type EquationResult   = Double
    type EquationResults  = Map[Variable, Map[Variable, EquationResult]]
    type Query            = List[Variable]
    type QueryResult      = Double
    type VisitedVariables = Set[Variable]
    type CurrentCost      = QueryResult
    type RouteCheckPoint  = (Variable, CurrentCost)

    def calcEquation(
      equations: List[Equation],
      values: Array[EquationResult],
      queries: List[Query]
    ): Array[QueryResult] = {
      val equationsData   = equations.zip(values)
      val equationResults = buildEquationResults(equationsData)

      queries.map(solve(_, equationResults)).toArray
    }

    def buildEquationResults(
      equationsData: List[(Equation, EquationResult)]
    ): EquationResults = {
      def addEquationResult(
        firstVar: Variable,
        secondVar: Variable,
        result: EquationResult,
        equations: EquationResults
      ): EquationResults = {
        val firstVarEquations  = equations.getOrElse(firstVar, Map.empty)
        val secondVarEquations = equations.getOrElse(secondVar, Map.empty)

        firstVarEquations.get(secondVar) match {
          case Some(_) => equations
          case None    =>
            equations +
              (firstVar  -> (firstVarEquations + (secondVar -> result))) +
              (secondVar -> (secondVarEquations + (firstVar -> 1 / result)))
        }
      }

      equationsData.foldLeft[EquationResults](Map.empty) {
        case (equationResults, (equation, equationResult)) =>
          equation match {
            case List(firstVar, secondVar) => addEquationResult(firstVar, secondVar, equationResult, equationResults)
            case _                         => equationResults
          }
      }
    }

    def solve(query: Query, equationResults: EquationResults): QueryResult = {
      @tailrec
      def traverse(
        current: Variable,
        destination: Variable,
        visited: VisitedVariables,
        routeCheckPoints: List[RouteCheckPoint],
        currentCost: CurrentCost
      ): QueryResult = {
        val currentEquationResults = equationResults.getOrElse(current, Map.empty)

        val destinationCostOpt = currentEquationResults.get(destination)
        destinationCostOpt match {
          case Some(cost) => currentCost * cost
          case None       =>
            val newVisited     = visited + current
            val newCheckPoints = currentEquationResults
              .filterNot { case (variable, _) => newVisited(variable) }
              .map {
                case (variable, equationResult) => (variable, currentCost * equationResult)
              }
              .toList ::: routeCheckPoints

            newCheckPoints match {
              case (nextVariable, nextCost) :: nextCheckPoints =>
                traverse(
                  current = nextVariable,
                  destination = destination,
                  visited = newVisited,
                  routeCheckPoints = nextCheckPoints,
                  currentCost = nextCost
                )
              case _                                           => -1
            }
        }
      }

      query match {
        case List(firstVariable, secondVariable) =>
          if (!(equationResults.contains(firstVariable) && equationResults.contains(secondVariable))) -1
          else if (firstVariable == secondVariable) 1
          else
            traverse(
              current = firstVariable,
              destination = secondVariable,
              visited = Set.empty,
              routeCheckPoints = Nil,
              currentCost = 1
            )
        case _                                   => -1
      }
    }
  }
}
