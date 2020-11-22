package zamsat

import scala.collection.immutable.List
import scala.collection.immutable.Map
import scala.collection.mutable.ListBuffer

object Solver {
  type Literal = Int
  type Clause = List[Literal]
  type ClauseDB = List[Clause]
  // each literal has a decision level
  type Model = Map[Literal, Boolean]
}

class Solver() {
  import Solver._
  import IG._

  private def literalToAssignment(v: Literal): (Literal, Boolean) = v.abs -> (v > 0)

  private def satisfy(clauses: ClauseDB, v: Literal): ClauseDB =
    clauses.filterNot(_.contains(v)).map(_.filter(_ != -v))

  private def unitPropagation(clauses: ClauseDB): (ClauseDB, Model) =
    clauses.find(_.size == 1).map(_.head) match {
      case Some(v) =>
        val (c, m) = unitPropagation(satisfy(clauses, v))
        (c, m + literalToAssignment(v))
      case None => (clauses, Map())
  }

  private def pureLiteral(clauses: ClauseDB): (ClauseDB, Model) = {
    val literals = clauses.foldLeft(Set[Literal]()) { (s1, s2) => s1.union(s2.toSet) }
    val units = literals.filterNot(e => literals.contains(-e))
    (clauses.filterNot(_.exists(units.contains)), units.map(literalToAssignment).toMap)
  }

  private def learnClause(g: IG, decisionLevel: Int): Clause = {
    //    println("Learned clause: " + g.OneUIP(decisionLevel).map(n => -n.literal))
    g.OneUIP(decisionLevel).map(n => -n.literal)
  }

  // deduction
  private def implications(clauses: ClauseDB, env: Model, graph: IG, dLevel: Int): (ClauseDB, Model, IG) = {

    val precedingLits = (clause: Clause) => clause.slice(0, clause.length-2)

    // if all prec assignments are evaluated to false, then implication assigns the last literal true
    val implied = (clause: Clause) =>
//      !env.contains(clause.last.abs) &&
      precedingLits(clause).foldLeft(true)((x, v) => x && env.contains(v.abs)) &&
      !precedingLits(clause).foldLeft(true)((x, v) => x && env.get(v.abs).get)

    val impliedClauses = clauses.filter(implied)

    var updateGraph: IG = graph
    val learnedClauses: ListBuffer[Clause] = ListBuffer[Clause]()

    impliedClauses match {
      case Nil => (clauses, env, graph)
      case _ => {
        for (clause <- impliedClauses) {
          if (precedingLits(clause).forall(literal => graph.getLiteral(-literal).isDefined)){
            // add the implied node and its edges
            if (!graph.getLiteral(clause.last).isDefined) {
              updateGraph = graph.add(ImpliedNode(clause.last, dLevel))
            }
            updateGraph = precedingLits(clause).foldLeft(updateGraph)((x, y) => x.add(
              Edge(graph.getLiteral(y).get, ImpliedNode(clause.last, dLevel))))
            // conflict
            if (graph.getLiteral(-clause.last).isDefined) {
              learnedClauses.append(learnClause(graph, dLevel))
            }
          }
        }

//        val learnedClauses: ClauseDB =
//          for {
//            clause <- impliedClauses
//            if (env.contains(clause.last.abs) && !env.get(clause.last.abs).get)
//          } yield learnClause(clauses, graph, dLevel, ImpliedNode(clause.last, dLevel))

        (learnedClauses.toList ++ clauses.filterNot(implied), env ++ impliedClauses.map(_.last)
          .map(v => literalToAssignment(v)).toMap, graph)
      }
    }
  }

  private def dpll(clauses: ClauseDB, decisionLevel: Int, implicationGraph: IG): Option[Model] = {
    val (uClauses, uModel) = unitPropagation(clauses)
    val (pClauses, pModel) = pureLiteral(uClauses)
    val (iClauses, iModel, graph) = implications(pClauses, uModel ++ pModel, implicationGraph, decisionLevel)

    if (iClauses.isEmpty) {
      Some(iModel)
    } else if (iClauses.exists(_.isEmpty)) {
      None
    } else {
      val literal = iClauses.head.head
      val nextDL: Int = decisionLevel + 1
      for (v <- Seq(literal, -literal)) {
        dpll(satisfy(clauses, v), nextDL, graph.add(DecisionNode(v, nextDL))) match {
          case Some(model) => return Some(iModel ++ model + literalToAssignment(v))
          case _ =>
        }
      }
      None
    }
  }

  def solve(clauses: ClauseDB): Option[Model] = {
    val implicationGraph: IG = new IG((List[DecisionNode](), List[ImpliedNode](), List[Edge]()))

    dpll(clauses, 0, implicationGraph)
  }
}
