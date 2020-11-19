package zamsat

import scala.collection.immutable.List
import scala.collection.immutable.Map

object Solver {
  type Literal = Int
  type Clause = List[Literal]
  type ClauseDB = List[Clause]
}

class Solver() {
  import Solver._
  
  private def literalToAssignment(v: Literal): (Literal, Boolean) = v.abs -> (v > 0)

  private def satisfy(clauses: ClauseDB, v: Literal): ClauseDB =
    clauses.filterNot(_.contains(v)).map(_.filter(_ != -v))

  private def unitPropagation(clauses: ClauseDB): (ClauseDB, Map[Literal, Boolean]) =
    clauses.find(_.size == 1).map(_.head) match {
      case Some(v) =>
        val (c, m) = unitPropagation(satisfy(clauses, v))
        (c, m + literalToAssignment(v))
      case None => (clauses, Map())
  }

  private def pureLiteral(clauses: ClauseDB): (ClauseDB, Map[Literal, Boolean]) = {
    val literals = clauses.foldLeft(Set[Literal]()) { (s1, s2) => s1.union(s2.toSet) }
    val units = literals.filterNot(e => literals.contains(-e))
    (clauses.filterNot(_.exists(units.contains)), units.map(literalToAssignment).toMap)
  }

  // deduction
  private def implications(clauses: ClauseDB, env: Map[Literal, Boolean]): (ClauseDB, Map[Literal, Boolean]) = {
    // if all prec assignments are evaluated to false, then implication assigns the last literal true
    val implied = (clause: Clause) => !env.contains(clause.last.abs) &&
      clause.slice(0, clause.length-2).foldLeft(true)((x, v) => x && env.contains(v.abs)) &&
      !clause.slice(0, clause.length-2).foldLeft(true)((x, v) => x && env.get(v.abs).get)

    val impliedClauses = clauses.filter(implied)

    impliedClauses match {
      case Nil => (clauses, env)
      case _ => (clauses.filterNot(implied), env ++ impliedClauses.map(_.last).map(v => v.abs -> true).toMap)
    }
  }

  private def dpll(clauses: ClauseDB, decisionLevel: Int): Option[Map[Literal, Boolean]] = {
    val (uClauses, uModel) = unitPropagation(clauses)
    val (pClauses, pModel) = pureLiteral(uClauses)
    val (iClauses, iModel) = implications(pClauses, uModel ++ pModel)

    if (iClauses.isEmpty) {
      Some(iModel)
    } else if (iClauses.exists(_.isEmpty)) {
      None
    } else {
      val literal = iClauses.head.head
      val nextDL: Int = decisionLevel + 1
      for (v <- Seq(literal, -literal)) {
        dpll(satisfy(clauses, v), nextDL) match {
          case Some(model) => return Some(iModel ++ model + literalToAssignment(v))
          case _ =>
        }
      }
      None
    }
  }

  def solve(clauses: ClauseDB): Option[Map[Literal, Boolean]] = dpll(clauses, 0)

}
