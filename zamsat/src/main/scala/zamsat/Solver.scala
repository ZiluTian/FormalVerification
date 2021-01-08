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

  private def dpll(clauses: ClauseDB, decisionLevel: Int): Option[Model] = {
    val (uClauses, uModel) = unitPropagation(clauses)
    val (pClauses, pModel) = pureLiteral(uClauses)

    if (pClauses.isEmpty) {
      Some(uModel ++ pModel)
    } else if (pClauses.exists(_.isEmpty)) {
      None
    } else {
      val literal = pClauses.head.head
      val nextDL: Int = decisionLevel + 1
      for (v <- Seq(literal, -literal)) {
        dpll(satisfy(clauses, v), nextDL) match {
          case Some(model) => return Some(uModel ++ pModel ++ model + literalToAssignment(v))
          case _ =>
        }
      }
      None
    }
  }

  def solve(clauses: ClauseDB): Option[Model] = {

    dpll(clauses, 0)
  }
}
