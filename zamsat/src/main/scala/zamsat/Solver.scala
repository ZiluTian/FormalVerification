package zamsat

import scala.collection.immutable.List
import scala.collection.immutable.Map

class Solver(clauses: List[Set[Int]]) {

  private def literalToAssignment(v: Int): (Int, Boolean) = v.abs -> (v > 0)

  private def satisfy(clauses: List[Set[Int]], v: Int): List[Set[Int]] =
    clauses.filterNot(_.contains(v)).map(_.filter(_ != -v))

  private def unitPropagation(clauses: List[Set[Int]]): (List[Set[Int]], Map[Int, Boolean]) =
    clauses.find(_.size == 1).map(_.head) match {
      case Some(v) =>
        val (c, m) = unitPropagation(satisfy(clauses, v))
        (c, m + literalToAssignment(v))
      case None => (clauses, Map())
  }

  private def pureLiteral(clauses: List[Set[Int]]): (List[Set[Int]], Map[Int, Boolean]) = {
    val literals = clauses.foldLeft(Set[Int]()) { (s1, s2) => s1.union(s2) }
    val units = literals.filterNot(e => literals.contains(-e))
    (clauses.filterNot(_.exists(units.contains)), units.map(literalToAssignment).toMap)
  }

  private def dpll(clauses: List[Set[Int]]): Option[Map[Int, Boolean]] = {
    val (uClauses, uModel) = unitPropagation(clauses)
    val (pClauses, pModel) = pureLiteral(uClauses)
    if (pClauses.isEmpty) {
      Some(uModel ++ pModel)
    } else if (pClauses.exists(_.isEmpty)) {
      None
    } else {
      val literal = pClauses.head.head
      for (v <- Seq(literal, -literal)) {
        dpll(satisfy(clauses, v)) match {
          case Some(model) => return Some(uModel ++ pModel ++ model + literalToAssignment(v))
          case _ =>
        }
      }
      None
    }
  }

  def solve(): Option[Map[Int, Boolean]] = dpll(clauses)

}
