package zamsat

import scala.util.Random

object Utils {

  // picks random truth values for unassigned variables to complete a model
  // then checks if that model satisfies the formula
  def probablySatisfies(model: Map[Int, Boolean], clauses: List[Set[Int]]): Boolean = {
    val completeModel = model ++ clauses.foldLeft(Set[Int]()) { (s1, s2) => s1.union(s2) }.map(_.abs)
      .diff(model.keySet).map(k => k -> Random.nextBoolean()).toMap
    clauses.forall((s: Set[Int]) => s.exists(e => completeModel.get(e.abs) match {
      case Some(b) => (e > 0) == b // satisfied literal
      case None => false           // unassigned literal
    }))
  }

  def generateRandomFormula(nLiterals: Int, nClauses: Int, maxLiteralsPerClause: Int): List[Set[Int]] = {
    (1 to nClauses).map(_ => (1 to maxLiteralsPerClause).map(_ =>
      (if (Random.nextBoolean()) 1 else -1) * (Random.nextInt(nLiterals) + 1)).toSet ) }.toList

  def formulaToString(clauses: List[Set[Int]]): String = clauses.map(c => "{ " + c.mkString(" ") + " }").mkString(" ")

}
