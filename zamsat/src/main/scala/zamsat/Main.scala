package zamsat

object Main extends App {
  val problem = Utils.generateRandomFormula(4, 5, 3)
  val solution = new Solver().solve(problem)
  val ok = solution match {
    case Some(model) => Utils.probablySatisfies(model, problem)
    case None => true
  }
  println("Problem: " + Utils.formulaToString(problem))
  println("Solution(" + (if (ok) "✔" else "✘") + "): " + solution)
}
