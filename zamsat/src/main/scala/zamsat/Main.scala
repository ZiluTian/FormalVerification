package zamsat

import scala.util.control.Breaks.{break, breakable}
import scala.collection.mutable.ArrayBuffer

object Main extends App {
  val filename = "benchmarks/uf20-91/uf20-02.cnf"
  val problem = new BenchmarkReader().readTestAsSetList(filename)
  val solver = new IterativeSolver(20, problem.to(ArrayBuffer))
  println("Problem: " + Utils.formulaToString(problem))

  breakable {
    while (true) {
      solver.solve() match {
        case Some (sol) =>
          // this is not nice :D
          val sol2 = sol.toList.zipWithIndex.map({case (a,b) => (b + 1, a) }).toMap
          println("Solution: " + sol2)
          if (Utils.probablySatisfies(sol2, problem)) {
            println("OK")
          } else {
            println("BAD")
          }
          // comment out the next break to get all solutions
          //break()
        case None =>
          println("No solution found!")
          break()
      }
      break()
    }
  }



}
