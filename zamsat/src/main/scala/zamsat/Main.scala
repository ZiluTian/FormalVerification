package zamsat

import scala.collection.mutable.ArrayBuffer
import scala.sys.exit

object Main extends App {
  val num_clauses = 538
  val num_vars = 125
  val satisfiable = true
  val num_problems = 100

  var times = List[Double]()

  for (i <- 1 to num_problems) {
    val uf = if (satisfiable) "uf" else "uuf"
    val filename = f"benchmarks/$uf$num_vars-$num_clauses/$uf$num_vars-0$i.cnf"
    println("\n" + filename)
    val problem = new BenchmarkReader().readTestAsSetList(filename)
    val solver = new IterativeSolver(num_vars, problem.to(ArrayBuffer))
    println("Problem: " + Utils.formulaToString(problem))

    val startTime = System.nanoTime()
    val sol = solver.solve()
    val totalTime = (System.nanoTime() - startTime) / 1e9d
    times ::= totalTime

    sol match {
      case Some(sol) =>
        // this is not nice :D
        val sol2 = sol.toList.zipWithIndex.map({ case (a, b) => (b + 1, a) }).toMap
        println("Solution: " + sol2)
        if (Utils.probablySatisfies(sol2, problem)) {
          println("OK")
        } else {
          println("BAD")
          exit()
        }
      // comment out the next break to get all solutions
      //break()
      case None =>
        println("No solution found!")
        if (satisfiable) {
          exit()
        }
    }
  }
  val avg = times.sum / times.length
  println(f"AVG: ${avg}")
}