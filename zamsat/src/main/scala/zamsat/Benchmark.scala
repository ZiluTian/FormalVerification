package zamsat

import zamsat.Solver
import zamsat.BenchmarkReader

object Benchmark extends App{
  for (name <- List("uf20-91/uf20-01.cnf", "uf50-218/uf50-01.cnf")) {
    printf("Running %s\n", name)
    val filename = "benchmarks/" + name
    val problem = new BenchmarkReader().readTestAsSetList(filename)
    val startTime = System.nanoTime()
    val result = new Solver().solve(problem)
    val totalTime = (System.nanoTime() - startTime) / 1e9d
    result match {
      case None => println("Wrong answer")
      case Some(_) => println("Correct answer")
    }
    printf("Total time: %f\n", totalTime)
  }
}
