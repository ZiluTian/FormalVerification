package zamsat

import zamsat.Solver
import zamsat.BenchmarkReader
import scala.io.Source
import scala.math.max

object Benchmark extends App{
  var sum: Double = 0;
  var satisfied: Int = 0;
  var cnt: Int = 0;
  for (name <- Source.stdin.getLines()) {
    if (name.length > 0) {
      val filename = "benchmarks/" + name
      val problem = new BenchmarkReader().readTestAsSetList(filename)
      val startTime = System.nanoTime()
      val result = new Solver().solve(problem)
      val totalTime = (System.nanoTime() - startTime) / 1e9d
          result match {
            case None => satisfied += 0
            case Some(_) => satisfied += 1
          }
      sum += totalTime
      cnt += 1
    }
  }
  printf("Satisfied: %d\nMean: %f\n", satisfied, sum / cnt)
}

object BenchmarkIterative extends App{
  var sum: Double = 0;
  var satisfied: Int = 0;
  var cnt: Int = 0;

  for (name <- Source.stdin.getLines()) {
    if (name.length > 0) {
      val filename = "benchmarks/" + name
      val problem = new BenchmarkReader().readTest(filename).map(_.toList)
      val numVars = max(problem.map(_.max).max, -problem.map(_.min).min)
      val solver = new IterativeSolver(numVars, problem)
      val startTime = System.nanoTime()
      val result = solver.solve()
      val totalTime = (System.nanoTime() - startTime) / 1e9d
      result match {
        case None => satisfied += 0
        case Some(_) => satisfied += 1
      }
      sum += totalTime
      cnt += 1
    }
  }
  printf("Satisfied: %d\nMean: %f\n", satisfied, sum / cnt)
}
