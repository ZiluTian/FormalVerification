package zamsat

import zamsat.Solver
import zamsat.BenchmarkReader
import scala.io.Source

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
