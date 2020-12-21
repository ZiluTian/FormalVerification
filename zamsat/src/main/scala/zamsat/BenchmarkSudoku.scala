package zamsat

import zamsat.SudokuBenchmarkReader
import zamsat.IterativeSolver

object BenchmarkSudoku extends App {
  var testsNum = 30
  var solutionsFound = 0;
  val reader = new SudokuBenchmarkReader(3, "benchmarks/sudoku/01_file1.txt")
  val startTime = System.nanoTime()
  var totalTime: Double = 0
  try {
    for (_ <- 0 until testsNum) {
      val problem = reader.readNext()
      var solver = new SudokuSolver(3)
      val res = solver.solve(problem)
      res match {
        case None => {}
        case Some(_) => {
          solutionsFound += 1
        }
      }
    }
    totalTime = (System.nanoTime() - startTime) / 1e9 / testsNum
  } finally {
    reader.closeFile()
  }
  println(totalTime)
  println(solutionsFound)
}
