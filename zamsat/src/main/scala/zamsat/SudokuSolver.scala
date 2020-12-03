package zamsat

import scala.collection.mutable.ArrayBuffer
import zamsat.IterativeSolver
import zamsat.SudokuReader

class SudokuSolver(sideSize: Int) {
  private val setSize : Int = sideSize * sideSize

  // to which variable number value d in cell (i, j) maps to
  private def cellToNumber(i: Int, j: Int, d: Int): Int = i * setSize * setSize + j * setSize + d + 1

  private def makeSetValidityClauses(cellsX: Array[Int], cellsY: Array[Int]): ArrayBuffer[List[Int]] = {
    var result = new ArrayBuffer[List[Int]]()
    for (i <- cellsX.indices) {
      for (j <- i + 1 until cellsX.length) {
        for (d <- 0 until setSize) {
          result.append(List(-cellToNumber(cellsX(i), cellsY(i), d), -cellToNumber(cellsX(j), cellsY(j), d)))
        }
      }
    }
    result
  }

  private def constructValidityRequirements(): ArrayBuffer[List[Int]] = {
    var result = new ArrayBuffer[List[Int]]()
    // definedness
    for (i <- 0 until setSize) {
      for (j <- 0 until setSize) {
        result.append((0 until setSize).map(d => cellToNumber(i, j, d)).toList)
      }
    }
    // uniqueness
    for (i <- 0 until setSize) {
      for (j <- 0 until setSize) {
        for (d1 <- 0 until setSize) {
          for (d2 <- d1 + 1 until setSize) {
            result.append(List(-cellToNumber(i, j, d1), -cellToNumber(i, j, d2)))
          }
        }
      }
    }
    // validity
    // rows and columns
    for (i <- 0 until setSize) {
      val cellsX : Array[Int] = (0 until setSize).toArray
      val cellsY : Array[Int] = Array.fill(setSize){i}
      result.appendAll(makeSetValidityClauses(cellsX, cellsY))
      result.appendAll(makeSetValidityClauses(cellsY, cellsX))
    }
    // regions
    for (i <- 0 until sideSize) {
      for (j <- 0 until sideSize) {
        val cellsX = (0 until sideSize).flatMap(x => (0 until sideSize).map(y => i * sideSize + x))
        val cellsY = (0 until sideSize).flatMap(x => (0 until sideSize).map(y => j * sideSize + y))
        result.appendAll(makeSetValidityClauses(cellsX.toArray, cellsY.toArray))
      }
    }
    return result
  }

  def solve(problem: Array[Array[Int]]): Option[Array[Array[Int]]] = {
    require(problem.length == setSize && problem.forall(_.length == setSize))
    var requirements = constructValidityRequirements()
    for (i <- 0 until setSize) {
      for (j <- 0 until setSize) {
        if (problem(i)(j) > -1) {
          requirements.append(List(cellToNumber(i, j, problem(i)(j))))
        }
      }
    }
    val solver = new IterativeSolver(setSize * setSize * setSize, requirements)
    val res = solver.solve()
    res match {
      case None => None
      case Some(state) => {
        val answer = Array.fill(setSize)(Array.fill(setSize)(-1))
        for (i <- 0 until setSize) {
          for (j <- 0 until setSize) {
            for (d <- 0 until setSize) {
              if (state(cellToNumber(i, j, d) - 1)) {
                answer(i)(j) = d
              }
            }
          }
        }
        Some(answer)
      }
    }
  }
}

object SudokuPrinter {
  def printSudoku(sideSize: Int, table: Array[Array[Int]]): Unit = {
    var setSize = sideSize * sideSize
    for (i <- 0 until setSize) {
      if (i % sideSize == 0) {
        for (j <- 0 until setSize) {
          if (j % sideSize == 0) {
            print('+')
          }
          print('-')
        }
        println('+')
      }
      for (j <- 0 until setSize) {
        if (j % sideSize == 0) {
          print('|')
        }
        if (table(i)(j) > -1) {
          print(table(i)(j) + 1)
        } else {
          print('.')
        }
      }
      println('|')
    }
    for (j <- 0 until setSize) {
      if (j % sideSize == 0) {
        print('+')
      }
      print('-')
    }
    println('+')
  }

  def printSimple(table: Array[Array[Int]]): Unit = {
    for (i <- table) {
      for (j <- i) {
        print(j + 1)
        print(' ')
      }
      println()
    }
  }
}

object RunSudoku extends App {
  var reader = new SudokuReader(3)
  val problem = reader.readFile("sudoku/2.s3")
  println("Problem:")
  SudokuPrinter.printSudoku(3, problem)
  var solver = new SudokuSolver(3)
  val res = solver.solve(problem)
  println("Solution:")
  res match {
    case None => println("No solution")
    case Some(table) => {
      SudokuPrinter.printSudoku(3, res.get)
    }
  }
}
