package zamsat

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.control.Breaks.{break, breakable}

class SudokuReader(sideSize: Int) {
  private val setSize : Int = sideSize * sideSize

  def readFile(filename: String): Array[Array[Int]] = {
    val file = Source.fromFile(filename)
    val result = Array.fill(setSize)(Array.fill(setSize)(-1))

    var curLine = 0
    try {
      breakable {
        for (line <- file.getLines()) {
          if (line.length > 0) {
            line.charAt(0) match {
              case '#' => {} // this is comment
              case _ => {
                require(line.length == setSize)
                for (i <- 0 until setSize) {
                  if (line(i) != '.' && line(i) != '0') {
                    result(curLine)(i) = line(i) - '1'
                  }
                }
                curLine += 1
              }
            }
          }
        }
      }
    } finally {
      file.close()
    }
    result
  }
}

class SudokuBenchmarkReader(sideSize: Int, filename: String) {
  private val setSize : Int = sideSize * sideSize
  private val file : Source = Source.fromFile(filename)
  private val lines : Iterator[String] = file.getLines()

  def closeFile(): Unit = {
      file.close()
  }

  def readNext(): Array[Array[Int]] = {
    val result = Array.fill(setSize)(Array.fill(setSize)(-1))
    var line = lines.next()
    while (line.length == 0) {
      line = lines.next()
    }
    line = line.split(';')(0)
    require(line.length == setSize * setSize)
    for (i <- 0 until setSize) {
      for (j <- 0 until setSize) {
        if (line(i * setSize + j) != '.') {
          result(i)(j) = line(i * setSize + j) - '1'
        }
      }
    }
    result
  }
}
