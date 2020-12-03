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
