package zamsat

import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._
import scala.io.Source

class BenchmarkReader {
  def readTest(name: String): ArrayBuffer[ArrayBuffer[Int]] = {
    var file = Source.fromFile(name)
    var result = new ArrayBuffer[ArrayBuffer[Int]]

    try {
      breakable {
        for (line <- file.getLines()) {
          if (line.length > 0) {
            line.charAt(0) match {
              case 'c' => {} // this is comment
              case 'p' => {
                // problem statement
              }
              case '%' => {break()} // don't know what it is
              case _ => {
                var clause = new ArrayBuffer[Int]
                for (number <- line.split(' ').filter(x => x.length > 0)) {
                  if (number.toInt != 0)
                    clause.addOne(number.toInt)
                }
                result.addOne(clause)
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

  def readTestAsSetList(name: String): List[Set[Int]] = {
    var data = readTest(name)
    List.from(data.view.map(x => Set.from(x)))
  }
}
