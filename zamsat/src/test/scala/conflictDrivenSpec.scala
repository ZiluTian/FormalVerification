import org.scalatest.flatspec.AnyFlatSpec
import zamsat.IG
import zamsat.IG._
import zamsat.IterativeSolver
import scala.collection.mutable.ArrayBuffer

import scala.collection.immutable.List

class conflictDrivenSpec extends AnyFlatSpec {
  "{ 1 2 } { 1 3 7 } { -2 -3 4 } { -4 5 8 } { -4 6 9 } { -5 -6 }" should "add a new conflict clause, (-4, 8, 9)" in {
    val sat = new IterativeSolver(9, ArrayBuffer(List(1, 2), List(1, 3, 7), List(-2, -3, 4), List(-4, 5, 8), List(-4, 6, 9), List(-5, -6))).solve()
    println(sat)
  }
}
