import org.scalatest.flatspec.AnyFlatSpec

import zamsat.{Solver, IterativeSolver}
import scala.collection.mutable.ArrayBuffer

class SATSpec extends AnyFlatSpec {

    "{ 1 2 } { -1 2 }" should "solve to (2 -> true)" in {
        val sat = new Solver().solve(List(List(1, 2), List(-1, 2)))
        assert(sat == Some(Map(2 -> true)))
    }

}

class IterSATSpec extends AnyFlatSpec {

    "{ 1 2 } { -1 2 }" should "solve to (_, true)" in {
        val sat = new IterativeSolver(2, ArrayBuffer(List(1, 2), List(-1, 2))).solve()
        assert(sat.isDefined && sat.get(1))
    }

    "{ 1 } { 2 }" should "solve to (true, true)" in {
        val sat = new IterativeSolver(2, ArrayBuffer(List(1), List(2))).solve()
        assert(sat.isDefined)
        assert(sat.get.sameElements(Array(true, true)))
    }

    "{ 1 } { -1 -2 }" should "solve to (true, false)" in {
        val sat = new IterativeSolver(2, ArrayBuffer(List(1), List(-1, -2))).solve()
        assert(sat.isDefined)
        assert(sat.get.sameElements(Array(true, false)))
    }

    "{ 1 } { -1 -2} {2 3}" should "solve to (true, false, true)" in {
        val sat = new IterativeSolver(3, ArrayBuffer(List(1), List(-1, -2), List(2, 3))).solve()
        assert(sat.isDefined)
        assert(sat.get.sameElements(Array(true, false, true)))
    }

    "{ 1 } { -1 -2} {4 3} {-4 3} {-3 4}" should "solve to (true, false, true, true)" in {
        val sat = new IterativeSolver(4, ArrayBuffer(List(1), List(-1, -2), List(4, 3), List(-4, 3), List(4, -3))).solve()
        assert(sat.isDefined)
        assert(sat.get.sameElements(Array(true, false, true, true)))
    }

    "{ 1 } { -1 -2} {4 3} {-4 3} {-3 4} {-3 -4}" should "solve to None" in {
        val sat = new IterativeSolver(4, ArrayBuffer(List(1), List(-1, -2), List(4, 3), List(-4, 3), List(4, -3), List(-4, -3))).solve()
        assert(sat.isEmpty)
    }
}
