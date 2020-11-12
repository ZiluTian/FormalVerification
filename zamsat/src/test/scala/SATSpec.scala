import org.scalatest.flatspec.AnyFlatSpec

import zamsat.Solver

class SATSpec extends AnyFlatSpec {

    "{ 1 2 } { -1 2 }" should "solve to (2 -> true)" in {
        val sat = new Solver(List(Set(1, 2), Set(-1, 2))).solve()
        assert(sat == Some(Map(2 -> true)))
    }

}
