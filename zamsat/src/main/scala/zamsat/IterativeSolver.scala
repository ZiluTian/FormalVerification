package zamsat

import scala.annotation.tailrec
import scala.collection.immutable.List

class IterativeSolver(numVars: Int, clauses: List[List[Int]]) {
  private final val state         : Array[Int] = Array.fill(numVars){0}
  private final val assignments   : Array[Int] = Array.fill(numVars){0}
  private final val decisions     : Array[Int] = Array.fill(numVars){0}
  private final var decisionLevel : Int        = -1
  private final var level         : Int        = -1

  private final val UNASSIGNED = 0
  private final val ASSIGN_TRUE = 1
  private final val ASSIGN_FALSE = 2
  private final val doDebug = false

  private final def debug(s: String): Unit = if (doDebug) println(s)

  private final def checkConflict(): Boolean = {
    clauses.exists(_.forall(v =>
      state(v.abs - 1) != UNASSIGNED &&
      state(v.abs - 1) != (if (v > 0) ASSIGN_TRUE else ASSIGN_FALSE)))
  }

  private final def allAssigned(): Boolean = level == numVars - 1

  private final def getUnassigned: Option[Int] =
    state.zipWithIndex.find({ case (el, idx) => el == UNASSIGNED }).map({ case (el, idx) => idx + 1 })

  private final def assign(literal: Int): Int = {
    level += 1
    debug(f"Assigning $literal at level $level (decisionlevel $decisionLevel)")
    state(literal.abs - 1) = if (literal > 0) ASSIGN_TRUE else ASSIGN_FALSE
    assignments(level) = literal
    debug(state.toList.toString())
    level
  }

  private final def decide(literal: Int): Int = {
    decisionLevel += 1
    debug(f"Deciding $literal at decisionlevel $decisionLevel")
    decisions(decisionLevel) = assign(literal)
    decisionLevel
  }

  @tailrec
  private final def backtrack(): Boolean = {
    if (decisionLevel >= 0) {
      debug(f"Backtracking from decision level $decisionLevel (level $level)")
      for (i <- level to decisions(decisionLevel) by -1) {
        debug(f"Unassigning ${assignments(i)} (level $i)")
        state(assignments(i).abs - 1) = UNASSIGNED
      }
      level = decisions(decisionLevel) - 1
      decisionLevel = decisionLevel - 1
      val decision = assignments(level + 1)
      if (decision > 0) {
        decide(-decision)
        true
      } else {
        backtrack()
      }
    } else {
      false
    }
  }

  @tailrec
  private final def unitPropagation(): Unit = {
    debug("Doing unit")
    def unit(clause: List[Int]): Int = {
      if (clause.exists(e => state(e.abs - 1) == (if (e > 0) ASSIGN_TRUE else ASSIGN_FALSE))) {
        0
      } else {
        val unassigned = clause.filter(e => state(e.abs - 1) == UNASSIGNED)
        if (unassigned.size == 1) {
          debug("unit: " + unassigned.head + "(" + clause + ")")
          debug((clause.map(e => state(e.abs - 1) == (if (e > 0) ASSIGN_TRUE else ASSIGN_FALSE)).toString()))
          unassigned.head
        } else {
          0
        }
      }
    }
    clauses.map(unit).find(_ != 0) match {
      case Some(v) => assign(v); unitPropagation()
      case _ =>
    }
  }

  private final def decide(): Boolean = {
    getUnassigned match {
      case Some(v) => decide(v); true
      case _ => false
    }
  }

  private final def dpll(): Boolean = {
    while (true) {
      unitPropagation()
      if (checkConflict()) {
        if (!backtrack()) {
          return false
        }
      } else if (allAssigned()) {
        return true
      } else {
        decide()
      }
    }
    false
  }

  def solve(): Option[Array[Boolean]] = {
    backtrack()
    debug("going again!")
    if (dpll()) {
      debug(state.map(e => e == ASSIGN_TRUE).mkString(", "))
      Some(state.map(e => e == ASSIGN_TRUE))
    } else {
      None
    }
  }

}
