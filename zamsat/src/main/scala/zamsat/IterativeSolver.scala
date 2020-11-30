package zamsat

import scala.annotation.tailrec
import scala.collection.immutable.List
import scala.collection.mutable.ArrayBuffer
import zamsat.LiteralWatcher
import zamsat.Assignment

class IterativeSolver(numVars: Int, clauses: ArrayBuffer[List[Int]]) {
  // state records the assigned truth value of all variables
  // the truth value of variable i can be found at state(i - 1)
  private final val state         : Array[Int] = Array.fill(numVars){0}
  // assignments is used as a stack that records the history of literal assignments (current = level)
  private final val assignments   : Array[Int] = Array.fill(numVars){0}
  // decisions records the history of decisions that were made, also used as a stack (current = decisionLevel)
  private final val decisions     : Array[Int] = Array.fill(numVars){0}
  private final var decisionLevel : Int        = -1
  private final var level         : Int        = -1

  private final val doDebug = false

  private final val literalWatcher = new LiteralWatcher(numVars, clauses)

  private final def debug(s: => String): Unit = if (doDebug) println(s)

  // we have a conflict if there is at least one clause of which every literal is assigned the wrong truth value
  // (so all assigned but none satisfied)
  private final def checkConflict(): Boolean = {
    clauses.exists(_.forall(v =>
      state(v.abs - 1) != Assignment.UNASSIGNED &&
      state(v.abs - 1) != (if (v > 0) Assignment.TRUE else Assignment.FALSE)))
  }

  // all variables are assigned a truth value if we are at assignment level numVars - 1
  private final def allAssigned(): Boolean = level == numVars - 1

  // getUnassigned optionally return the first unassigned variable it can find (lowest number)
  private final def getUnassigned: Option[Int] =
    state.zipWithIndex.find({ case (el, idx) => el == Assignment.UNASSIGNED }).map({ case (el, idx) => idx + 1 })

  // assign takes in a literal and makes it so that literal is satisfied
  // (will overwrite previous truth value if there is one)
  private final def assign(literal: Int): Int = {
    level += 1
    debug(f"Assigning $literal at level $level (decisionlevel $decisionLevel)")
    state(literal.abs - 1) = if (literal > 0) Assignment.TRUE else Assignment.FALSE
    assignments(level) = literal
    debug(state.toList.toString())
    level
  }

  // decide takes in a literal makes it so that literal is satisfied,
  // but also records that this was a decision so we can revert it if needed
  private final def decide(literal: Int): Int = {
    decisionLevel += 1
    debug(f"Deciding $literal at decisionlevel $decisionLevel")
    decisions(decisionLevel) = assign(literal)
    decisionLevel
  }

  // backtrack undoes all assigments from the current decision level
  @tailrec
  private final def backtrack(): Boolean = {
    if (decisionLevel >= 0) {
      debug(f"Backtracking from decision level $decisionLevel (level $level)")
      // undoes all assigments including the assigment of the decision itself
      for (i <- level to decisions(decisionLevel) by -1) {
        debug(f"Unassigning ${assignments(i)} (level $i)")
        state(assignments(i).abs - 1) = Assignment.UNASSIGNED
      }
      level = decisions(decisionLevel) - 1
      decisionLevel = decisionLevel - 1
      val decision = assignments(level + 1)
      if (decision > 0) {
        // if the decision we reversed was a positive literal, we still have to try its negation
        decide(-decision)
        true
      } else {
        // otherwise we backtrack another decision level
        backtrack()
      }
    } else {
      // if the current decision level is -1 then we cannot backtrack any further (unsat)
      false
    }
  }

  private final def unitPropagation(): Unit = {
    // assume that the last assignment was a decision
    require(decisionLevel >= 0 && decisions(decisionLevel) == level)
    debug("Doing unit")
    var currentLevel = level
    // check the consequences of assignment at each level, starting with current one
    do {
      // unit finds a unit literal in a clause if there is one, otherwise returns 0
      var impliedLiterals = literalWatcher.getImpliedLiterals(state, assignments(currentLevel))
      impliedLiterals match {
        case Some(literals) => {
          for (literal <- literals) {
            // check for double assignment
            if (state(literal.abs - 1) == Assignment.UNASSIGNED) {
              assign(literal)
            }
          }
        }
        case None => return
      }

      currentLevel += 1
    } while (currentLevel <= level)
  }

  private final def decide(): Boolean = {
    // get the first unassigned variable and decide that it is true
    // returns true if a decision was made, false otherwise
    getUnassigned match {
      case Some(v) => decide(v); true
      case _ => false
    }
  }

  private final def dpll(): Boolean = {
    while (true) {
      if (level > -1) {
        unitPropagation()
      }
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
    literalWatcher.prepareWatchedLiterals()
    // we backtrack here to be able to return multiple solutions
    // the first time solve is called backtrack will do nothing because the decision level is -1
    backtrack()
    debug("going again!")
    if (dpll()) {
      debug(state.map(e => e == Assignment.TRUE).mkString(", "))
      Some(state.map(e => e == Assignment.TRUE))
    } else {
      None
    }
  }

}
