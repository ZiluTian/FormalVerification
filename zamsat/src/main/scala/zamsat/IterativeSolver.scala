package zamsat

import scala.annotation.tailrec
import scala.collection.immutable.List
import scala.collection.mutable.ArrayBuffer

class IterativeSolver(numRealVars: Int, private var clauses: ArrayBuffer[List[Int]]) {
  // we add a fake variable to deal with initial unit clauses
  private final val numVars       : Int = numRealVars + 1
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

  // all variables are assigned a truth value if we are at assignment level numVars - 1
  private final def allAssigned: Boolean = level == numVars - 1

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
    // don't backtrack the first decision bc it was made on fake variable
    if (decisionLevel >= 1) {
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
        decide(-decision)
        true
      } else {
        // otherwise we backtrack another decision level
        backtrack()
      }
    } else {
      // if the current decision level is 0 then we cannot backtrack any further (unsat)
      false
    }
  }

  private final def unitPropagation(): Boolean = {
    // assume that the last assignment was a decision
    require(decisionLevel >= 0 && decisions(decisionLevel) == level)
    debug("Doing unit")
    var currentLevel = level
    // check the consequences of assignment at each level, starting with current one
    do {
      // unit finds a unit literal in a clause if there is one, otherwise returns 0
      val impliedLiterals = literalWatcher.getImpliedLiterals(state, assignments(currentLevel))
      impliedLiterals match {
        case Some(literals) =>
          for (literal <- literals) {
            // check for double assignment
            if (state(literal.abs - 1) == Assignment.UNASSIGNED) {
              assign(literal)
            }
          }
        case None => return false
      }

      currentLevel += 1
    } while (currentLevel <= level)
    true
  }

  private final def decide(): Unit = {
    // get the first unassigned variable and decide that it is true
    // returns true if a decision was made, false otherwise
    getUnassigned match {
      case Some(v) => decide(v)
      case _ =>
    }
  }

  private final def dpll(): Boolean = {
    // do a decision on fake variable
    // it always should be assigned 0
    decide(-numVars)
    while (true) {
      if (!unitPropagation()) {
        if (!backtrack()) {
          return false
        }
      } else if (allAssigned) {
        return true
      } else {
        decide()
      }
    }
    false
  }

  def solve(): Option[Array[Boolean]] = {
    // ensure that all clauses have at least 2 variables
    if (clauses.exists(_.isEmpty)) {
      return None
    }
    // adding fake variable to all unit clauses
    clauses = clauses.map(x =>
      if (x.length > 1) {
        x
      } else {
        numVars :: x
      }
    )
    literalWatcher.prepareWatchedLiterals()
    // we backtrack here to be able to return multiple solutions
    // the first time solve is called backtrack will do nothing because the decision level is -1
    backtrack()
    debug("going again!")
    if (dpll()) {
      debug(state.map(e => e == Assignment.TRUE).mkString(", "))
      Some(state.slice(0, state.length - 1).map(e => e == Assignment.TRUE))
    } else {
      None
    }
  }

}
