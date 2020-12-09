package zamsat

import scala.annotation.tailrec
import scala.collection.immutable.List
import scala.collection.mutable.ArrayBuffer

class IterativeSolver(numVars: Int, private var clauses: ArrayBuffer[List[Int]]) {
  // state records the assigned truth value of all variables
  // the truth value of variable i can be found at state(i - 1)
  private final val state         : Array[Int] = Array.fill(numVars){0}
  // assignments is used as a stack that records the history of literal assignments (current = level)
  private final val assignments   : Array[Int] = Array.fill(numVars){0}
  // decisions records the history of decisions that were made, also used as a stack (current = decisionLevel)
  private final val decisions     : Array[Int] = Array.fill(numVars){0}
  private final var decisionLevel : Int        = -1
  private final var level         : Int        = -1

  private final val litsPerClause : Array[Int] = Array.fill(clauses.size){0}
  private final val satCounter    : Array[Int] = Array.fill(clauses.size){0}
  private final val unsatCounter  : Array[Int] = Array.fill(clauses.size){0}
  private final val varClauses : Array[List[Int]] = Array.fill(clauses.size*2){Nil}
  private final def varToCtrIdx(v: Int) = (v.abs - 1) * 2 + (if (v > 0) 0 else 1)

  private final def clauseIsImplied(id: Int) = satCounter(id) == 0 && unsatCounter(id) == litsPerClause(id) - 1

  for ((clause, index) <- clauses.zipWithIndex) {
    litsPerClause(index) = clause.size
    for (variable <- clause) {
      val varIdx = varToCtrIdx(variable)
      varClauses(varIdx) = index :: varClauses(varIdx)
    }
  }
  private final def counterAssign(v: Int): Option[List[Int]] = {
    for (clause <- varClauses(varToCtrIdx(v))) {
      satCounter(clause) += 1
    }
    var impliedClauses : List[Int] = Nil
    var conflict = false
    for (clause <- varClauses(varToCtrIdx(-v))) {
      unsatCounter(clause) += 1
      if (unsatCounter(clause) == litsPerClause(clause)) {
        conflict = true
      } else if (clauseIsImplied(clause)) {
        impliedClauses = clause :: impliedClauses
      }
    }
    if (conflict) {
      None
    } else {
      Some(impliedClauses)
    }
  }
  private final def counterUnassign(v: Int): Unit = {
    for (clause <- varClauses(varToCtrIdx(v))) {
      satCounter(clause) -= 1
    }
    for (clause <- varClauses(varToCtrIdx(-v))) {
      unsatCounter(clause) -= 1
    }
  }

  private final val doDebug = false

  private final def debug(s: => String): Unit = if (doDebug) println(s)

  // all variables are assigned a truth value if we are at assignment level numVars - 1
  private final def allAssigned: Boolean = level == numVars - 1

  // getUnassigned optionally return the first unassigned variable it can find (lowest number)
  private final def getUnassigned: Option[Int] =
    state.zipWithIndex.find({ case (el, idx) => el == Assignment.UNASSIGNED }).map({ case (el, idx) => idx + 1 })

  // assign takes in a literal and makes it so that literal is satisfied
  // (will overwrite previous truth value if there is one)
  private final def assign(literal: Int): (Int, Boolean) = {
    val currentLevel = level+1
    level = currentLevel
    debug(f"Assigning $literal at level $level (decisionlevel $decisionLevel)")
    state(literal.abs - 1) = if (literal > 0) Assignment.TRUE else Assignment.FALSE
    assignments(level) = literal

    counterAssign(literal) match {
      case Some(l) =>
        for (clause <- l) {
          clauses(clause).find(v => state(v.abs - 1) == Assignment.UNASSIGNED) match {
            case Some(lit) =>
              if (!assign(lit)._2) {
                return (currentLevel, false)
              }
            case None =>
          }
        }
        (currentLevel, true)
      case None =>
        (currentLevel, false)
    }
  }

  // decide takes in a literal makes it so that literal is satisfied,
  // but also records that this was a decision so we can revert it if needed
  private final def decide(literal: Int): (Int, Boolean) = {
    decisionLevel += 1
    debug(f"Deciding $literal at decisionlevel $decisionLevel")
    val (lvl, noConflict) = assign(literal)
    decisions(decisionLevel) = lvl
    (decisionLevel, noConflict)
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
        counterUnassign(assignments(i))
      }
      level = decisions(decisionLevel) - 1
      decisionLevel = decisionLevel - 1
      val decision = assignments(level + 1)
      if (decision > 0) {
        // if the decision we reversed was a positive literal, we still have to try its negation
        if (decide(-decision)._2) {
          true
        } else {
          backtrack()
        }
      } else {
        // otherwise we backtrack another decision level
        backtrack()
      }
    } else {
      // if the current decision level is 0 then we cannot backtrack any further (unsat)
      false
    }
  }

  private final def decide(): Boolean = {
    decide(getUnassigned.get)._2
  }

  private final def dpll(): Boolean = {
    while (true) {
      if (allAssigned) {
        return true
      }
      if (!decide()) {
        if (!backtrack()) {
          return false
        }
      }
    }
    false
  }

  def solve(): Option[Array[Boolean]] = {
    if (dpll()) {
      debug(state.map(e => e == Assignment.TRUE).mkString(", "))
      Some(state.map(e => e == Assignment.TRUE))
    } else {
      None
    }
  }

}
