package zamsat

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.Random
import zamsat.Assignment

import scala.collection.immutable.List

class LiteralWatcher(numVars: Int, clauses: ArrayBuffer[List[Int]]) {
  private final val random = new Random(123123123)

  private final val positiveWatchedClauses : Array[mutable.HashSet[Int]] = Array.fill(numVars){new mutable.HashSet[Int]()}
  private final val negativeWatchedClauses : Array[mutable.HashSet[Int]] = Array.fill(numVars){new mutable.HashSet[Int]()}
  private final val watchedLiterals : ArrayBuffer[mutable.HashSet[Int]] = ArrayBuffer.fill(clauses.length){new mutable.HashSet[Int]()}

  // next two methods don't check whether the invariants hold
  private final def addWatchedLiteral(clauseId: Int, literal: Int): Unit = {
    watchedLiterals(clauseId).add(literal)
    if (literal > 0) {
      positiveWatchedClauses(literal.abs - 1).add(clauseId)
    } else {
      negativeWatchedClauses(literal.abs - 1).add(clauseId)
    }
  }

  private final def removeWatchedLiteral(clauseId: Int, literal: Int): Unit = {
    watchedLiterals(clauseId).remove(literal)
    if (literal > 0) {
      positiveWatchedClauses(literal.abs - 1).remove(clauseId)
    } else {
      negativeWatchedClauses(literal.abs - 1).remove(clauseId)
    }
  }

  // finds all implied clauses watched by a literal and returns implied literals from them
  // this function also resets watched literal assignments
  final def getImpliedLiterals(state: Array[Int], checkLiteral: Int): Option[List[(Int, List[Int])]] = {
    // unit finds a unit literal in a clause if there is one, otherwise returns 0
    // clause must be unsatisfied
    def unit(clause: List[Int]): Int = {
      // get all unassigned literals
      val unassigned = clause.filter(e => state(e.abs - 1) == Assignment.UNASSIGNED)
      // if there is just one, return it; otherwise return 0
      if (unassigned.size == 1) {
        unassigned.head
      } else {
        0
      }
    }
    //  select relevant clauses
    val possibleClauses: List[Int] = (checkLiteral match {
      case v if v > 0 => negativeWatchedClauses(v.abs - 1)
      case v if v < 0 => positiveWatchedClauses(v.abs - 1)
      case 0 => new mutable.HashSet[Int]
    }).toList.filter(id => clauses(id).forall(e => state(e.abs - 1) != Assignment.satAssignment(e)))
    val impliedValues = new ListBuffer[(Int, List[Int])]()

    for (clauseId <- possibleClauses) {
      var impliedLiteral = unit(clauses(clauseId))
      if (impliedLiteral == 0) {
        if (!resetWatchedLiterals(clauseId, state)) {
          return None
        }
      } else {
        impliedValues.append((impliedLiteral, clauses(clauseId)))
      }
    }
    Some(impliedValues.toList)
  }

  private final def selectWatchedLiterals(clauseId: Int, unknownLiterals: List[Int]): Unit = {
    val clauseLength = unknownLiterals.length
    val firstPos = random.nextInt(clauseLength)
    addWatchedLiteral(clauseId, unknownLiterals(firstPos))
    if (clauseLength > 1) {
      var secondPos = firstPos
      while (firstPos == secondPos) {
        secondPos = random.nextInt(clauseLength)
      }
      addWatchedLiteral(clauseId, unknownLiterals(secondPos))
    }
  }

  // clear currently watched literals and select new ones
  private final def resetWatchedLiterals(clauseId: Int, state: Array[Int]): Boolean = {
    val unknownLiterals: List[Int] = clauses(clauseId).view.filter(e => state(e.abs - 1) == Assignment.UNASSIGNED).toList
    if (unknownLiterals.isEmpty) {
      // then the clause is false
      return false
    }

    for (literal <- watchedLiterals(clauseId)) {
      removeWatchedLiteral(clauseId, literal)
    }

    selectWatchedLiterals(clauseId, unknownLiterals)
    true
  }

  // assigns watched literals to a clause
  // at least 2 literals in the clause should be unknown
  final def addClause(clauseId: Int, state: Array[Int]): Boolean = {
    val unknownLiterals: List[Int] = clauses(clauseId).view.filter(e => state(e.abs - 1) == Assignment.UNASSIGNED).toList
    require(unknownLiterals.length >= 2)
    selectWatchedLiterals(clauseId, unknownLiterals)
    true
  }

  final def prepareWatchedLiterals(): Unit = {
    for (clauseId <- clauses.indices) {
      selectWatchedLiterals(clauseId, clauses(clauseId))
    }
  }
}
