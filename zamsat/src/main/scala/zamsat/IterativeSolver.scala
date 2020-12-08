package zamsat

import scala.annotation.tailrec
import scala.collection.immutable.List
import scala.collection.mutable.ArrayBuffer
import IG._

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

  private val implicationGraph: IG = new IG()

  private val enableIG: Boolean = true

  // varClauses is an array of lists of clause indices that contain a particular literal
  // the list for literal v can be found at (v.abs - 1) * 2 + (if (v > 0) 0 else 1)
  private final val varClauses : Array[List[Int]] = Array.fill(numVars*2){Nil}
  private final def varToCtrIdx(v: Int) = (v.abs - 1) * 2 + (if (v > 0) 0 else 1)
  for ((clause, index) <- clauses.zipWithIndex) {
    for (variable <- clause) {
      val varIdx = varToCtrIdx(variable)
      varClauses(varIdx) = index :: varClauses(varIdx)
    }
  }
  // order determines the decision order by the amount of times a literal occurs in clauses (more occurrences -> get picked first)
  private final val order = constructOrder()
  private final var orderIdx = 0
  private final def constructOrder(): Array[Int] = {
    val dupOrder = varClauses.map(_.size).zipWithIndex.sortBy(_._1)(Ordering[Int].reverse).map(e => (if (e._2 % 2 == 0) 1 else -1) * (e._2 / 2 + 1))
    dupOrder.foldLeft(List[Int](numVars)) {
      case (acc, item) if acc.contains(-item) => acc
      case (acc, item) => item::acc
    }.reverse.toArray
  }

  private final val doDebug = false

  private final val literalWatcher = new LiteralWatcher(numVars, clauses)

  private final def debug(s: => String): Unit = if (doDebug) println(s)

  // all variables are assigned a truth value if we are at assignment level numVars - 1
  private final def allAssigned: Boolean = level == numVars - 1

  // assign takes in a literal and makes it so that literal is satisfied
  // (will overwrite previous truth value if there is one)
  private final def assign(literal: Int, clause: Option[List[Int]] = None): Int = {
    level += 1
    debug(f"Assigning $literal at level $level (decisionlevel $decisionLevel)")
    state(literal.abs - 1) = if (literal > 0) Assignment.TRUE else Assignment.FALSE

    if (enableIG) {
      clause match {
        case None => // decision node assign
          debug(f"Adding decision node to IG $decisionLevel")
          implicationGraph.add(DecisionNode(literal, decisionLevel))
        case Some(c) => { // implication node
          if (c.diff(List(literal)).forall(l => implicationGraph.getLiteral(-l).isDefined)){
            debug(f"Adding implication node to IG $decisionLevel")
            if (!implicationGraph.getLiteral(literal).isDefined){
              implicationGraph.add(ImpliedNode(literal, decisionLevel))
            }

            c.diff(List(literal))
              .map(n => implicationGraph.add(Edge(implicationGraph.getLiteral(-n).get, ImpliedNode(literal, decisionLevel))))

            if (implicationGraph.getLiteral(-literal).isDefined) {
              val learnedClause: List[Int] = implicationGraph.OneUIP(decisionLevel)
              debug(f"Conflict detected in IG!")
              clauses.addOne(learnedClause)
              literalWatcher.addClause(clauses.length, state)
              // update the implication graph to reflect the learned clause. Consider making it part of the learning clause.
              implicationGraph.removeConflictNodes(implicationGraph.UIPS(decisionLevel).last)
            }
          }
        }
      }
    }

    assignments(level) = literal
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

      if (enableIG){
        implicationGraph.removeLevelNodes(decisionLevel)
      }

      level = decisions(decisionLevel) - 1
      decisionLevel = decisionLevel - 1
      val decision = assignments(level + 1)
      while (order(orderIdx).abs != decision.abs) {
        orderIdx -= 1
      }
      if (order(orderIdx) == decision) {
        // if the decision we reversed was the same sign as defined in [order], we still have to try its negation
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
          for ((literal, c) <- literals) {
            // check for double assignment
            if (state(literal.abs - 1) == Assignment.UNASSIGNED) {
              assign(literal, Some(c))
            }
          }
        case None => return false
      }

      currentLevel += 1
    } while (currentLevel <= level)
    true
  }

  private final def decide(): Unit = {
    while(state(order(orderIdx).abs - 1) != Assignment.UNASSIGNED) {
      orderIdx += 1
    }
    decide(order(orderIdx))
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
