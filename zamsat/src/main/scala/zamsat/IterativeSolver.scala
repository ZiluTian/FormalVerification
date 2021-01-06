package zamsat

import scala.annotation.tailrec
import scala.collection.immutable.List
import scala.collection.mutable.ArrayBuffer
import IG._

import scala.collection.mutable

class IterativeSolver(numRealVars: Int, private var clauses: ArrayBuffer[List[Int]], vsidsDecay: Double = .85) {
  // we add a fake variable to deal with initial unit clauses
  private final val numVars              : Int = numRealVars + 1
  // state records the assigned truth value of all variables
  // the truth value of variable i can be found at state(i - 1)
  private final val state               : Array[Int] = Array.fill(numVars){0}
  // assignments is used as a stack that records the history of literal assignments (current = level)
  private final val assignments         : Array[Int] = Array.fill(numVars){0}
  // decisions records the history of decisions that were made, also used as a stack (current = decisionLevel)
  private final val decisions           : Array[Int] = Array.fill(numVars){0}
  private final val bothPolaritiesTried : Array[Boolean] = Array.fill(numVars){false}
  private final var decisionLevel       : Int        = -1
  private final var level               : Int        = -1

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
  private var scores = mutable.Map[Int, Double]()
  (1 to numRealVars).foreach(i => {
    scores(i) = 0
    scores(-i) = 0
  })
  clauses.foreach(c => c.foreach(l => scores(l) += 1))

  private final val doDebug = false

  private final val literalWatcher = new LiteralWatcher(numVars, clauses)

  private final def debug(s: => String): Unit = if (doDebug) println(s)

  // all variables are assigned a truth value if we are at assignment level numVars - 1
  private final def allAssigned: Boolean = level == numVars - 1

  // add the conflicting clause c and literal to implication graph
  private final def addImplicationNode(c: List[Int], literal: Int): Unit = {
    assert(enableIG)
    val implicationExist: Option[Node] = implicationGraph.getLiteral(literal)

    if (implicationExist.isDefined) {
      assert(implicationExist.get.isInstanceOf[ImpliedNode])
    }

    val antecedentAssign: List[Int] = c.filterNot(e => e.abs == literal.abs)

    if (antecedentAssign.forall(l => implicationGraph.getLiteral(-l).isDefined)) {
      var impLevel = decisionLevel
      if (c.length > 1) {
        impLevel = antecedentAssign.map(l => implicationGraph.getLiteral(-l).get.level).max
      }

      if (!implicationExist.isDefined) {
        debug(f"Adding implication $literal node $impLevel $c")
        implicationGraph.add(ImpliedNode(literal, impLevel))
      }

      assert(implicationGraph.getLiteral(assignments(decisions(decisionLevel))) == implicationGraph.getDecisionNode(decisionLevel))

      antecedentAssign
        .map(n => implicationGraph.add(Edge(implicationGraph.getLiteral(-n).get, ImpliedNode(literal, impLevel))))

      if (implicationGraph.getLiteral(-literal).isDefined) {
        debug(f"Conflict detected in IG!")

        val cNode1: Node = implicationGraph.getLiteral(literal).get
        val cNode2: Node = implicationGraph.getLiteral(-literal).get

        // Conflict nodes should only be implied nodes
        debug("Conflicting nodes: " + cNode1 + cNode2)
        if (cNode1.isInstanceOf[DecisionNode]) {
          implicationGraph.removeNode(cNode1)
          return
        }

        if (cNode2.isInstanceOf[DecisionNode]) {
          implicationGraph.removeNode(cNode1)
          return
        }

        //        assert(cNode1.isInstanceOf[ImpliedNode])
        //        assert(cNode2.isInstanceOf[ImpliedNode])

        val relevantDecisions: Set[DecisionNode] =
          implicationGraph.getAllDecisionParents(cNode1)
          .union(implicationGraph.getAllDecisionParents(cNode2))

        val uipsPerLevel: Set[(DecisionNode, List[Node])] = relevantDecisions.map(n => (n, implicationGraph.UIPS(cNode1, cNode2, n)))

        var conflictSide: Set[Node] = Set(cNode1, cNode2)

        uipsPerLevel.map(n => {
          if (n._2.isEmpty) {
            conflictSide = conflictSide.union(implicationGraph.cut(n._1))
          } else {
            conflictSide = conflictSide.union(implicationGraph.cut(n._2.last))
          }
        })

        val learnedClause: List[Int] = implicationGraph.conflictClause(conflictSide.diff(uipsPerLevel.flatMap(x => x._2)))
        learnedClause.foreach(l => scores(l) += 1)
        scores = scores.map{ case (l, s) => (l, s * vsidsDecay) }
        debug(f"Learned clause: $learnedClause")

        clauses.addOne(learnedClause)
        literalWatcher.addClause(learnedClause, state)

        conflictSide.foreach(n => implicationGraph.removeNode(n))
      }
    }
  }
  // assign takes in a literal and makes it so that literal is satisfied
  // (will overwrite previous truth value if there is one)
  private final def assign(literal: Int, clause: Option[List[Int]] = None): Int = {
    level += 1
    debug(f"Assigning $literal at level $level (decisionlevel $decisionLevel)")
    state(literal.abs - 1) = if (literal > 0) Assignment.TRUE else Assignment.FALSE

    if (enableIG) {
      clause match {
        case None => // decision node assign
          debug(f"Adding decision node $literal  $decisionLevel")
          assert(!implicationGraph.getLiteral(literal).isDefined)
          implicationGraph.add(DecisionNode(literal, decisionLevel))
        case Some(c) => { // implication node
          addImplicationNode(c, literal)
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
        if (enableIG){
          implicationGraph.removeLiteral(assignments(i))
          assert(implicationGraph.getLiteral(assignments(i)) == None)
        }
        state(assignments(i).abs - 1) = Assignment.UNASSIGNED
      }

      implicationGraph.getImpliedNodes(decisionLevel).foreach(n => implicationGraph.removeNode(n))
      assert(implicationGraph.getDecisionNode(decisionLevel).isEmpty)

      level = decisions(decisionLevel) - 1
      decisionLevel = decisionLevel - 1
      val decision = assignments(level + 1)
      if (!bothPolaritiesTried(decisionLevel + 1)) {
        debug(f"Trying reverse for decision level ${decisionLevel + 1}")
        // if the decision we reversed was the same sign as defined in [order], we still have to try its negation
        decide(-decision)
        bothPolaritiesTried(decisionLevel) = true
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

  @tailrec
  private final def unitHelper(impliedLiterals: List[(Int, Option[List[(Int, List[Int])]])], speculateLiteral: Int): Boolean = {
    impliedLiterals match {
      case List() => true
      case x :: y =>
        x match {
          case (_, Some(literals)) =>
            for ((literal, c) <- literals) {
              // check for double assignment
              if (state(literal.abs - 1) == Assignment.UNASSIGNED) {
                assign(literal, Some(c))
              }
            }
          case (cid, None) =>   // found a conflict
            debug(f"Conflict detected! ${clauses(cid)}")
            if (enableIG) {
              addImplicationNode(clauses(cid), -speculateLiteral)
            }
            return false
          case _ =>
            assert(false)
        }
        unitHelper(y, speculateLiteral)
    }
  }

  private final def unitPropagation(): Boolean = {
    // assume that the last assignment was a decision
    require(decisionLevel >= 0 && decisions(decisionLevel) == level)
//    debug("Doing unit")
    var currentLevel = level
    // check the consequences of assignment at each level, starting with current one
    do {
      val speculateLiteral: Int = assignments(currentLevel)
      val impliedLiterals = literalWatcher.getImpliedLiterals(state, speculateLiteral)
      if (!unitHelper(impliedLiterals, speculateLiteral)) {
        return false
      }
      currentLevel += 1
    } while (currentLevel <= level)
    true
  }

  private final def decide(): Unit = {
    val next = scores.filter(kv => state(kv._1.abs - 1) == Assignment.UNASSIGNED).maxBy(_._2)._1
    debug(f"VSIDS: $next (score: ${scores(next)})")
    decide(next)
    bothPolaritiesTried(decisionLevel) = false
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
