package zamsat

import scala.collection.immutable.List
import scala.collection.immutable.Map
import scala.collection.mutable.ListBuffer

object Solver {
  type Literal = Int
  type Clause = List[Literal]
  type ClauseDB = List[Clause]
  // each literal has a decision level
  type Model = Map[Literal, Boolean]

  sealed abstract class Node {
    def literal: Literal
    def level: Int
  }
  case class DecisionNode(literal: Literal, level: Int) extends Node
  case class ImpliedNode(literal: Literal, level: Int) extends Node
  case class Edge(from: Node, to: Node)
  type IG = (List[DecisionNode], List[ImpliedNode], List[Edge])
}

class Solver() {
  import Solver._

  private def addNode(g: IG, n: Node): IG = {
    n match {
      case n: DecisionNode => (n:: g._1, g._2, g._3)
      case n: ImpliedNode => (g._1, n:: g._2, g._3)
    }
  }

  private def addEdge(g: IG, e: Edge): IG = {
    (g._1, g._2, e :: g._3)
  }

  private def getLiteral(g: IG, l: Literal): Option[Node] = {
    val decisionVar = g._1.filter(_.literal == l).headOption
    if (decisionVar.isDefined){
      decisionVar
    } else {
      g._2.filter(_.literal == l).headOption
    }
  }

  private def getParents(g: IG, n: Node): List[Node] = {
    g._3.filter(e => e.to == n).map(_.from)
  }

  private def getChildren(g: IG, n: Node): List[Node] = {
    g._3.filter(e => e.from == n).map(_.to)
  }

  // return all the paths between from and to
  private def allPaths(g: IG, from: Node, to: Node, path: List[Node], paths: ListBuffer[List[Node]]): Unit = {
    val candEdges: List[Edge] = g._3.filter(_.from == from)
    candEdges match {
      case Nil =>
      case _ => {
        val candNodes: List[Node] = candEdges.map(_.to).filter(n => n.level == from.level)
        if (candNodes.contains(to)) {
          paths.append(List.concat(path, List(from, to)))
        } else {
          candNodes.foreach(n => allPaths(g, n, to, path :+ from, paths))
        }
      }
    }
  }

  private def OneUIP(g: IG, decisionLevel: Int, conflict: ImpliedNode): List[Node] = {
    val dNode: DecisionNode = g._1.filter(n => n.level == decisionLevel).head
    val paths: ListBuffer[List[Node]] = new ListBuffer[List[Node]]()
    allPaths(g, dNode, conflict, List(), paths)
    assert(!paths.isEmpty)
    val uips: List[Node] = paths(0).foldLeft(List[Node]())({(x, y) => if (paths.forall(_.contains(y))) x:+y else x})
    getChildren(g, uips.last).foldLeft(Set[Node]())((x, y) => x.union(getParents(g, y).toSet)).toList
  }

  private def learnClause(clauses: ClauseDB, g: IG, decisionLevel: Int, conflict: ImpliedNode): Clause = {
    OneUIP(g, decisionLevel, conflict).map(n => -n.literal)
  }

  private def literalToAssignment(v: Literal): (Literal, Boolean) = v.abs -> (v > 0)

  private def satisfy(clauses: ClauseDB, v: Literal): ClauseDB =
    clauses.filterNot(_.contains(v)).map(_.filter(_ != -v))

  private def unitPropagation(clauses: ClauseDB): (ClauseDB, Model) =
    clauses.find(_.size == 1).map(_.head) match {
      case Some(v) =>
        val (c, m) = unitPropagation(satisfy(clauses, v))
        (c, m + literalToAssignment(v))
      case None => (clauses, Map())
  }

  private def pureLiteral(clauses: ClauseDB): (ClauseDB, Model) = {
    val literals = clauses.foldLeft(Set[Literal]()) { (s1, s2) => s1.union(s2.toSet) }
    val units = literals.filterNot(e => literals.contains(-e))
    (clauses.filterNot(_.exists(units.contains)), units.map(literalToAssignment).toMap)
  }

  // deduction
  private def implications(clauses: ClauseDB, env: Model, graph: IG, dLevel: Int): (ClauseDB, Model, IG) = {

    val precedingLits = (clause: Clause) => clause.slice(0, clause.length-2)

    // if all prec assignments are evaluated to false, then implication assigns the last literal true
    val implied = (clause: Clause) =>
//      !env.contains(clause.last.abs) &&
      precedingLits(clause).foldLeft(true)((x, v) => x && env.contains(v.abs)) &&
      !precedingLits(clause).foldLeft(true)((x, v) => x && env.get(v.abs).get)

    val impliedClauses = clauses.filter(implied)

    var updateGraph: IG = graph
    val learnedClauses: ListBuffer[Clause] = ListBuffer[Clause]()

    impliedClauses match {
      case Nil => (clauses, env, graph)
      case _ => {
        for (clause <- impliedClauses) {
          if (precedingLits(clause).forall(literal => getLiteral(graph, -literal).isDefined)){
            // add the implied node and its edges
            if (!getLiteral(graph, clause.last).isDefined) {
              updateGraph = addNode(graph, ImpliedNode(clause.last, dLevel))
            }
            updateGraph = precedingLits(clause).foldLeft(updateGraph)((x, y) => addEdge(x,
              Edge(getLiteral(graph, y).get, ImpliedNode(clause.last, dLevel))))
            // conflict
            if (getLiteral(graph, -clause.last).isDefined) {
              learnedClauses.append(learnClause(clauses, graph, dLevel, ImpliedNode(clause.last, dLevel)))
            }
          }
        }

//        val learnedClauses: ClauseDB =
//          for {
//            clause <- impliedClauses
//            if (env.contains(clause.last.abs) && !env.get(clause.last.abs).get)
//          } yield learnClause(clauses, graph, dLevel, ImpliedNode(clause.last, dLevel))

        (learnedClauses.toList ++ clauses.filterNot(implied), env ++ impliedClauses.map(_.last)
          .map(v => literalToAssignment(v)).toMap, graph)
      }
    }
  }

  private def dpll(clauses: ClauseDB, decisionLevel: Int, implicationGraph: IG): Option[Model] = {
    val (uClauses, uModel) = unitPropagation(clauses)
    val (pClauses, pModel) = pureLiteral(uClauses)
    val (iClauses, iModel, graph) = implications(pClauses, uModel ++ pModel, implicationGraph, decisionLevel)

    if (iClauses.isEmpty) {
      Some(iModel)
    } else if (iClauses.exists(_.isEmpty)) {
      None
    } else {
      val literal = iClauses.head.head
      val nextDL: Int = decisionLevel + 1
      for (v <- Seq(literal, -literal)) {
        dpll(satisfy(clauses, v), nextDL, addNode(graph, DecisionNode(v, nextDL))) match {
          case Some(model) => return Some(iModel ++ model + literalToAssignment(v))
          case _ =>
        }
      }
      None
    }
  }

  def solve(clauses: ClauseDB): Option[Model] = {
    val implicationGraph: IG = (List[DecisionNode](), List[ImpliedNode](), List[Edge]())

    dpll(clauses, 0, implicationGraph)
  }
}
