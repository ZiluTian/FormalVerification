package zamsat

import zamsat.Solver.Literal

import scala.collection.immutable.List
import scala.collection.mutable.ListBuffer

object IG {

  sealed trait Node {
    def literal: Literal
    def level: Int

    def getParents(g: State): List[Node] = {
      g._3.filter(e => e.to == this).map(_.from)
    }

    def getChildren(g: State): List[Node] = {
      g._3.filter(e => e.from == this).map(_.to)
    }
  }

  case class DecisionNode(literal: Literal, level: Int) extends Node

  case class ImpliedNode(literal: Literal, level: Int) extends Node

  sealed case class Edge(from: Node, to: Node)

  type State = (List[DecisionNode], List[ImpliedNode], List[Edge])
}

class IG(state: IG.State) {

  import IG._

  def add(node: Node): IG = {
    node match {
      case n: DecisionNode => new IG(n:: state._1, state._2, state._3)
      case n: ImpliedNode => new IG(state._1, n:: state._2, state._3)
    }
  }

  def add(edge: Edge): IG = {
    new IG(state._1, state._2, edge :: state._3)
  }

  def allPaths(from: Node, to: Node, path: List[Node] = List(), paths: Set[List[Node]] = Set[List[Node]]()): Set[List[Node]] = {
    val candEdges: List[Edge] = state._3.filter(_.from == from)
    candEdges match {
      case Nil => Set()
      case _ => {
        val candNodes: List[Node] = candEdges.map(_.to).filter(n => n.level == from.level)
        if (candNodes.contains(to)) {
          paths.union(Set(List.concat(path, List(from, to))))
        } else {
          candNodes.foldLeft(paths)((x, y) => x.union(allPaths(y, to, path :+ from, x)))
        }
      }
    }
  }

  def conflictNode(dLevel: Int): Option[Node] = {
    state._2
      .filter(x => x.isInstanceOf[ImpliedNode])
      .filter(x => x.level == dLevel)
      .map(x => Math.abs(x.literal))
      .groupBy(identity).collect {
        case (x, ys) if ys.lengthCompare(1) > 0 => ImpliedNode(x, dLevel) }.headOption
  }

  def UIPS(dLevel: Int): List[Node] = {
    conflictNode(dLevel) match {
      case None => List()
      case Some(x) => {
        val conflict: Node = x
        val dNode: DecisionNode = state._1.filter(n => n.level == dLevel).head
        val paths: Set[List[Node]] = allPaths(dNode, conflict)
        assert(!paths.isEmpty)
        (paths.head.foldLeft(List[Node]())({ (x, y) => if (paths.forall(_.contains(y))) x :+ y else x })).filterNot(n => n == x)
      }
    }
  }

  def OneUIP(dLevel: Int): List[Node] = {
    conflictNode(dLevel) match {
      case None => List()
      case Some(x) => {
        val uips: List[Node] = UIPS(dLevel)
        uips.last.getChildren(state).foldLeft(Set[Node]())((x, y) => x.union(y.getParents(state).toSet)).toList
      }
    }
  }

  def LastUIP(dLevel: Int): List[Node] = {
    conflictNode(dLevel) match {
      case None => List()
      case Some(x) => {
        state._2.filter(n => n.level < dLevel) ++ state._1.filter(n => n.level <= dLevel)
      }
    }
  }

  def getLiteral(l: Literal): Option[Node] = {
    val decisionVar = state._1.filter(_.literal == l).headOption
    if (decisionVar.isDefined){
      decisionVar
    } else {
      state._2.filter(_.literal == l).headOption
    }
  }

  override def toString: String = {
    "Decision nodes: " + state._1.mkString(" ") +
    "Implication nodes: " + state._2.mkString(" ")
//    "Edges: " + state._3.mkString(" ")
  }
}
