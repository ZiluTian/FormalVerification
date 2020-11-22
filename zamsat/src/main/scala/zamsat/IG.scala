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

  // todo: rewrite in more functional style
  private def allPaths(from: Node, to: Node, path: List[Node], paths: ListBuffer[List[Node]]): Unit = {
    val candEdges: List[Edge] = state._3.filter(_.from == from)
    candEdges match {
      case Nil =>
      case _ => {
        val candNodes: List[Node] = candEdges.map(_.to).filter(n => n.level == from.level)
        if (candNodes.contains(to)) {
          paths.append(List.concat(path, List(from, to)))
        } else {
          candNodes.foreach(n => allPaths(n, to, path :+ from, paths))
        }
      }
    }
  }

  def conflictNode(dLevel: Int): Option[Node] = {
    state._2.map(_ match {
      case ImpliedNode(x, l) if (state._2.contains(ImpliedNode(-x, l)) && l==dLevel) => ImpliedNode(x, l)
    }).headOption
  }

  def OneUIP(dLevel: Int): List[Node] = {
    conflictNode(dLevel) match {
      case None => List()
      case Some(x) => {
        val conflict: Node = x
        val dNode: DecisionNode = state._1.filter(n => n.level == dLevel).head
        val paths: ListBuffer[List[Node]] = new ListBuffer[List[Node]]()
        allPaths(dNode, conflict, List(), paths)
        assert(!paths.isEmpty)
        val uips: List[Node] = paths(0).foldLeft(List[Node]())({(x, y) => if (paths.forall(_.contains(y))) x:+y else x})
        uips.last.getChildren(state).foldLeft(Set[Node]())((x, y) => x.union(y.getParents(state).toSet)).toList
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
