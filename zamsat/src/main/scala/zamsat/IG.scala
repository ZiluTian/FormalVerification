package zamsat

import zamsat.Solver.Literal

import scala.collection.immutable.List
import scala.collection.mutable.ListBuffer

object IG {

  sealed trait Node {
    def literal: Literal
    def level: Int
  }

  case class DecisionNode(literal: Literal, level: Int) extends Node

  case class ImpliedNode(literal: Literal, level: Int) extends Node

  sealed case class Edge(from: Node, to: Node)

}

class IG() {

  import IG._

  var decisionNodess: ListBuffer[DecisionNode] = new ListBuffer[DecisionNode]()
  var impliedNodess: ListBuffer[ImpliedNode] = new ListBuffer[ImpliedNode]()
  var edgess: ListBuffer[Edge] = new ListBuffer[Edge]()

  def init(dN: List[DecisionNode], iN: List[ImpliedNode], es: List[Edge]): Unit = {
    decisionNodess.appendAll(dN)
    impliedNodess.appendAll(iN)
    edgess.appendAll(es)
  }

  def add(node: Node): Unit = {
    node match {
      case n: DecisionNode => decisionNodess.append(n)
      case n: ImpliedNode => impliedNodess.append(n)
    }
  }

  def add(edge: Edge): Unit = {
    edgess.append(edge)
  }

  def getDecisionNodess(): List[DecisionNode] = {
    decisionNodess.toList
  }

  def getImpliedNodess(): List[ImpliedNode] = {
    impliedNodess.toList
  }

  def getEdgess(): List[Edge] = {
    edgess.toList
  }

  def getParents(n: Node): Set[Node] = {
    edgess.filter(e => e.to == n).map(_.from).toSet
  }

  def getChildren(n: Node): Set[Node] = {
    edgess.filter(e => e.from == n).map(_.to).toSet
  }

  def allPaths(from: Node, to: Node, path: List[Node] = List(), paths: Set[List[Node]] = Set[List[Node]]()): Set[List[Node]] = {
    val candEdges: List[Edge] = edgess.filter(_.from == from).toList
    candEdges match {
      case Nil => Set()
      case _ =>
        val candNodes: List[Node] = candEdges.map(_.to).filter(n => n.level == from.level)
        if (candNodes.contains(to)) {
          paths.union(Set(List.concat(path, List(from, to))))
        } else {
          candNodes.foldLeft(paths)((x, y) => x.union(allPaths(y, to, path :+ from, x)))
        }
    }
  }

  def conflictNode(dLevel: Int): Option[Node] = {
    impliedNodess
      .filter(x => x.isInstanceOf[ImpliedNode] && x.level == dLevel)
      .map(x => Math.abs(x.literal))
      .groupBy(identity)
      .collectFirst{
        case (x, ys) if ys.lengthCompare(1) > 0 => ImpliedNode(x, dLevel)
      }
  }

  def UIPS(dLevel: Int): List[Node] = {
    conflictNode(dLevel) match {
      case None => List()
      case Some(x) =>
        val conflict: Node = x
        val dNode: DecisionNode = decisionNodess.filter(n => n.level == dLevel).head
        val paths: Set[List[Node]] = allPaths(dNode, conflict)
        assert(paths.nonEmpty)
        paths.head.foldLeft(List[Node]()){ (x, y) => if (paths.forall(_.contains(y))) x :+ y else x }.filterNot(n => n == x)
    }
  }

  // A cut splits the graph into reason side and conflict side, regardless of dLevel
  // divide the IG right before the given uip
  // return the conflict side. Reason side is complementary
  def cut(uip: Node): Set[Node] = {
    var cNodes: List[Node] = List()
    var frontier: List[Node] = getChildren(uip).toList

    while (frontier.nonEmpty) {
      val root: Node = frontier.head
      frontier = frontier.splitAt(1)._2
      if (!cNodes.contains(root)) {
        cNodes = root :: cNodes
        frontier ++= getChildren(root)
      }
    }

    cNodes.toSet
  }

  // conflictClause returns all nodes belonging to reason side that have edges leading to conflicting side
  // the clause learned from the conflict
  def conflictClause(conflictSide: Set[Node]): List[Literal] = {
    conflictSide.flatMap(c => getParents(c).diff(conflictSide)).map(n => -n.literal).toList
  }

  // update the IG when removing the conflict nodes
  def removeConflictNodes(uip: Node): Unit = {
    val conflictNodes: Set[Node] = cut(uip)

    impliedNodess = impliedNodess.filterNot(n => conflictNodes.contains(n))
    decisionNodess = decisionNodess.filterNot(n => conflictNodes.contains(n))
    edgess = edgess.filterNot(n => conflictNodes.contains(n.from) || conflictNodes.contains(n.to))
  }

  // Can extend to arbitrary learning scheme by varying the UIP selection
  def learn(uip: Node): List[Literal] = {
    conflictClause(cut(uip))
  }

  def OneUIP(dLevel: Int): List[Literal] = {
    learn(UIPS(dLevel).last)
  }

  def getLiteral(l: Literal): Option[Node] = {
    val decisionVar = decisionNodess.find(_.literal == l)
    if (decisionVar.isDefined){
      decisionVar
    } else {
      impliedNodess.find(_.literal == l)
    }
  }

  override def toString: String = {
    "Decision nodes: " + decisionNodess.mkString(" ") +
    "Implication nodes: " + impliedNodess.mkString(" ")
//    "Edges: " + edgess.mkString(" ")
  }
}
