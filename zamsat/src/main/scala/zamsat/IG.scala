package zamsat

import zamsat.Solver.Literal

import scala.collection.mutable.ListBuffer

object IG {

  sealed trait Node {
    def literal: Literal
    def level: Int
  }

  case class DecisionNode(literal: Literal, level: Int) extends Node

  case class ImpliedNode(literal: Literal, level: Int) extends Node

  sealed case class Edge(from: Node, to: Node)

  val learnedClauses: ListBuffer[List[Int]] = new ListBuffer[List[Int]]()
}

class IG() {

  import IG._

  var decisionNodess: Set[DecisionNode] = Set[DecisionNode]()
  var impliedNodess: Set[ImpliedNode] = Set[ImpliedNode]()
  var edgess: Set[Edge] = Set[Edge]()

  def init(dN: List[DecisionNode], iN: List[ImpliedNode], es: List[Edge]): Unit = {
    decisionNodess = decisionNodess.union(dN.toSet)
    impliedNodess = impliedNodess.union(iN.toSet)
    edgess = edgess.union(es.toSet)
  }

  def add(node: Node): Unit = {
    node match {
      case n: DecisionNode => decisionNodess = decisionNodess.union(Set(n))
      case n: ImpliedNode => impliedNodess = impliedNodess.union(Set(n))
    }
  }

  def add(edge: Edge): Unit = {
    edgess = edgess.union(Set(edge))
  }

  def getDecisionNode(dLevel: Int): Option[DecisionNode] = {
    decisionNodess.filter(n => n.level == dLevel).headOption
  }

  def getImpliedNodes(dLevel: Int): Set[ImpliedNode] = {
    impliedNodess.filter(n => n.level == dLevel)
  }

  def getEdgess(): Set[Edge] = {
    edgess
  }

  def getParents(n: Node): Set[Node] = {
    edgess.filter(e => e.to == n).map(_.from)
  }

  def getChildren(n: Node): Set[Node] = {
    edgess.filter(e => e.from == n).map(_.to)
  }

  def allPaths(from: Node, to: Node, path: List[Node] = List(), paths: Set[List[Node]] = Set[List[Node]]()): Set[List[Node]] = {
    val candEdges: List[Edge] = edgess.filter(_.from == from).toList
    candEdges match {
      case Nil =>
        Set()
      case _ =>
        val candNodes: List[Node] = candEdges.map(_.to)
        if (candNodes.contains(to)) {
          paths.union(Set(List.concat(path, List(from, to))))
        } else {
          candNodes.foldLeft(paths)((x, y) => x.union(allPaths(y, to, path :+ from, x)))
        }
    }
  }

  def getAllDecisionParents(node: Node): Set[DecisionNode] = {

    var cNodes: List[Node] = List()
    var frontier: List[Node] = getParents(node).toList

    while (frontier.nonEmpty) {
      val root: Node = frontier.head
      frontier = frontier.splitAt(1)._2
      if (!cNodes.contains(root)) {
        cNodes = root :: cNodes
        frontier ++= getParents(root)
      }
    }

    cNodes.toSet
      .filter(n => n.isInstanceOf[DecisionNode])
      .asInstanceOf[Set[DecisionNode]]
  }

  def UIPS(cNode1: Node, cNode2: Node, dNode: DecisionNode): List[Node] = {

    val paths1: Set[List[Node]] = allPaths(dNode, cNode1)
    val paths2: Set[List[Node]] = allPaths(dNode, cNode2)
    val paths: Set[List[Node]] = paths1 ++ paths2
//    paths.foreach(println)
    if (paths.nonEmpty && paths1.nonEmpty && paths2.nonEmpty) {
        paths.head.foldLeft(List[Node]()){ (x, y) => if (paths.forall(_.contains(y))) x :+ y else x }
    } else {
      List()
    }
  }


  // A cut splits the graph into reason side and conflict side
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
    val learned: List[Literal] = conflictSide.flatMap(c => getParents(c).diff(conflictSide)).map(n => -n.literal).toList
//    println(" learned scheme " + learned)
    // for debugging, to test the learned clauses are satisfied
//    IG.learnedClauses.append(learned)
    learned
  }

  def removeNode(node: Node): Unit = {
    impliedNodess = impliedNodess.filterNot(n => n == node)
    decisionNodess = decisionNodess.filterNot(n => n == node)
    edgess = edgess.filterNot(n => (n.from == node || n.to==node))
  }

  def removeLiteral(l: Literal): Unit = {
    val removeL: Option[Node] = getLiteral(l)
    if (removeL.isDefined) {
      removeNode(removeL.get)
    }
  }

  def getLiteral(l: Literal): Option[Node] = {
    val decisionVar = decisionNodess.find(_.literal == l)
    val impliedVar = impliedNodess.find(_.literal == l)
    assert(!decisionVar.isDefined || !impliedVar.isDefined)
    if (decisionVar.isDefined) {
      decisionVar
    } else {
      impliedVar
    }
  }

  override def toString: String = {
    "Decision nodes: " + decisionNodess.mkString(" ") +
      "Implication nodes: " + impliedNodess.mkString(" ")
    "Edges: " + edgess.mkString(" ")
  }
}
