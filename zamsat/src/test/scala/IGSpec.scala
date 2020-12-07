import org.scalatest.flatspec.AnyFlatSpec
import zamsat.IG
import zamsat.IG._

import scala.collection.immutable.List

class IGSpec extends AnyFlatSpec {

  // Build the graph as in https://www.cs.princeton.edu/courses/archive/fall13/cos402/readings/SAT_learning_clauses.pdf on page 8
  val n1 = DecisionNode(-1, 4)

  val n2 = ImpliedNode(2, 4)
  val n3 = ImpliedNode(3, 4)
  val n4 = ImpliedNode(4, 4)
  val n5 = ImpliedNode(5, 4)
  val n6 = ImpliedNode(6, 4)
  val n7 = ImpliedNode(-7, 1)
  val n8 = ImpliedNode(-8, 2)
  val n9 = ImpliedNode(-9, 3)
  val n10 = ImpliedNode(-5, 4)

  val e1 = Edge(n1, n2)
  val e2 = Edge(n1, n3)
  val e3 = Edge(n7, n3)
  val e4 = Edge(n2, n4)
  val e5 = Edge(n3, n4)
  val e6 = Edge(n8, n5)
  val e7 = Edge(n4, n5)
  val e8 = Edge(n1, n3)
  val e9 = Edge(n4, n6)
  val e10 = Edge(n9, n6)
  val e11 = Edge(n6, n10)
  //    val e12 = Edge(n5, n10)

  var sampleGraph: IG = new IG()
  sampleGraph.init(List(n1), List(n2, n3, n4, n5, n6, n7, n8, n9, n10), List(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11))

  "Conflict node" should "detect node 5" in {
    assert(sampleGraph.conflictNode(4).contains(n5))
  }

  "All paths" should "identify all the paths between two nodes" in {

    val path1: Set[List[Node]] = sampleGraph.allPaths(n1, n10)
    val path2: Set[List[Node]] = sampleGraph.allPaths(n1, n5)

    assert(path1.size == 2)
    assert(path2.size == 2)
  }

  "UIPS" should "return n1 and n4" in {
    val uips: List[Node] = sampleGraph.UIPS(4)
    assert(uips == List(n1, n4))
  }

  "Cut oneUIP" should "return conflict side n5, n10, n6 and the rest reason side"
  "OneUIP" should "return List(n8, n4, n9)" in {
    assert(sampleGraph.cut(n4) == Set(n5, n6, n10))
    assert(sampleGraph.learn(n4) == Set(n8, n4, n9))
  }

  "LastUIP" should "return List(n8, n1, n7, n9)" in {
    assert(sampleGraph.cut(n1) == Set(n2, n3, n4, n5, n6, n10))
    assert(sampleGraph.learn(n1) == Set(n7, n8, n9, n1))
  }
}