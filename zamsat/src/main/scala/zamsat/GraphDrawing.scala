package zamsat

import java.io.File

import guru.nidi.graphviz.attribute.{Color, RankDir}
import guru.nidi.graphviz.engine.{Format, Graphviz}
import guru.nidi.graphviz.model.{Factory, Graph}

object GraphDrawing {
  def drawGraph(graph: Set[IG.Edge], name: String = ""): Unit = {
    var g: Graph = Factory
      .graph("ImplicationGraph")
      .directed()
      .graphAttr()
      .`with`(RankDir.LEFT_TO_RIGHT)

    graph
      .groupBy(_.from)
      .foreach(y => {
        var nStart = Factory.node(y._1.toString())
        y._2.foreach(x => {
          nStart = nStart.link(
            Factory
              .to(Factory.node(x.to.toString()))
              .`with`(Color.BLACK))
          })
        g = g.`with`(nStart)
      })

    val gviz = Graphviz.fromGraph(g)

    gviz.render(Format.PNG).toFile(new File("debug/graph_" + name + ".png"))
  }
}
