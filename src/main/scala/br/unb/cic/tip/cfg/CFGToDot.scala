package br.unb.cic.tip

import org.typelevel.paiges.Doc
import br.unb.cic.tip.*
import br.unb.cic.tip.utils.Node.SimpleNode
import br.unb.cic.tip.utils.Node

def exportDot(cfg: Graph, path: Path = List()): String = {
  val prefix = Doc.text("digraph CFG { ")

  val edges = cfg.map { case (from, to) =>
      createNode(from) + Doc.space + Doc.text("->") + Doc.space + createNode(to)
  }
  var body = Doc.intercalate(Doc.text("\n"), edges)

  //work with path
  if (! path.isEmpty) {
    body = createPath(path) + body
  }

  val suffix = Doc.text("}")
  val res = body.tightBracketBy(prefix, suffix)
  res.render(20)
}

def createNode(v: Node): Doc = {
  Doc.text("\"") + (v match {
    case SimpleNode(s) => Doc.text(s.toString)
    case _ => Doc.text(v.toString)
  }) + Doc.text("\"")
}

def createPath(path: Path):  Doc = {
  val prefix = Doc.text("subgraph cluster_0 { ")
  val colorNode = Doc.text(  "node[color = red];")
  val colorBackground = Doc.text(  "color = white")
  val e = path.map { p => Doc.space + createNode(p) + Doc.space}
  val edges = Doc.intercalate(Doc.text(""), e)
  val suffix = Doc.text("}")

  prefix + colorNode + edges + colorBackground + suffix

}
