package br.unb.cic.tip

import org.typelevel.paiges.Doc
import br.unb.cic.tip.*
import br.unb.cic.tip.Node.SimpleNode

def exportDot(cfg: Graph): String = {
  val prefix = Doc.text("digraph CFG { ")
  val edges = cfg.map { case (from, to) =>
      createNode(from) + Doc.space + Doc.text("->") + Doc.space + createNode(to)
  }
  val body = Doc.intercalate(Doc.text("\n"), edges)
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
