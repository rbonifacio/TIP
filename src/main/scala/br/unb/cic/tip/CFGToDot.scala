package br.unb.cic.tip

import org.typelevel.paiges.Doc
import br.unb.cic.tip.*

def exportDot(cfg: Graph): String = {
  val prefix = Doc.text("digraph CFG { ")
  val edges = cfg.map { case (from, to) =>
    Doc.text(from.toString) + Doc.space + Doc.text("->") + Doc.space + Doc.text(to.toString)
  }
  val body = Doc.intercalate(Doc.text("\n"), edges)
  val suffix = Doc.text("}")
  val res = body.tightBracketBy(prefix, suffix)
  res.render(20)
}
