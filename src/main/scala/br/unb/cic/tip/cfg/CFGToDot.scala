package br.unb.cic.tip

import org.typelevel.paiges.Doc
import br.unb.cic.tip.*
import br.unb.cic.tip.utils.Node.{SimpleNode, StartNode, EndNode}
import br.unb.cic.tip.utils.{AfterCallStmt, CallStmt, Node}

def exportDot(cfg: Graph, path: Path = List()): String = {

  val edges = cfg.map { case (from, to) =>
    createNode(from) + Doc.space + Doc.text("->") + Doc.space + createNode(to) + (if (isCallStmt(from) && isCallStmt(to)) Doc.space + Doc.text("[color=\"gray\"]") else Doc.text(""))
  }
  var body = Doc.intercalate(Doc.text("\n"), edges)

  // add path if it was sent
  if (! path.isEmpty) {
    body = createPath(path) + body
  }

  // add color for call nodes
  val callNodes = callStatement(cfg).map( stmt => createNode(stmt.toString, "skyblue"))
  // add color for limit nodes (start and end)
  val limitNodes = functions(cfg).map( stmt => createNode(StartNode(stmt).toString, "yellow") + Doc.space + createNode(EndNode(stmt).toString, "yellow"))

  // add nodes that will have some styles at the end of the body
  body = body + Doc.text("\n") + Doc.intercalate(Doc.text("\n"), limitNodes union callNodes)

  // add prefix and sufix
  val prefix = Doc.text("digraph CFG { ")
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

def createNode(body: String, color: String): Doc = {
  Doc.text("\"") + Doc.text(body) + Doc.text("\"") + Doc.space + Doc.text(s"[style=filled, fillcolor=$color]")
}

def isCallStmt(v: Node): Boolean = v match {
  case SimpleNode(stmt) => stmt match
    case CallStmt(_, _) => true
    case AfterCallStmt(_, _) => true
    case _ => false
  case _ => false
}

def createPath(path: Path):  Doc = {
  val prefix = Doc.text("subgraph cluster_0 { ")
//  val colorNode = Doc.text(  "node[color = red];")
  val colorBackground = Doc.text(  "color = white")
  val colorEdges = Doc.text(  "[color = green]")

  val e = path.map { p => Doc.space + createNode(p) + Doc.space}
  val edges = Doc.intercalate(Doc.text("->"), e)
  val suffix = Doc.text(" }")

  prefix + edges + colorEdges + Doc.space + colorBackground + suffix
//  prefix + edges + Doc.space + Doc.text("[color=\"green\"]") + suffix
}

//"CallStmt(c,sum)" [style=filled, fillcolor=red]
