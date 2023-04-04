package br.unb.cic.tip

import br.unb.cic.tip.*
import br.unb.cic.tip.Stmt.*
import br.unb.cic.tip.Expression.*
import br.unb.cic.tip.Node.*

type Path = List[Node]

def path(cfg: Graph, functionName: Id, limit: Int = 2): Set[Path] = {
  path(StartNode(functionName), EndNode(functionName), cfg, List(), limit)
}

def path(from: Node, to: Node, cfg: Graph, visited: List[Node], limit: Int): Set[Path] = {
  var res: Set[Path] = if (from == to) Set(List(from)) else Set()

  val newVisited = from :: visited

  for((n, t) <- cfg if (n == from) && (newVisited.filter(p => p == t).size < limit)) {
    res = res ++ path(t, to, cfg, newVisited, limit).map(path => from :: path)
  }
  res
}