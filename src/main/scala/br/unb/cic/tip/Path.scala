package br.unb.cic.tip

import br.unb.cic.tip.*
import br.unb.cic.tip.Stmt.*
import br.unb.cic.tip.Expression.*
import br.unb.cic.tip.Node.*

type Path = List[Node]

def path(flow: Graph, function: Id, limit: Int = 2): Set[Path] = {
  path(StartNode(function), EndNode(function), flow, List(), limit)
}

def path(from: Node, target: Node, flow: Graph, visited: List[Node], limit: Int): Set[Path] = {
  var res: Set[Path] = if(from == target) Set(List(from)) else Set()

  val newVisited = from :: visited

  for((n, t) <- flow if (n == from) && (newVisited.filter(p => p == t).size < limit)) {
    res = res ++ path(t, target, flow, newVisited, limit).map(path => from :: path)
  }
  res
}