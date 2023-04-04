package br.unb.cic.tip

import br.unb.cic.tip.*
import br.unb.cic.tip.Stmt.*
import br.unb.cic.tip.Expression.*
import br.unb.cic.tip.Node.*

type Path = List[Node]

/**
 * Generate a list of Paths using the CFG from the all program and the name of
 * a function that already exists and takes on count an specific "deep".
 * It means the times a path can be re-visited
 */
def path(cfg: Graph, functionName: Id, limit: Int = 2): Set[Path] = {
  path(StartNode(functionName), EndNode(functionName), cfg, List(), limit)
}

/**
 * Generate a list of Paths from an specific source node to a target node exploring the CFG and
 * taking on count an specific "deep". It means the times a path can be re-visited
 */
def path(from: Node, to: Node, cfg: Graph, visited: List[Node], limit: Int): Set[Path] = {
  var res: Set[Path] = if (from == to) Set(List(from)) else Set()

  val newVisited = from :: visited

  for((n, t) <- cfg if (n == from) && (newVisited.filter(p => p == t).size < limit)) {
    res = res ++ path(t, to, cfg, newVisited, limit).map(path => from :: path)
  }
  res
}