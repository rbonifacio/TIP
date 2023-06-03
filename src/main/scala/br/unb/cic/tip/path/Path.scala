package br.unb.cic.tip

import br.unb.cic.tip.*
import br.unb.cic.tip.utils.{Id, Node, Expression, Stmt}
import br.unb.cic.tip.utils.Node.*
import br.unb.cic.tip.utils.Stmt.*

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



def isValidPath(path: Path): Boolean =
  areCallersBalanced(gatherCallerAndCallee(path)) && AreCallersEdgesNotDirected(path)

def areCallersBalanced(callers: List[Stmt]): Boolean = callers.isEmpty match
  case true => true
  case _ => callers.head match
    case CallStmt(stmt) => callers.tail.contains(AfterCallStmt(stmt)) match
      case true => areCallersBalanced(callers.tail.filter( _ != AfterCallStmt(stmt)))
      case _ => false

def AreCallersEdgesNotDirected(path: Path) : Boolean = path match {
  case Nil => true
  case _ :: Nil => true
  case x :: y :: xs => if (isCallStmt(x) && isCallStmt(y)) false else AreCallersEdgesNotDirected (y :: xs)
}



def gatherCallerAndCallee(path: Path): List[Stmt] = path.isEmpty match
  case true => List()
  case false => path.head match
    case SimpleNode(stmt) => stmt match
      case CallStmt(_) => stmt :: gatherCallerAndCallee(path.tail)
      case AfterCallStmt(_) => stmt :: gatherCallerAndCallee(path.tail)
      case _ => gatherCallerAndCallee(path.tail)
    case _ => gatherCallerAndCallee(path.tail)