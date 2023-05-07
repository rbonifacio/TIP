package br.unb.cic.tip

import br.unb.cic.tip.*
import br.unb.cic.tip.utils.{Id, Node, Expression, Stmt}
import br.unb.cic.tip.utils.Node.*
import br.unb.cic.tip.utils._

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


//def validPath(path: Path, stack: List[Stmt]): Boolean = path match
////  case List() if stack.isEmpty => true // base case + succeeded to recognize the path
////  case List() if ! stack.isEmpty => false // base case + failed to recognize the path
////  case _ => { // the recursive case
////
////  }
//{
//  case List() => stack.isEmpty
//  case _ => path.head match {
//    case SimpleNode(stmt) => stmt match
//      case CallStmt(_, _) => stmt :: stack
//      case _ => false
//  }
//}
def findValidPath(path: Path): Boolean =
  validPath(gatherCallerAndCallee(path)) 
  // && validPath2(path)

def validPath(callers: List[Stmt]): Boolean = callers.isEmpty match
  case true => true
  case _ => callers.head match
    case CallStmt(stmt) => callers.tail.contains(AfterCallStmt(stmt)) match
      case true => validPath(callers.tail.filter( _ != AfterCallStmt(stmt)))
      case _ => false


//def validPath2(path: Path) : Boolean = path match {
//  case Nil => true
//  case _ :: Nil => true
//  case x :: y :: xs => x match
//    case SimpleNode(stmt) => stmt.isInstanceOf[CallStmt] match
//      case true => y match
//        case SimpleNode(stmt) => stmt.isInstanceOf[AfterCallStmt] match
//          case true => true
//          case _ => validPath2(y :: xs)
//      case _ => validPath2(y :: xs)
//      case _ => validPath2 (y :: xs)
//    case _ => validPath2 (y :: xs)
////    if (x.isInstanceOf[CallStmt]) false else validPath2 (y :: xs)
//}


def gatherCallerAndCallee(path: Path): List[Stmt] = path.isEmpty match
  case true => List()
  case false => path.head match
    case SimpleNode(stmt) => stmt match
      case CallStmt(_) => stmt :: gatherCallerAndCallee(path.tail)
      case AfterCallStmt(_) => stmt :: gatherCallerAndCallee(path.tail)
      case _ => gatherCallerAndCallee(path.tail)
    case _ => gatherCallerAndCallee(path.tail)