package br.unb.cic.tip

import br.unb.cic.tip.Stmt.*
import br.unb.cic.tip.Expression.*
import br.unb.cic.tip.Node.*
import br.unb.cic.tip.predecessor.visited

type Edge = (Node, Node)
type Graph = Set[Edge]
type Assignment = (Id, Expression)
type RD = (Node, Set[Assignment])

def initStmt(stmt: Stmt): Stmt = stmt match {
  case SequenceStmt(s1, s2) => s1
  case _ => stmt
}

def finalStmt(stmt: Stmt): Set[Stmt] = stmt match {
  case SequenceStmt(s1, s2) => finalStmt(s2)
  case IfElseStmt(condition, s1, s2) => finalStmt(s1) `union` (if (s2.isDefined) finalStmt(s2.get) else Set[Stmt]())
  case _ => Set(stmt)
}

def blocks(stmt: Stmt): Set[Stmt] = stmt match {
  case SequenceStmt(s1, s2) => blocks(s1) union blocks(s2)
  case IfElseStmt(condition, s1, Some(s2)) => Set(stmt) union blocks(s1) union blocks(s2)
  case IfElseStmt(condition, s1, None) => Set(stmt) union blocks(s1)
  case WhileStmt(condition, s1) => Set(stmt) union blocks(s1)
  case _ => Set(stmt)
}

def flow(stmt: Stmt): Graph = stmt match {
  case SequenceStmt(s1, s2) => flow(s1) union flow(s2) union finalStmt(s1).map(s => (SimpleNode(s),SimpleNode(initStmt(s2))))
  case IfElseStmt(condition, s1, Some(s2)) => flow(s1) union flow(s2) union Set((SimpleNode(stmt), SimpleNode(initStmt(s1)))) union Set((SimpleNode(stmt), SimpleNode(initStmt(s2))))
  case IfElseStmt(condition, s1, None) => flow(s1) union Set((SimpleNode(stmt), SimpleNode(initStmt(s1))))
  case WhileStmt(condition, s1) => flow(s1) union Set((SimpleNode(stmt), SimpleNode(initStmt(s1)))) union finalStmt(s1).map(s => (SimpleNode(s),SimpleNode(stmt)))
  case _ => Set()
}

def flow(f: FunDecl): Graph =
  Set((StartNode, SimpleNode(initStmt(f.body)))) union flow(f.body) union finalStmt(f.body).map(s => (SimpleNode(s), EndNode))

def statements(node: Node): Set[Assignment] = node match {
  case SimpleNode(stmt) => statements(stmt)
  case _ => Set()
}

def statements(stmts: Set[Stmt]): Set[Assignment] = {
  var stmtList = Set[Assignment]()
  stmts.foreach(stmt => {
    stmtList = stmtList union statements(stmt)
  })
  stmtList
}

def statements(stmt: Stmt): Set[Assignment] = stmt match {
  case AssignmentStmt(l, r) => Set((l, r))
  case _ => Set()
}

def convertGraph2Stmts(graph: Graph): Set[Stmt] = {
  var stmtList = Set[Stmt]()
  graph.map(g => {
    stmtList = stmtList union convertNode2Stmt(g._1) union convertNode2Stmt(g._2)
  })
  stmtList
}

def convertNode2Stmt(node: Node): Set[Stmt] = node match {
  case SimpleNode(stmt) => Set(stmt)
  case _ => Set()
}

object predecessor {

  var visited = Set[Node]()

  def run(graph: Graph, node: Node ):  Graph = {
    visited = Set[Node]()
    predecessors(graph, node)
  }
  def run(graph: Graph, stmt: Stmt ):  Graph = {
    visited = Set[Node]()
    predecessors(graph, SimpleNode(stmt))
  }
  def predecessors(graph: Graph, node: Node ):  Graph = {

    if (visited.contains(node)) {
      return Set()
    }

    visited = visited union Set(node)
    var myG = Set[Edge]()
    val myPre = graph.filter({ case (s, t) => t == node && s != StartNode })
    myPre.foreach({ case (s, _) => myG = myG union predecessors(graph, s)})
    myPre union myG
  }
}

def ReachingDefinitions(graph: Graph): Set[RD] = {
    var visited = Set[Node]()
    var rd = Set[RD]()
    graph.map(g => {
      if (! visited.contains(g._1)) {
        rd = rd union Set((g._1, ReachingDefinitions(graph, g._1)))
        visited = visited union Set(g._1)
      }
      if (! visited.contains(g._2)) {
        rd = rd union Set((g._2, ReachingDefinitions(graph, g._2)))
        visited = visited union Set(g._2)
      }
    })
    rd
}

def ReachingDefinitions(graph: Graph, node: Node): Set[Assignment] = node match {
  case SimpleNode(stmt) => ReachingDefinitions(graph, stmt)
  case _ => Set()
}

def ReachingDefinitions(graph: Graph, stmt: Stmt): Set[Assignment] = stmt match {
  case AssignmentStmt(_, _) => {
    statements(convertGraph2Stmts(predecessor.run(graph, stmt))).filter({ case (l, r) => l != getIdForAssignment(stmt)}) union statements(stmt)
  }
  case _ => statements(convertGraph2Stmts(predecessor.run(graph, stmt)))
}

def getIdForAssignment(stmt: Stmt): Id = stmt match {
  case AssignmentStmt(l, r) => l
  case _ => null
}


