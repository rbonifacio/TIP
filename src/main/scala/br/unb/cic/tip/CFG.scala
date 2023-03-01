package br.unb.cic.tip

import br.unb.cic.tip.Stmt.*
import br.unb.cic.tip.Expression.*
import br.unb.cic.tip.Node.*

type Edge = (Node, Node)
type Graph = Set[Edge]

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