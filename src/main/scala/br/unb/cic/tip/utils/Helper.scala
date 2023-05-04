package br.unb.cic.tip

import br.unb.cic.tip.utils.{Expression, Stmt}
import br.unb.cic.tip.utils.Expression.*
import br.unb.cic.tip.utils.*
import br.unb.cic.tip.utils.Node.SimpleNode

import scala.collection.immutable.Set

def nonTrivialExpressions(stmt: Stmt): Set[Expression] = stmt match
  case SequenceStmt(s1, s2) => nonTrivialExpressions(s1) union nonTrivialExpressions(s2)
  case AssignmentStmt(_, exp) => nonTrivialExpressions(exp)
  case IfElseStmt(condition, _, _) => nonTrivialExpressions(condition)
  case WhileStmt(condition, _) => nonTrivialExpressions(condition)
  case OutputStmt(exp: Expression) => nonTrivialExpressions(exp)
  case _ => Set()

def nonTrivialExpressions(exp: Expression): Set[Expression] = exp match
  case AddExp(left, right) => Set(exp) union nonTrivialExpressions(left) union nonTrivialExpressions(right)
  case SubExp(left, right) => Set(exp) union nonTrivialExpressions(left) union nonTrivialExpressions(right)
  case MultiExp(left, right) => Set(exp) union nonTrivialExpressions(left) union nonTrivialExpressions(right)
  case DivExp(left, right) => Set(exp) union nonTrivialExpressions(left) union nonTrivialExpressions(right)
  case EqExp(left, right) => Set(exp) union nonTrivialExpressions(left) union nonTrivialExpressions(right)
  case GTExp(left, right) => Set(exp) union nonTrivialExpressions(left) union nonTrivialExpressions(right)
  case BracketExp(exp) => nonTrivialExpressions(exp)
  case ConstExp(_) => Set()
  case VariableExp(_) => Set()
  case InputExp => Set()
  case _ => Set()

def expHasVariable(exp: Expression, id: String): Boolean = exp match {
  case VariableExp(name) => name == id
  case AddExp(left, right) => expHasVariable(left, id) || expHasVariable(right, id)
  case SubExp(left, right) => expHasVariable(left, id) || expHasVariable(right, id)
  case MultiExp(left, right) => expHasVariable(left, id) || expHasVariable(right, id)
  case DivExp(left, right) => expHasVariable(left, id) || expHasVariable(right, id)
  case EqExp(left, right) => expHasVariable(left, id) || expHasVariable(right, id)
  case GTExp(left, right) => expHasVariable(left, id) || expHasVariable(right, id)
  case BracketExp(e) => expHasVariable(e, id)
  case ConstExp(_) => false
  case InputExp => false
  case _ => false
}

def callStatement(cfg: Graph): Set[Stmt] = {
  cfg.map(node => callStatement(node)).foldLeft(Set())(_ union _)
}

def callStatement(edge: Edge): Set[Stmt] = {
  callStatement(edge._1) union callStatement(edge._2)
}

def callStatement(node: Node): Set[Stmt] = node match {
  case SimpleNode(stmt) => stmt match {
    case CallStmt(_, _) => Set(stmt)
    case AfterCallStmt(_, _) => Set(stmt)
    case _ => Set()
  }
  case _ => Set()
}