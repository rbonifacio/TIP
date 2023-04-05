package br.unb.cic.tip

import br.unb.cic.tip.*
import br.unb.cic.tip.Stmt.*
import br.unb.cic.tip.Expression.*
import br.unb.cic.tip.Node.SimpleNode

import scala.collection.mutable

object AvailableExpressions extends MFP[Expression] {
  val direction = ForwardAnalysis
  def latticeOperator = Meet

  def entry(program: Stmt, stmt: Stmt, analysis: Result): Set[Expression] = {
    if (stmt == initStmt(program)) {
      Set()
    } else {
      var res = getLatticeBottom(program)
      for ((from, to) <- cfg if to == SimpleNode(stmt)) {
        from match {
          case SimpleNode(s) => res = analysis(s)._2 & res
          case _             => throw new RuntimeException("Error")
        }
      }
      res
    }
  }

  def exit(program: Stmt, stmt: Stmt, analysis: Result): Set[Expression] = {
    (analysis(stmt)._1 union gen(stmt)) diff kill(program, stmt)
  }

  def kill(program: Stmt, stmt: Stmt): Set[Expression] = stmt match {
    case AssignmentStmt(id, exp) =>
      getLatticeBottom(program).filter(expDependsOn(_, id))
    case _ => Set()
  }

  def gen(stmt: Stmt): Set[Expression] = stmt match {
    case AssignmentStmt(id, exp) =>
      genFromExps(exp).filterNot(expDependsOn(_, id))
    case WhileStmt(exp, _)     => genFromExps(exp)
    case IfElseStmt(exp, _, _) => genFromExps(exp)
    case OutputStmt(exp)       => genFromExps(exp)
    case _                     => Set()
  }

  def genFromExps(exp: Expression): Set[Expression] = exp match {
    case AddExp(l, r)   => Set(exp) | genFromExps(l) | genFromExps(r)
    case SubExp(l, r)   => Set(exp) | genFromExps(l) | genFromExps(r)
    case MultiExp(l, r) => Set(exp) | genFromExps(l) | genFromExps(r)
    case DivExp(l, r)   => Set(exp) | genFromExps(l) | genFromExps(r)
    case EqExp(l, r)    => Set(exp) | genFromExps(l) | genFromExps(r)
    case GTExp(l, r)    => Set(exp) | genFromExps(l) | genFromExps(r)
    case BracketExp(e)  => genFromExps(e)
    case ConstExp(_)    => Set()
    case VariableExp(_) => Set()
    case InputExp       => Set()
    case _              => Set(exp)
  }

  def expDependsOn(exp: Expression, id: String): Boolean = exp match {
    case VariableExp(name) => name == id
    case AddExp(l, r)      => expDependsOn(l, id) || expDependsOn(r, id)
    case SubExp(l, r)      => expDependsOn(l, id) || expDependsOn(r, id)
    case MultiExp(l, r)    => expDependsOn(l, id) || expDependsOn(r, id)
    case DivExp(l, r)      => expDependsOn(l, id) || expDependsOn(r, id)
    case EqExp(l, r)       => expDependsOn(l, id) || expDependsOn(r, id)
    case GTExp(l, r)       => expDependsOn(l, id) || expDependsOn(r, id)
    case BracketExp(e)     => expDependsOn(e, id)
    case ConstExp(_)       => false
    case InputExp          => false
    case _                 => true
  }
}
