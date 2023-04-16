package br.unb.cic.tip.df

import br.unb.cic.tip.*
import br.unb.cic.tip.Expression.*
import br.unb.cic.tip.Node.SimpleNode
import br.unb.cic.tip.Stmt.*

import scala.collection.mutable

type AE = (Set[Expression], Set[Expression])
type ResultAE = mutable.HashMap[Stmt, AE]

object AvailableExpressions {

  def run(program: Stmt): ResultAE = {

    var explore = true
    val AE: ResultAE = mutable.HashMap()
    val allExpressions = nonTrivialExpressions(program)
    var en = allExpressions
    var ex = Set[Expression]()

    for (stmt <- blocks(program)) {
      AE(stmt) = (en, ex)
    }

    while (explore)
    {
      val lastRD = AE.clone()
      for (stmt <- blocks(program))
      {
        en = entry(program, stmt, AE, allExpressions)
        ex = exit(stmt, AE, allExpressions)
        AE(stmt) = (en, ex)
      }
      explore = lastRD != AE
    }
    AE
  }

  def entry(program: Stmt, stmt: Stmt, AE: ResultAE, allExpressions: Set[Expression]): Set[Expression] = {
    if (stmt == initStmt(program)) {
      Set()
    } else {
      var res = allExpressions
      for ((from, to) <- flow(program) if to == SimpleNode(stmt)) {
        from match {
          case SimpleNode(s) => res = AE(s)._2 & res
        }
      }
      res
    }
  }

  def exit(stmt: Stmt, AE: ResultAE, allExpressions: Set[Expression]): Set[Expression] = {
    (AE(stmt)._1 union gen(stmt, allExpressions)) diff kill(stmt, allExpressions)
  }


  def kill(stmt: Stmt, allExpressions: Set[Expression]): Set[Expression] = stmt match {
    case AssignmentStmt(id, _) => allExpressions.filter(exp => expHasVariable(exp, id))
    case _ => Set()
  }

  def gen(stmt: Stmt, allExpressions: Set[Expression]): Set[Expression] = nonTrivialExpressions(stmt)

  def nonTrivialExpressions(stmt: Stmt): Set[Expression] = stmt match
    case SequenceStmt(s1, s2)        => nonTrivialExpressions(s1) union nonTrivialExpressions(s2)
    case AssignmentStmt(_ , exp)     => nonTrivialExpressions(exp)
    case IfElseStmt(condition, _, _) => nonTrivialExpressions(condition)
    case WhileStmt(condition, _)     => nonTrivialExpressions(condition)
    case SequenceStmt(s1, s2)        => nonTrivialExpressions(s1) union nonTrivialExpressions(s2)
    case OutputStmt(exp: Expression) => nonTrivialExpressions(exp)
    case _                           => Set()

  def nonTrivialExpressions(exp: Expression): Set[Expression] = exp match
    case AddExp(left, right)    => Set(exp) union nonTrivialExpressions(left) union nonTrivialExpressions(right)
    case SubExp(left, right)    => Set(exp) union nonTrivialExpressions(left) union nonTrivialExpressions(right)
    case MultiExp(left, right)  => Set(exp) union nonTrivialExpressions(left) union nonTrivialExpressions(right)
    case DivExp(left, right)    => Set(exp) union nonTrivialExpressions(left) union nonTrivialExpressions(right)
    case EqExp(left, right)     => Set(exp) union nonTrivialExpressions(left) union nonTrivialExpressions(right)
    case GTExp(left, right)     => Set(exp) union nonTrivialExpressions(left) union nonTrivialExpressions(right)
    case BracketExp(exp)        => nonTrivialExpressions(exp)
    case ConstExp(_)            => Set()
    case VariableExp(_)         => Set()
    case InputExp               => Set()
    case _                      => Set()

  def expHasVariable(exp: Expression, id: String): Boolean = exp match {
    case VariableExp(name)     => name == id
    case AddExp(left, right)   => expHasVariable(left, id) || expHasVariable(right, id)
    case SubExp(left, right)   => expHasVariable(left, id) || expHasVariable(right, id)
    case MultiExp(left, right) => expHasVariable(left, id) || expHasVariable(right, id)
    case DivExp(left, right)   => expHasVariable(left, id) || expHasVariable(right, id)
    case EqExp(left, right)    => expHasVariable(left, id) || expHasVariable(right, id)
    case GTExp(left, right)    => expHasVariable(left, id) || expHasVariable(right, id)
    case BracketExp(e)         => expHasVariable(e, id)
    case ConstExp(_)           => false
    case InputExp              => false
    case _                     => false
  }
}
