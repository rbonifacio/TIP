package br.unb.cic.tip.df

import br.unb.cic.tip.*
import br.unb.cic.tip.utils.{Expression, Stmt}
import br.unb.cic.tip.utils.Expression.*
import br.unb.cic.tip.utils.Node.SimpleNode
import br.unb.cic.tip.utils.Stmt.AssignmentStmt

import scala.collection.mutable

import br.unb.cic.tip.utils.Stmt.given

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

    while (explore) {
      val lastRD = AE.clone()
      for (stmt <- blocks(program)) {
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
          case _             => throw new Error("Match error")
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
}
