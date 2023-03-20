package br.unb.cic.tip

import br.unb.cic.tip.*
import br.unb.cic.tip.Stmt.*
import br.unb.cic.tip.Expression.*
import br.unb.cic.tip.Node.SimpleNode

import scala.collection.mutable

type LV = (Set[VariableExp], Set[VariableExp])
type Result = mutable.HashMap[Stmt, LV]

object LiveVariable {

  def run(program: Stmt): Result = {
    var explore = true

    val RD: Result = mutable.HashMap()
    var en = Set[VariableExp]()
    var ex = Set[VariableExp]()

    val cfg = flowR(program)

    for (stmt <- blocks(program)) {
      RD(stmt) = (en, ex)
    }

    while (explore)
    {
      val lastRD = RD.clone()
      for (stmt <- blocks(program))
      {
        ex = exit(program, stmt, RD)
        en = entry(program, stmt, RD)
        RD(stmt) = (en, ex)
      }
      explore = lastRD != RD
    }
    RD
  }

  def entry(program: Stmt, stmt: Stmt, RD: Result): Set[VariableExp] = stmt match {
    case AssignmentStmt(id, exp) => (RD(stmt)._2 diff kill(stmt)) union gen(stmt)
    case _ => RD(stmt)._2 union gen(stmt)
  }

  def exit(program: Stmt, stmt: Stmt, RD: Result): Set[VariableExp] = {
    var res = Set[VariableExp]()
    for ((from, to) <- flowR(program) if to == SimpleNode(stmt)) {
      from match {
        case SimpleNode(s) => res = RD(s)._1 union res
      }
    }
    res
  }

  def kill(stmt: Stmt): Set[VariableExp] = stmt match {
    case AssignmentStmt(id, exp) => Set(VariableExp(id))
    case _ => Set()
  }

  def gen(stmt: Stmt): Set[VariableExp] = stmt match {
    case AssignmentStmt(id, exp) => variables(exp)
    case IfElseStmt(condition, s1, s2) => variables(condition)
    case WhileStmt(condition, s1) => variables(condition)
    case OutputStmt(exp) => variables(exp)
    case _ => Set()
  }
}