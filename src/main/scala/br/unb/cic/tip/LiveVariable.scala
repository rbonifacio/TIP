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

    val LV: Result = mutable.HashMap()
    var en = Set[VariableExp]()
    var ex = Set[VariableExp]()

    val cfg = flowR(program)

    for (stmt <- blocks(program)) {
      LV(stmt) = (en, ex)
    }

    while (explore) {
      val lastLV = LV.clone()
      for (stmt <- blocks(program))
        {
          ex = exit(program, stmt, LV)
          en = entry(program, stmt, LV)
          LV(stmt) = (en, ex)
        }
        explore = lastLV != LV
    }
    LV
  }

  def entry(program: Stmt, stmt: Stmt, LV: Result): Set[VariableExp] =
    stmt match {
      case AssignmentStmt(id, exp) =>
        (LV(stmt)._2 diff kill(stmt)) union gen(stmt)
      case _ => LV(stmt)._2 union gen(stmt)
    }

  def exit(program: Stmt, stmt: Stmt, LV: Result): Set[VariableExp] = {
    var res = Set[VariableExp]()
    for ((from, to) <- flowR(program) if to == SimpleNode(stmt)) {
      from match {
        case SimpleNode(s) => res = LV(s)._1 union res
        case _             => throw new Error("a")
      }
    }
    res
  }

  def kill(stmt: Stmt): Set[VariableExp] = stmt match {
    case AssignmentStmt(id, exp) => Set(VariableExp(id))
    case _                       => Set()
  }

  def gen(stmt: Stmt): Set[VariableExp] = stmt match {
    case AssignmentStmt(id, exp)       => variables(exp)
    case IfElseStmt(condition, s1, s2) => variables(condition)
    case WhileStmt(condition, s1)      => variables(condition)
    case OutputStmt(exp)               => variables(exp)
    case _                             => Set()
  }
}
