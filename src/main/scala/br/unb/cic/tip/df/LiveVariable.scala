package br.unb.cic.tip.df

import br.unb.cic.tip.*
import br.unb.cic.tip.utils.Expression.*
import br.unb.cic.tip.utils.Node.SimpleNode
import br.unb.cic.tip.utils.Stmt
import br.unb.cic.tip.utils.Stmt.*

import scala.collection.mutable

type LV = (Set[VariableExp], Set[VariableExp]) // IN - OUT for each stmt
type ResultLV = mutable.HashMap[Stmt, LV]

object LiveVariable {

  def run(program: Stmt): ResultLV = {
    var explore = true

    val LV: ResultLV = mutable.HashMap()
    var en = Set[VariableExp]()
    var ex = Set[VariableExp]()

    for (stmt <- blocks(program)) {
      LV(stmt) = (en, ex)
    }

    while (explore)
    {
      val lastLV = LV.clone()

      for (stmt <- blocks(program))
      {
        ex = exit(program, stmt, LV)
        en = entry(stmt, LV)
        LV(stmt) = (en, ex)
      }
      explore = lastLV != LV
    }
    LV
  }

  def entry(stmt: Stmt, LV: ResultLV): Set[VariableExp] = stmt match {
    case AssignmentStmt(_, _) => (LV(stmt)._2 diff kill(stmt)) union gen(stmt)
    case _ => LV(stmt)._2 union gen(stmt)
    //to do; maybe there some stmt that does not create
  }

  def exit(program: Stmt, stmt: Stmt, LV: ResultLV): Set[VariableExp] = {
    var res = Set[VariableExp]()
    for ((from, to) <- flowR(program) if to == SimpleNode(stmt)) {
      from match {
        case SimpleNode(s) => res = LV(s)._1 union res
        case _             => throw new Error("Match error")
      }
    }
    res
  }

  def kill(stmt: Stmt): Set[VariableExp] = stmt match {
    case AssignmentStmt(id, _) => Set(VariableExp(id))
    case _ => Set()
  }

  def gen(stmt: Stmt): Set[VariableExp] = stmt match {
    case AssignmentStmt(_, exp) => variables(exp)
    case IfElseStmt(condition, _, _) => variables(condition)
    case WhileStmt(condition, _) => variables(condition)
    case OutputStmt(exp) => variables(exp)
    case _ => Set()
  }
}