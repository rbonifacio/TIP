package br.unb.cic.tip

import br.unb.cic.tip.*
import br.unb.cic.tip.Stmt.*
import br.unb.cic.tip.Expression.*
import br.unb.cic.tip.Node.SimpleNode

import scala.collection.mutable

object ReachingDefinition extends MFP[AssignmentStmt] {

  val direction = ForwardAnalysis
  def latticeOperator = Join

  def entry(program: Stmt, stmt: Stmt, RD: Result): Set[AssignmentStmt] = {
    var res = Set[AssignmentStmt]()
    for ((from, to) <- flow(program) if to == SimpleNode(stmt)) {
      from match {
        case SimpleNode(s) => res = RD(s)._2 union res
        case _             => throw new RuntimeException()
      }
    }
    res
  }

  def exit(program: Stmt, stmt: Stmt, RD: Result): Set[AssignmentStmt] =
    stmt match {
      case AssignmentStmt(id, exp) =>
        (RD(stmt)._1 diff kill(program, stmt)) union gen(stmt)
      case _ => RD(stmt)._1
    }

  def kill(program: Stmt, stmt: Stmt): Set[AssignmentStmt] =
    stmt match {
      case AssignmentStmt(id, _) =>
        assignments(program).filter(_.name == id)
      case _ => Set()
    }

  def gen(stmt: Stmt): Set[AssignmentStmt] = stmt match {
    case AssignmentStmt(id, exp) => Set(AssignmentStmt(id, exp))
    case _                       => Set()
  }
}
