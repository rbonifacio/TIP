package br.unb.cic.tip.df.mfp

import br.unb.cic.tip.utils._
import br.unb.cic.tip.utils.Stmt._
import br.unb.cic.tip.utils.{Expression}
import br.unb.cic.tip._

object LiveVariableMFP extends MFP[BasicExp] {

  def direction = ReverseAnalysis

  def latticeOperator = Join

  def extremeAnalysisInformation(program: Stmt) = Set()

  def kill(program: Stmt, stmt: Stmt): Set[BasicExp] = stmt match {
    case AssignmentStmt(id, _) => Set(id)
    case _                       => Set()
  }

  def gen(stmt: Stmt): Set[BasicExp] = stmt match {
    case AssignmentStmt(_, exp)        => variables(exp)
    case IfElseStmt(condition, _, _) => variables(condition)
    case WhileStmt(condition, _)      => variables(condition)
    case OutputStmt(exp)               => variables(exp)
    case _                             => Set()
  }
}
