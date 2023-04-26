package br.unb.cic.tip.df.mfp

import br.unb.cic.tip.utils._
import br.unb.cic.tip.utils.Stmt._
import br.unb.cic.tip.utils.Expression._
import br.unb.cic.tip._

object LiveVariableMFP extends MFP[VariableExp] {

  def direction = ReverseAnalysis

  def latticeOperator = Join

  def extremeAnalysisInformation(program: Stmt) = Set()

  def kill(program: Stmt, stmt: Stmt): Set[VariableExp] = stmt match {
    case AssignmentStmt(id, _) => Set(VariableExp(id))
    case _                       => Set()
  }

  def gen(stmt: Stmt): Set[VariableExp] = stmt match {
    case AssignmentStmt(_, exp)        => variables(exp)
    case IfElseStmt(condition, _, _) => variables(condition)
    case WhileStmt(condition, _)      => variables(condition)
    case OutputStmt(exp)               => variables(exp)
    case _                             => Set()
  }
}
