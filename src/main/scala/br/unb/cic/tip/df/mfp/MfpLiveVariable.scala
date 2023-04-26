package br.unb.cic.tip.df.mfp

import br.unb.cic.tip.utils._
import br.unb.cic.tip.utils.Stmt._
import br.unb.cic.tip.utils.Expression._
import br.unb.cic.tip._

object MfpLiveVariable extends MFP[VariableExp] {

  val direction = ReverseAnalysis

  def latticeOperator = Join

  def extremeAnalysisInformation(program: Stmt) = Set()

  def kill(program: Stmt, stmt: Stmt): Set[VariableExp] = stmt match {
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
