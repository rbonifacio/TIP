package br.unb.cic.tip.df.mfp

import br.unb.cic.tip.utils._
import br.unb.cic.tip.utils.Stmt._
import br.unb.cic.tip._

object MfpAvailableExpressions extends MFP[Expression] {
  val direction = ForwardAnalysis
  def latticeOperator = Meet

  def extremeAnalysisInformation(program: Stmt) = Set()

  def kill(program: Stmt, stmt: Stmt): Set[Expression] = stmt match {
    case AssignmentStmt(id, exp) =>
      getLatticeBottom(program).filter(expDependsOn(_, id))
    case _ => Set()
  }

  def gen(stmt: Stmt): Set[Expression] = stmt match {
    case AssignmentStmt(id, exp) =>
      nonTrivialExps(exp).filterNot(expDependsOn(_, id))
    case WhileStmt(exp, _)     => nonTrivialExps(exp)
    case IfElseStmt(exp, _, _) => nonTrivialExps(exp)
    case OutputStmt(exp)       => nonTrivialExps(exp)
    case _                     => Set()
  }
}
