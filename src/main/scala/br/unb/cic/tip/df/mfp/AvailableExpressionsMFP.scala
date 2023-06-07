package br.unb.cic.tip.df.mfp

import br.unb.cic.tip.utils._
import br.unb.cic.tip.utils.Stmt._
import br.unb.cic.tip._

object AvailableExpressionsMFP extends MFP[Expression] {

  def direction = ForwardAnalysis

  def latticeOperator = Meet

  def extremeAnalysisInformation(program: Stmt) = Set()

  def kill(program: Stmt, stmt: Stmt): Set[Expression] = stmt match {
    case AssignmentStmt(id, _) =>
      getLatticeBottom(program).filter(expDependsOn(_, id.asInstanceOf[VariableExp]))
    case _ => Set()
  }

  def gen(stmt: Stmt): Set[Expression] = stmt match {
    case AssignmentStmt(id, exp)  => nonTrivialExps(exp).filterNot(expDependsOn(_, id.asInstanceOf[VariableExp]))
    case WhileStmt(exp, _)        => nonTrivialExps(exp)
    case IfElseStmt(exp, _, _)    => nonTrivialExps(exp)
    case OutputStmt(exp)          => nonTrivialExps(exp)
    case _                        => Set()
  }
}
