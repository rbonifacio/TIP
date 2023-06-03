package br.unb.cic.tip.df.mfp

import br.unb.cic.tip.assignments
import br.unb.cic.tip.utils.Stmt
import br.unb.cic.tip.utils.Stmt.AssignmentStmt
import br.unb.cic.tip.utils.Expression.*

object ReachingDefinitionMFP extends MFP[AssignmentStmt] {

  def direction = ForwardAnalysis

  def latticeOperator = Join

  def extremeAnalysisInformation(program: Stmt) = Set()

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
