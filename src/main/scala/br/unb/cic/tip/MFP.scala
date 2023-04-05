package br.unb.cic.tip

import scala.collection.mutable

sealed trait AnalysisDirection
object ForwardAnalysis extends AnalysisDirection
object ReverseAnalysis extends AnalysisDirection

sealed trait LatticeOperator
object Meet extends LatticeOperator
object Join extends LatticeOperator

abstract class MFP[T] {
  def direction: AnalysisDirection
  def latticeOperator: LatticeOperator

  type EntryExitSets = (Set[T], Set[T])
  type Result = mutable.HashMap[Stmt, EntryExitSets]

  def transferFunction(program: Stmt, stmt: Stmt, analysis: Result): Set[T] =
    (analysis(stmt)._1 diff kill(program, stmt)) union gen(stmt)

  def kill(program: Stmt, stmt: Stmt): Set[T]
  def gen(stmt: Stmt): Set[T]

  def entry(program: Stmt, stmt: Stmt, analysis: Result): Set[T]
  def exit(program: Stmt, stmt: Stmt, analysis: Result): Set[T]

  var cfg: Graph = Set()

  def directionedFlow(program: Stmt) = direction match {
    case ForwardAnalysis => flow(program)
    case ReverseAnalysis => flowR(program)
  }

  def getLatticeBottom(program: Stmt): Set[T] = latticeOperator match {
    case Meet => blocks(program).foldLeft(Set[T]())(_ ++ gen(_))
    case Join => Set[T]()
  }

  def run(program: Stmt): Result = {
    val analysis: Result = mutable.HashMap()

    cfg = directionedFlow(program)

    val latticeBottom = getLatticeBottom(program)

    for (stmt <- blocks(program)) {
      analysis(stmt) = (latticeBottom, latticeBottom)
    }

    var explore = true
    while (explore) {
      val lastRD = analysis.clone()

      for (stmt <- blocks(program)) {

        val entryExpressions = entry(program, stmt, analysis)
        val exitExpressions = transferFunction(program, stmt, analysis)
        analysis(stmt) = (entryExpressions, exitExpressions)
      }
      explore = lastRD != analysis
    }

    analysis
  }
}
