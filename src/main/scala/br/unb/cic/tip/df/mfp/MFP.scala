package br.unb.cic.tip.df.mfp

import scala.collection.mutable
import br.unb.cic.tip.utils._
import br.unb.cic.tip._
import br.unb.cic.tip.utils.Node.SimpleNode

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

  def analysis1(program: Stmt, stmt: Stmt, analysis: Result) =
    if (extremeStmts(program) contains stmt) {
      extremeAnalysisInformation(program)
    } else {
      cfg
        .filter((_, to) => to == SimpleNode(stmt))
        .foldLeft(getLatticeBottom(program))((set, edge) =>
          edge._1 match {
            case SimpleNode(s) => join(analysis(s)._2, set)
            case _             => throw new RuntimeException()
          }
        )
    }

  var cfg: Graph = Set()

  def directionedFlow(program: Stmt) = direction match {
    case ForwardAnalysis => flow(program)
    case ReverseAnalysis => flowR(program)
  }

  def extremeStmts(program: Stmt) = direction match {
    case ForwardAnalysis => Set(initStmt(program))
    case ReverseAnalysis => finalStmt(program)
  }

  def extremeAnalysisInformation(program: Stmt): Set[T]

  def getLatticeBottom(program: Stmt): Set[T] = latticeOperator match {
    case Meet => blocks(program).foldLeft(Set[T]())(_ ++ gen(_))
    case Join => Set[T]()
  }

  def join(s1: Set[T], s2: Set[T]) = latticeOperator match {
    case Meet => s1 intersect s2
    case Join => s1 union s2
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
      val lastAnalysis = analysis.clone()

      for (stmt <- blocks(program)) {
        val analysis1Result = analysis1(program, stmt, analysis)
        val analysis2Result = transferFunction(program, stmt, analysis)
        analysis(stmt) = (analysis1Result, analysis2Result)
      }

      explore = lastAnalysis != analysis
    }

    // from PPA [66]:
    // forward  => analysis 1 = entry; analysis 2 = exit
    // backward => analysis 1 = exit; analysis 2 = entry
    //  this is not *mandatory*, but fixes the tests for LV as they expect
    //  analysis 2 to be exit

    direction match {
      case ForwardAnalysis => analysis
      case ReverseAnalysis =>
        analysis.map((stmt, result) => (stmt, (result._2, result._1)))
    }
  }
}
