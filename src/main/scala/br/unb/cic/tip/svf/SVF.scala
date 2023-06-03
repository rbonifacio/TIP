package br.unb.cic.tip.svf


import br.unb.cic.tip.df.{ReachingDefinition, ResultRD}
import br.unb.cic.tip.{blocks, getMethodBody, variables}
import br.unb.cic.tip.utils.{AssignmentPointerStmt, AssignmentStmt, Expression, FunDecl, NopStmt, Program, ReturnStmt, Stmt}
import br.unb.cic.tip.utils.Expression.{FunctionCallExp, LoadExp, PointerExp, VariableExp}

import scala.collection.mutable

type NodeSVF = (Stmt, Expression)
type EdgeSVF = (NodeSVF, NodeSVF)
//type EdgeSVF = (Expression, Expression)
type GraphSVF = Set[EdgeSVF]

object SVF {

  private var graph: GraphSVF = Set()
  private var RD: ResultRD = mutable.HashMap()

  def run(program: Program): GraphSVF = {
    val bodyMain = getMethodBody(program)
    graph = Set()
    RD = ReachingDefinition.run(bodyMain, program)
    run(bodyMain)
  }

  private def run(body: Stmt): GraphSVF = {

    for (stmt <- blocks(body)) {
      analyzer(stmt)
    }
    graph
  }

  private def analyzer(stmt: Stmt): Unit = stmt match {
    case AssignmentStmt(left, right) => ruleCopyVar(stmt, VariableExp(left), right)
    case AssignmentPointerStmt(left, right) => pointersOperations(stmt, left, right)
    case _ => Set()
  }

  private def pointersOperations(stmt: Stmt, left: Expression, right: Expression): Unit = {
    (left, right) match {
      case (l: PointerExp, r: PointerExp) => ruleCopy(stmt, l, r) // l: p = q
      case (l: PointerExp, r: LoadExp) =>  // l: p = *q
      case (l: LoadExp, r: PointerExp) =>  // l: *p = q
//      case call rule
//      case return rule
      case _ =>
    }
  }

  /**
   * Case:
   *  - s: v = v1
   *  - s: v = v1 op v2
   * Rule:
   *  - v1@s1 -> v@s
   *  - v2@s2 -> v@s
   */
  private def ruleCopyVar(stmt: Stmt, left: VariableExp, right: Expression): Unit = {
    variables(right).foreach(v => graph += ((findDefinition(stmt, v), v), (stmt, left)))
  }


  /**
   * Case: s: p = q
   * Rule: q@s'-> p@s
  */
  private def ruleCopy(stmt: Stmt, left: PointerExp, right: PointerExp): Unit = {
    graph += ((findDefinition(stmt, VariableExp(right.name)), right), (stmt, left))
  }

  /**
   * This is a "use" operation so its represented by [u(o)]
   *
   * Case: l: p = *q
   * Rule: ∀ o pt(q)
   *  - ∀ o@Ln -> p@L
   */
  private def ruleLoad(left: PointerExp, right: LoadExp): Unit = {}


  /**
   * This is an "use and definition" operation so its represented by [o = x(o1)]
   * Case: L: *p = q
   * Rule: ∀ o pt(p)
   *  - q@L1 -> ∀ o@L (strong)
   *  - o1@L1 --> ∀ o@L (weak)
   */
  private def ruleStore(left: LoadExp, right: PointerExp): Unit = {}


  /**
   * DIRECT                         | INDIRECT
   * Case:                          |
   *                                |
   *  Lcs: r = f(..., p, ...)       | [U(o)]
   *  Lf: f(..., q, ...)            | [o1 = X(_)]
   *                                |
   * Rule:                          |
   *  - p@L1 -> q@Lf                | - o@L1 --> o1@Lf
   *
   */
  private def ruleCall(caller: FunctionCallExp, callee: FunDecl): Unit = {}


  /**
   * DIRECT                         | INDIRECT
   * Case:                          |
   *                                |
   *  Lf: f(..., q, ...) {          |
   *    ...                         |
   *    return x                    |  [U(o)]
   *  }                             |
   *  Lcs: r = f(..., p, ...)       |  [o1 = X(_)]
   *                                |
   * Rule:                          |
   *  - x@Lf -> r@Lcs               | - o@Lf --> o1@Lcs
   *                                |
   */
  private def ruleReturn(stmt: ReturnStmt, caller: FunctionCallExp): Unit = {}


  /**
   * find the statement were a variable was "defined"
   */
  private def findDefinition(stmt: Stmt, v: VariableExp): Stmt = RD((stmt, NopStmt))._2.find(_.name == v.name) getOrElse NopStmt

}
