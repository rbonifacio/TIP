package br.unb.cic.tip.svf


import br.unb.cic.tip.{blocks, getMethodBody}
import br.unb.cic.tip.utils.{Expression, FunDecl, Program, ReturnStmt, Stmt}
import br.unb.cic.tip.utils.Expression.{FunctionCallExp, LoadExp, PointerExp, VariableExp}

import scala.collection.mutable

type NodeSVF = (Stmt, Expression)
//type EdgeSVF = (NodeSVF, NodeSVF)
type EdgeSVF = (Expression, Expression)
type GraphSVF = Set[EdgeSVF]

object SVF {

  private var graph: GraphSVF = Set()

  def run(program: Program): GraphSVF = {
    run(getMethodBody(program))
  }

  private def run(body: Stmt): GraphSVF = {
    for (stmt <- blocks(body)) {
      analyzer(stmt)
    }
    graph
  }

  private def analyzer(stmt: Stmt): Unit = stmt match {
    case _ => Set()
  }

  /**
   * Case: l: p = q
   * Rule: q@l1 -> p@l
  */
  private def ruleCopy(left: PointerExp, right: PointerExp): Unit = {}

  /**
   * Case: l: v3 = phi(v1, v2)
   * Rule:
   *  - v1@l1-> v3@l
   *  - v2@l2-> v3@l
   */
  private def rulePhi(left: VariableExp, right: Stmt): Unit = {}


  /**
   * This is a "use" operation so its represented by [u(o)]
   *
   * Case: l: p = *q
   * Rule: ∀ o pt(q)
   *  - o@ln -> p@l
   */
  private def ruleLoad(left: PointerExp, right: LoadExp): Unit = {}


  /**
   * This is an "use and definition" operation so its represented by [o = x(o1)]
   * Case: l: *p = q
   * Rule: ∀ o pt(p)
   *  - q@l1 -> o@l (strong)
   *  - o1@l1 --> o@l (weak)
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


}
