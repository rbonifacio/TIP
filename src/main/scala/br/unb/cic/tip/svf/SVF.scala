package br.unb.cic.tip.svf

import br.unb.cic.tip.df.{ReachingDefinition, ResultRD}
import br.unb.cic.tip.pointer.{BasicAndersen, ResultPT}
import br.unb.cic.tip.{blocks, getMethodBody, variables}
import br.unb.cic.tip.utils.{BasicExp, Expression, FunDecl, FunctionCallExp, Id, LoadExp, PointerExp, Program, Stmt, VariableExp}
import br.unb.cic.tip.utils.Stmt.*

import scala.collection.mutable

type NodeSVF = (Stmt, Expression)
type EdgeSVF = (NodeSVF, NodeSVF)
//type EdgeSVF = (Expression, Expression)
type GraphSVF = Set[EdgeSVF]

object SVF {

  private var graph: GraphSVF = Set()
  private var RD: ResultRD = mutable.HashMap()
  private var PT: ResultPT = mutable.HashMap()

  def run(program: Program): GraphSVF = {
    val bodyMain = getMethodBody(program)
    graph = Set()
    RD = ReachingDefinition.run(bodyMain, program)
    PT = BasicAndersen.pointTo(bodyMain)
    run(bodyMain)
  }

  private def run(body: Stmt): GraphSVF = {

    for (stmt <- blocks(body)) {
      analyzer(stmt)
    }
    graph
  }

  private def analyzer(stmt: Stmt): Unit = stmt match {
    case AssignmentStmt(left, right) => analyzer(stmt, left, right)
    case _ =>
  }

  private def analyzer(stmt: Stmt, left: BasicExp, right: Expression): Unit = {
    (left, right) match {
      case (l: PointerExp, r: LoadExp) =>  ruleLoad(stmt, l, r) // l: p = *q
      case (l: BasicExp, r: Expression) => ruleCopy(stmt, l, r) // a = b; p = q
      case _ =>
    }
  }

  /**
   * This is a copy operation for variables and pointers.
   * 
   * Case:
   *  - s: v = v1
   *  - s: v = v1 op v2
   *  - s: p = q
   * Rule:
   *  - v1@s1 -> v@s
   *  - v2@s2 -> v@s
   *  - q@s'-> p@s
   */
  private def ruleCopy(stmt: Stmt, left: BasicExp, right: Expression): Unit = {
    variables(right).foreach(v => createGraph((findDefinition(stmt, v), v), (stmt, left)))
  }

  /**
   * This is a "use" operation so its represented by [u(o)]
   *
   * Case: l: p = *q
   * Rule: ∀ o pt(q)
   *  - ∀ o@Ln -> p@L
   */
  private def ruleLoad(stmt: Stmt, left: PointerExp, right: LoadExp): Unit = {
    PT(right.pointer).foreach(v => createGraph((findDefinition(stmt, v), v), (stmt, left)))
  }


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
//  private def ruleReturn(stmt: ReturnStmt, caller: FunctionCallExp): Unit = {}

  /**
   * generate graph only for basic expressions
   */
    private def createGraph(source: NodeSVF, target: NodeSVF): Unit = source._2.isInstanceOf[BasicExp] match {
      case true => graph += (source, target)
      case _ =>
    }

    /**
   * find the statement were a variable was "defined"
   */
    private def findDefinition(stmt: Stmt, v: BasicExp): Stmt = RD((stmt, NopStmt))._2.find(_.name == v) getOrElse NopStmt

    private def findDefinition(stmt: Stmt, v: Expression): Stmt = v.isInstanceOf[BasicExp] match {
      case true => findDefinition(stmt, v.asInstanceOf[BasicExp])
      case _ => NopStmt
    }
}
