package br.unb.cic.tip.svf

import br.unb.cic.tip.df.{ReachingDefinition, ResultRD}
import br.unb.cic.tip.pointer.{BasicAndersen, ResultPT}
import br.unb.cic.tip.{blocks, getMethodBody, variables}
import br.unb.cic.tip.utils.{AllocExp, BasicExp, Expression, FunDecl, FunctionCallExp, Id, LoadExp, NameExp, PointerExp, Program, Stmt, VariableExp}
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
  private var program: Program = List()

  def run(p: Program): GraphSVF = {
    program = p
    val bodyMain = getMethodBody(program)
    graph = Set()
    RD = ReachingDefinition.run(bodyMain, program)
    PT = BasicAndersen.pointTo(bodyMain, true)
    run(bodyMain, NopStmt)
  }

  private def run(body: Stmt, caller: Stmt): GraphSVF = {

   for (stmt <- blocks(body)) {
     analyzer(stmt, caller)
   }
    graph
  }

  private def analyzer(stmt: Stmt, caller: Stmt): Unit = stmt match {
    case AssignmentStmt(left, right) => analyzer(stmt, left, right, caller: Stmt)
    case ReturnStmt(_) => ruleReturn(stmt.asInstanceOf[ReturnStmt], caller) // return x
    case _ =>
  }

  private def analyzer(stmt: Stmt, left: BasicExp, right: Expression, caller: Stmt): Unit = {
    (left, right) match {
      case (l: PointerExp, r: LoadExp) =>  ruleLoad(stmt, l, r, caller) // l: p = *q
      case (l: LoadExp, r: PointerExp) =>  ruleStore(stmt, l, r, caller) // l: *p = q
//      case (l: VariableExp, r: FunctionCallExp) =>  ruleCall(stmt, r)
      case (l: BasicExp, r: FunctionCallExp) => { // a = call fName(b)
        run(getMethodBody(program, "fSign"), stmt)
      }
      case (l: BasicExp, r: Expression) => ruleCopy(stmt, l, r, caller) // a = b; p = q
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
  private def ruleCopy(stmt: Stmt, left: BasicExp, right: Expression, caller: Stmt): Unit = {
    variables(right).foreach(v => createGraph((findDefinition(stmt, v, caller), v), (stmt, left)))
  }

  /**
   * This is a "use" operation so its represented by [u(o)]
   *
   * Case: l: p = *q
   * Rule: ∀ o pt(q)
   *  - ∀ o@Ln -> p@L
   */
  private def ruleLoad(stmt: Stmt, left: PointerExp, right: LoadExp, caller: Stmt): Unit = {
    PT(right.pointer).foreach(v => createGraph((findDefinition(stmt, v, caller), v), (stmt, left)))
  }


  /**
   * This is an "use and definition" operation so its represented by [o = x(o1)]
   * Case: L: *p = q
   * Rule: ∀ o pt(p)
   *  - q@L1 -> ∀ o@L (strong)
   *  - o1@L1 --> ∀ o@L (weak)
   */
  private def ruleStore(stmt: Stmt, left: LoadExp, right: PointerExp, caller: Stmt): Unit = {
    PT(left.pointer).foreach(v => createGraph((findDefinition(stmt, right, caller), right), (stmt, v)))
  }


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
  private def ruleCall(stmt: Stmt, caller: Stmt): Unit = {
//    caller.args.map(p => println(findDefinition(stmt, p)))
  }


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
  private def ruleReturn(stmt: ReturnStmt, caller: Stmt): Unit = caller match {
    case AssignmentStmt(name, _) =>
        createGraph((findDefinition(stmt, stmt.exp, caller), stmt.exp), (stmt, stmt.exp))
        createGraph((stmt, stmt.exp), (caller, name))
    case _ =>
  }


  /**
   * generate SVF graph
   */
    private def createGraph(source: NodeSVF, target: NodeSVF): Unit = graph += (source, target)

    /**
   * find the statement were a variable was "defined"
   */
    private def findDefinition(stmt: Stmt, v: BasicExp, context: Stmt = NopStmt): Stmt = RD((stmt, context))._2.find(_.name == v) getOrElse NopStmt

    private def findDefinition(stmt: Stmt, v: Expression, context: Stmt): Stmt = v.isInstanceOf[BasicExp] match {
      case true => findDefinition(stmt, v.asInstanceOf[BasicExp], context)
      case _ => stmt match
        case ReturnStmt(exp) => findDefinition(stmt, exp, context)
        case _ => NopStmt
    }

    private def findDefinition(stmt: Stmt, v: Expression): Stmt = findDefinition(stmt, v.asInstanceOf[BasicExp], NopStmt)
}
