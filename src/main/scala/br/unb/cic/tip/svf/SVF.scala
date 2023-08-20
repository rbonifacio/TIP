package br.unb.cic.tip.svf

import br.unb.cic.tip.df.{ReachingDefinition, ResultRD}
import br.unb.cic.tip.pointer.{BasicAndersen, ResultPT}
import br.unb.cic.tip.{blocks, getMethodBody, variables}
import br.unb.cic.tip.utils.{AllocExp, BasicExp, Expression, FunDecl, FunctionCallExp, Id, LoadExp, NameExp, PointerExp, Program, Stmt, VariableExp}
import br.unb.cic.tip.utils.Stmt.*

import scala.collection.mutable

type NodeSVF = (Stmt, Expression)
type EdgeSVF = (NodeSVF, NodeSVF)
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
      case (_: BasicExp, r: FunctionCallExp) => { // a = call fName(b)
        run(getMethodBody(program, r.name), stmt)
      }
      case (l: BasicExp, r: Expression) => ruleCopy(stmt, l, r, caller) // a = b; p = q
      case _ =>
    }
  }

  /**
   * This is a copy operation for variables and pointers.
   * 
   *        Case          |     Rule
   *  - s: v = v1         | - v1@s' -> v@s
   *  - s: v = v1 op v2   | - v1@s' -> v@s and v2@s'\'-> v@s
   *  - s: p = q          | - q@s'-> p@s
   */
  private def ruleCopy(stmt: Stmt, left: BasicExp, right: Expression, caller: Stmt):
    Unit =
        variables(right).foreach(
          v => createGraph(
            (findDefinition(stmt, v, caller), v),
            (stmt, left)
          )
        )


  /**
   * This is a "use" operation so its represented by [u(o)]
   *
   *     Case       |     Rule
   * - s: p = *q    |  - o@s' -> p@s, ∀ o pt(q)
   *
   */
  private def ruleLoad(stmt: Stmt, left: PointerExp, right: LoadExp, caller: Stmt):
    Unit =
      PT(right.pointer).foreach(
        v => createGraph(
          (findDefinition(stmt, v, caller), v),
          (stmt, left)
        )
      )


  /**
   * This is an "use and definition" operation so its represented by [o = x(o1)]
   *
   *     Case     |     Rule
   * - s: *p = q  | - q@s' -> o@s,        ∀ o pt(p) : (strong)
   *              | - pt(o)@s' --> o@s,   ∀ o pt(p) : (weak)
   */
  private def ruleStore(stmt: Stmt, left: LoadExp, right: PointerExp, caller: Stmt):
  Unit =
    PT(left.pointer).foreach(
      v => createGraph(
        (findDefinition(stmt, right, caller),
          right), (stmt, v)
      )
    )


  /**
   * DIRECT                         | INDIRECT
   * Case:                          |
   *                                |
   *  sc: r = f(..., p, ...)        | [U(o)]
   *  sf: f(..., q, ...)            | [o1 = X(_)]
   *                                |
   * Rule:                          |
   *  - p@s' -> q@sf                | - o@s' --> q@sf,  ∀ o pt(p)
   *
   */
  private def ruleCall(stmt: Stmt, caller: Stmt): Unit = {}


  /**
   * DIRECT                           | INDIRECT
   * Case:                            |
   *                                  |
   *  sf:   f(..., q, ...) {          |
   *          ...                     |
   *  sf':    return x                |  [U(o)]
   *        }                         |
   *  sc: r = f(..., p, ...)          |  [o1 = X(_)]
   *                                  |
   * Rule:                            |
   *  - x@sf' -> r@sc                 | - o@sf' --> r@sc ,  ∀ o pt(r)
   *                                  |
   */
  private def ruleReturn(stmt: ReturnStmt, caller: Stmt):
  Unit = caller match {
    case AssignmentStmt(name, _) =>
        createGraph((findDefinition(stmt, stmt.exp, caller), stmt.exp), (stmt, stmt.exp))
        createGraph((stmt, stmt.exp), (caller, name))
  }


  /**
   * generate SVF graph
   */
    private def createGraph(source: NodeSVF, target: NodeSVF): Unit = graph += (source, target)

    /**
   * find the statement were a variable was "defined"
   */
    private def findDefinition(stmt: Stmt, v: BasicExp, context: Stmt = NopStmt): Stmt = {
      val result = RD((stmt, context))._2.find(_.name == v) getOrElse NopStmt
      result match
        case NopStmt => RD((context, NopStmt))._2.find(_.name == v) getOrElse NopStmt
        case _ => result
    }

    private def findDefinition(stmt: Stmt, v: Expression, context: Stmt): Stmt = v.isInstanceOf[BasicExp] match {
      case true => findDefinition(stmt, v.asInstanceOf[BasicExp], context)
      case _ => stmt match
        case ReturnStmt(exp) => findDefinition(stmt, exp, context)
        case _ => NopStmt
    }

    private def findDefinition(stmt: Stmt, v: Expression): Stmt = findDefinition(stmt, v.asInstanceOf[BasicExp], NopStmt)
}
