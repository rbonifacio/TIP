package br.unb.cic.tip.pointer

import br.unb.cic.tip.df.ReachingDefinition.RD
import br.unb.cic.tip.{blocks, nonTrivialExpressions, variables}
import br.unb.cic.tip.utils.{AllocExp, BasicExp, Expression, LoadExp, LocationExp, PointerExp, Stmt, VariableExp}
import br.unb.cic.tip.utils.Stmt.AssignmentStmt

import scala.collection.mutable

type Cell = Expression
type ResultPT = mutable.HashMap[BasicExp, Set[Cell]]

object BasicAndersen {

  private var pt: ResultPT = mutable.HashMap()

  private var disableAllocationRule: Boolean = false

  def pointTo(body: Stmt): ResultPT = {
    pt = mutable.HashMap()
    pointTo(body: Stmt, false)
  }
  def pointTo(body: Stmt, disableAllocation: Boolean): ResultPT = {

    disableAllocationRule = disableAllocation
    var explore = true
    for (variable <- variables(body)) {
      pt(variable) = Set()
    }

    while (explore) {
      val lastPT = pt.clone()

      for (stmt <- blocks(body)) {
          gen(stmt)
      }
      explore = lastPT != pt
    }
    pt
  }

  def gen(stmt: Stmt): Unit = stmt match {
    case AssignmentStmt(left, right) => (left, right) match {
      case (l: PointerExp, r: AllocExp) => disableAllocationRule match {
        case false => ruleAllocation(l, r) // alloc: p = alloc i
        case true =>
      }
      case (l: PointerExp, r: LocationExp) => ruleLocation (l, r) // location: p1 = &q
      case (l: PointerExp, r: PointerExp) => ruleCopy(l, r) // assign: p = q
      case (l: PointerExp, r: LoadExp) => ruleLoad(l, r) // load: p = *q
      case (l: LoadExp, r: PointerExp) => ruleStore(l, r) // store: *p = q
      case (l: PointerExp, r: Expression) => ruleDeferred(l, r) // deferred: p = null
      case (_: VariableExp, _: Expression) =>  // any other thing
    }
    case _ =>
  }

  /**
   * Case: p = alloc i
   * Rule: alloc-i ∈ pt(p)
   */
  private def ruleAllocation(left: PointerExp, right: AllocExp): Unit = pt(left) = pt(left) + right


  /**
   * Case: p1 = &q
   * Rule: q ∈ pt(p)
   */
  private def ruleLocation(left: PointerExp, right: LocationExp): Unit = pt(left) = pt(left) + right.pointer


  /**
   * Case: p = q
   * Rule: pt(q) ⊆ pt(p)
   */
  private def ruleCopy(left: PointerExp, right: PointerExp): Unit = pt(left) = pt(left) union pt(right)


  /**
   * Case: p = *q
   * Rule: c ∈ pt(q) =⇒ pt(c) ⊆ pt(p) for each c ∈ Cells
   */
  private def ruleLoad(left: PointerExp, right: LoadExp): Unit = {
      for (v <- pt(right.pointer) if v.isInstanceOf[BasicExp] )
        pt(left) = pt(left) union pt(v.asInstanceOf[BasicExp] )
  }


  /**
   * Case: *p = q
   * Rule: c ∈ pt(p) =⇒ pt(q) ⊆ pt(c) for each c ∈ Cells
   */
  private def ruleStore(left: LoadExp, right: PointerExp): Unit = {
      for (v <- pt(left.pointer) if v.isInstanceOf[BasicExp])
        pt(v.asInstanceOf[BasicExp]) = pt(v.asInstanceOf[BasicExp]) union pt(variables(right).head)
  }

  /**
   * Case: p = null
   * Rule: pt(p) = ()
   */
  private def ruleDeferred(left: PointerExp, right: Expression): Unit = pt(left) = Set()
}

//template for rules
//  /**
//   * Case:
//   * Rule:
//   */
//  def rule?(left: VariableExp, right: ?): Unit = {
//
//  }
