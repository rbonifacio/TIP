package br.unb.cic.tip.pointer

import br.unb.cic.tip.df.ReachingDefinition.RD
import br.unb.cic.tip.{blocks, nonTrivialExpressions, variables}
import br.unb.cic.tip.utils.{AllocExp, BasicExp, Expression, LocationExp, PointerExp, Stmt, VariableExp}
import br.unb.cic.tip.utils.Stmt.AssignmentStmt

import scala.collection.mutable

type Cell = Expression
type Result = mutable.HashMap[BasicExp, Set[Cell]]

object BasicAndersen {

  private var pt: Result = mutable.HashMap()

  def pointTo(body: Stmt): Result = {

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
      case (l: PointerExp, r: AllocExp) => ruleAllocation(l, r) // alloc: x = alloc i
      case (l: PointerExp, r: LocationExp) => ruleLocation (l, r) // location: x1 = &x2
      case (l: PointerExp, r: PointerExp) => ruleCopy(l, r) // assign: x1 = x2

//      case (l: LoadExp, _) => ruleStore(l, right) // store: *x1 = x2
//      case (l: VariableExp, r: LoadExp) => ruleLoad(l, r) // load: x1 = *x2
//      case (l: VariableExp, r: NullExp) => ruleDeferred(l, r) // deferred: X = null
//      case (l: VariableExp, _) => pt(l) = pt(l) + right // any other thing
      case (_: VariableExp, _: Expression) =>  // assign: x1 = x2
    }
  }

  /**
   * Case: x = alloc i
   * Rule: alloc-i ∈ pt(x)
   */
  private def ruleAllocation(left: PointerExp, right: AllocExp): Unit = pt(left) = pt(left) + right

  /**
   * Case: x1 = &x2
   * Rule: x2 ∈ pt(x1)
   */
  def ruleLocation(left: PointerExp, right: LocationExp): Unit = pt(left) = pt(left) + PointerExp(right.pointer)

  /**
   * Case: x1 = x2
   * Rule: pt(x2) ⊆ pt(x1)
   */
  def ruleCopy(left: PointerExp, right: PointerExp): Unit =  {
    pt(left) = pt(left) union pt(right)
  }


    /**
     * Case: x1 = *x2
     * Rule: c ∈ pt(x2) =⇒ pt(c) ⊆ pt(x1) for each c ∈ Cells
     */
//    def ruleLoad(left: VariableExp, right: LoadExp): Unit = right.exp match {
//        case PointerExp (name) => {
//          for (v <- pt(VariableExp (name) ) if v.isInstanceOf[VariableExp] )
//            pt(left) = pt(left) union pt(v.asInstanceOf[VariableExp] )
//          }
//    }

    /**
     * Case: *x1 = x2
     * Rule: c ∈ pt(x1) =⇒ pt(x2) ⊆ pt(c) for each c ∈ Cells
     */
//    def ruleStore(left: LoadExp, right: Expression): Unit = left.exp match {
//      case PointerExp(name) => {
//        for (v <- pt(VariableExp(name)) if v.isInstanceOf[VariableExp])
//          pt(v.asInstanceOf[VariableExp]) = pt(v.asInstanceOf[VariableExp]) union pt(variables(right).head)
//      }
//    }

    /**
     * Case: x1 = null
     * Rule: pt(x1) = ()
     */
//    def ruleDeferred(left: VariableExp, right: NullExp): Unit = pt(left) = Set()
}
//template for rules
//  /**
//   * Case:
//   * Rule:
//   */
//  def rule?(left: VariableExp, right: ?): Unit = {
//
//  }
