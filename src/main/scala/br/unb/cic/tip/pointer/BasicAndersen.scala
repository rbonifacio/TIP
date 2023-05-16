package br.unb.cic.tip.pointer

import br.unb.cic.tip.df.ReachingDefinition.RD
import br.unb.cic.tip.{blocks, nonTrivialExpressions, variables}
import br.unb.cic.tip.utils.Expression.{NullExp, VariableExp, LoadExp, PointerExp, AllocExp, LocationExp}
import br.unb.cic.tip.utils.{AssignmentPointerStmt, AssignmentStmt, Expression, Stmt}

import scala.collection.mutable

type Cell = Expression
type Result = mutable.HashMap[VariableExp, Set[Cell]]

object BasicAndersen {

  var pt: Result = mutable.HashMap()

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
    case AssignmentPointerStmt(left, right) => (left, right) match {
      case (l: LoadExp, _) => ruleStore(l, right) // store: *x1 = x2
      case (l: VariableExp, r: AllocExp) => ruleAllocation(l, r) // alloc: x = alloc i
      case (l: VariableExp, r: LocationExp) => ruleLocation (l, r) // location: x1 = &x2
      case (l: VariableExp, r: PointerExp) => ruleCopy(l, r) // assign: x1 = x2
      case (l: VariableExp, r: LoadExp) => ruleLoad(l, r) // load: x1 = *x2
//      case (l: VariableExp, r: NullExp) => ruleDeferred(l, r) // deferred: X = null
      case (l: VariableExp, _) => pt(l) = pt(l) + right // any other thing
    }
  }

  /**
   * Case: x = alloc i
   * Rule: alloc-i ∈ pt(x)
   */
  def ruleAllocation(left: VariableExp, right: AllocExp): Unit = pt(left) = pt(left) + right

  /**
   * Case: x1 = &x2
   * Rule: x2 ∈ pt(x1)
   */
  def ruleLocation(left: VariableExp, right: LocationExp): Unit = pt(left) = pt(left) + VariableExp(right.pointer)

  /**
   * Case: x1 = x2
   * Rule: pt(x2) ⊆ pt(x1)
   */
  def ruleCopy(left: VariableExp, right: PointerExp): Unit = pt(left) = pt(left) union pt(VariableExp(right.name))


    /**
     * Case: x1 = *x2
     * Rule: c ∈ pt(x2) =⇒ pt(c) ⊆ pt(x1) for each c ∈ Cells
     */
    def ruleLoad(left: VariableExp, right: LoadExp): Unit = right.exp match {
        case PointerExp (name) => {
          for (v <- pt(VariableExp (name) ) if v.isInstanceOf[VariableExp] )
            pt(left) = pt(left) union pt(v.asInstanceOf[VariableExp] )
          }
    }

    /**
     * Case: *x1 = x2
     * Rule: c ∈ pt(x1) =⇒ pt(x2) ⊆ pt(c) for each c ∈ Cells
     */
    def ruleStore(left: LoadExp, right: Expression): Unit = left.exp match {
      case PointerExp(name) => {
        for (v <- pt(VariableExp(name)) if v.isInstanceOf[VariableExp])
          pt(v.asInstanceOf[VariableExp]) = pt(v.asInstanceOf[VariableExp]) union pt(variables(right).head)
      }
    }

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
