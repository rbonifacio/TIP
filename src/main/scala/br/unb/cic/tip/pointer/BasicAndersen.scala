package br.unb.cic.tip.pointer

import br.unb.cic.tip.df.ReachingDefinition.RD
import br.unb.cic.tip.{blocks, nonTrivialExpressions, variables}
import br.unb.cic.tip.utils.Expression.{NullExp, VariableExp, LoadExp, PointerExp, AllocExp, LocationExp}
import br.unb.cic.tip.utils.{AssignmentPointerStmt, AssignmentStmt, Expression, Stmt}

import scala.collection.mutable

type Result = mutable.HashMap[VariableExp, Set[Expression]]

object BasicAndersen {

  var result: Result = mutable.HashMap()

  def run(body: Stmt): Result = {

    var explore = true
    for (variable <- variables(body)) {
      result(variable) = Set()
    }


    while (explore) {
      val lastLV = result.clone()

      for (stmt <- blocks(body)) {
          gen(stmt)
      }
      explore = lastLV != result
    }
    result
  }

  def gen(stmt: Stmt): Unit = stmt match {
    case AssignmentPointerStmt(left, right) => (left, right) match {
      case (l: LoadExp, _) => ruleStore(l, right) // store: *x1 = x2
      case (l: VariableExp, r: AllocExp) => ruleAllocation(l, r) // alloc: x = alloc i
      case (l: VariableExp, r: LocationExp) => ruleLocation (l, r) // location: x1 = &x2
      case (l: VariableExp, r: PointerExp) => ruleCopy(l, r) // assign: x1 = x2
      case (l: VariableExp, r: LoadExp) => ruleLoad(l, r) // load: x1 = *x2
//      case (l: VariableExp, r: NullExp) => ruleDeferred(l, r) // deferred: X = null
      case (l: VariableExp, _) => result(l) = result(l) + right //any other thing
    }
  }

  /**
   * Case: x = alloc i
   * Rule: alloc-i ∈ pt(x)
   */
  def ruleAllocation(left: VariableExp, right: AllocExp): Unit = {
      result(left) = result(left) + right
  }

  /**
   * Case: x1 = &x2
   * Rule: x2 ∈ pt(x1)
   */
  def ruleLocation(left: VariableExp, right: LocationExp): Unit = {
    result(left) = result(left) + VariableExp(right.pointer)
  }

  /**
   * Case: x1 = x2
   * Rule: pt(x2) ⊆ pt(x1)
   */
  def ruleCopy(left: VariableExp, right: PointerExp): Unit = {
    result(left) = result(left) union result(VariableExp(right.name))
  }

    /**
     * Case: x1 = *x2
     * Rule: c ∈ pt(x2) =⇒ pt(c) ⊆ pt(x1) for each c ∈ Cells
     */
    def ruleLoad(left: VariableExp, right: LoadExp): Unit = right.exp match {
        case PointerExp (name) => {
          for (v <- result (VariableExp (name) ) if v.isInstanceOf[VariableExp] )
            result (left) = result (left) union result (v.asInstanceOf[VariableExp] )
          }
    }

    /**
     * Case: *x1 = x2
     * Rule: c ∈ pt(x1) =⇒ pt(x2) ⊆ pt(c) for each c ∈ Cells
     */
    def ruleStore(left: LoadExp, right: Expression): Unit = left.exp match {
      case PointerExp(name) => {
        for (v <- result(VariableExp(name)) if v.isInstanceOf[VariableExp])
          result(v.asInstanceOf[VariableExp]) = result(v.asInstanceOf[VariableExp]) union result(variables(right).head)
      }
    }

    /**
     * Case: x1 = null
     * Rule: pt(x1) = ()
     */
//    def ruleDeferred(left: VariableExp, right: NullExp): Unit = result(left) = Set()
}
//template for rules
//  /**
//   * Case:
//   * Rule:
//   */
//  def rule?(left: VariableExp, right: ?): Unit = {
//
//  }
