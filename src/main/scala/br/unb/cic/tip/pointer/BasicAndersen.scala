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
      case (l: LoadExp, _) => l.exp match { // assign: *X1 = X2
         case PointerExp (name) => {
           for (v <- result (VariableExp(name)) if v.isInstanceOf[VariableExp] )
             result (v.asInstanceOf[VariableExp]) = result (v.asInstanceOf[VariableExp]) union result (variables(right).head)
         }
         case _ => Set ()
      }
      case (l: VariableExp, r: AllocExp) => result(l) = result(l) + r // alloc: X = alloc P
      case (l: VariableExp, r: LocationExp) => result(l) = result(l) + VariableExp(r.pointer)  // location: X1 = &X2
      case (l: VariableExp, r: PointerExp) => result(l) = result(l) union result(VariableExp(r.name)) // assign: X1 = X2
      case (l: VariableExp, r: LoadExp) => r.exp match { // assign: *X1 = X2
        case PointerExp(name) => {
          for (v <- result(VariableExp(name)) if v.isInstanceOf[VariableExp])
            result(l) = result(l) union result(v.asInstanceOf[VariableExp])
        }
        case _ => Set()
      }
//      case (l: VariableExp, r: NullExp) => result(l) = Set() pre: X = null
      case (l: VariableExp, _) => result(l) = result(l) + right //any other thing
    }
    case _ => Set()
  }
}
