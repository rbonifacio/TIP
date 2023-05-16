package br.unb.cic.tip.pointer

import br.unb.cic.tip.df.ReachingDefinition.RD
import br.unb.cic.tip.{blocks, nonTrivialExpressions, variables}
import br.unb.cic.tip.utils.Expression.*
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
    case AssignmentPointerStmt(id, exp) =>  id match {
      case LoadExp(e) => e match { // assign: *X1 = X2
          case PointerExp (name) => {
            for (v <- result (VariableExp(name)) if v.isInstanceOf[VariableExp] )
              result (v.asInstanceOf[VariableExp]) = result (v.asInstanceOf[VariableExp]) union result (variables(exp).head)
          }
          case _ => Set ()
      }
      case _ => genLeft(variables(id).head, exp)
    }
    case _ => Set()
  }

  def genLeft(left: VariableExp, right: Expression): Unit = right match {
    case AllocExp(exp) => result(left) = result(left) + right // alloc: X = alloc P
    case LocationExp(exp) => result(left) = result(left) + VariableExp(exp) // location: X1 = &X2
    case PointerExp(exp) => result(left) = result(left) union result(VariableExp(exp)) // assign: X1 = X2
    case LoadExp(exp) => exp match { // assign: X1 = *X2
      case PointerExp(name) => {
        for (v <- result(VariableExp(name)) if v.isInstanceOf[VariableExp])
          result(left) = result(left) union result(v.asInstanceOf[VariableExp])
      }
      case _ => Set()
    }
    //      case LoadExp(exp) => Set() // assign: *X1 = X2
    case NullExp => result(left) = Set() // pre: X = null
    case _ => result(left) = result(left) + right //any other thing
  }
}
