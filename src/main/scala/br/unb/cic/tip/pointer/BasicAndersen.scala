package br.unb.cic.tip.pointer

import br.unb.cic.tip.df.ReachingDefinition.RD
import br.unb.cic.tip.{blocks, nonTrivialExpressions, variables}
import br.unb.cic.tip.utils.Expression.*
import br.unb.cic.tip.utils.{AssignmentStmt, Expression, Stmt}

import scala.collection.mutable

type Result = mutable.HashMap[VariableExp, Set[Expression]]

object BasicAndersen {

  var result: Result = mutable.HashMap()

  def run(body: Stmt): Result = {

    for (variable <- variables(body)) {
      result(variable) = Set()
    }

    for (stmt <- blocks(body)) {
        gen(stmt)
    }
    result
  }

  def gen(stmt: Stmt): Unit = stmt match {
//    case AssignmentStmt(id, exp) =>  id match {
//      case PointerExp(_) => Set()
//      case _ => Set()
//    }

    case AssignmentStmt(right, left) => left match {
        case AllocExp(exp) => result(VariableExp(right)) = result(VariableExp(right)) + left // alloc: X = alloc P
        case LocationExp(exp) => result(VariableExp(right)) = result(VariableExp(right)) + VariableExp(exp) // location: X1 = &X2
        case PointerExp(exp) => result(VariableExp(right)) = result(VariableExp(right)) union result(VariableExp(exp)) // assign: X1 = X2
        case LoadExp(exp) => exp match { // assign: X1 = *X2
          case PointerExp(name) => {
              for (v <- result(VariableExp(name)) if v.isInstanceOf[VariableExp])
                  result(VariableExp(right)) = result(VariableExp(right)) union result(v.asInstanceOf[VariableExp])
          }
          case _ => Set()
        }
  //      case LoadExp(exp) => Set() // assign: *X1 = X2
        case NullExp => result(VariableExp(right)) = Set() // pre: X = null
        case _ => result(VariableExp(right)) = result(VariableExp(right)) + left //any other thing
    }
    case _ => Set()
  }
}
