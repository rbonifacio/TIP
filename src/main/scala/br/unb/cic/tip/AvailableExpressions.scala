package br.unb.cic.tip

import br.unb.cic.tip.*
import br.unb.cic.tip.Stmt.*
import br.unb.cic.tip.Expression.*
import br.unb.cic.tip.Node.SimpleNode

import scala.collection.mutable

object AvailableExpressions {
  type AE = (Set[Expression], Set[Expression])
  type AEResult = mutable.HashMap[Stmt, AE]

  var cfg: Graph = Set()

  var allExpressions = Set[Expression]()

  def run(program: Stmt): AEResult = {
    var explore = true

    val AE: AEResult = mutable.HashMap()

    cfg = flow(program)

    for (stmt <- blocks(program)) {
      allExpressions ++= gen(stmt)
    }

    for (stmt <- blocks(program)) {
      AE(stmt) = (allExpressions, allExpressions)
    }

    while (explore) {
      val lastRD = AE.clone()

      for (stmt <- blocks(program)) {

        val entryExpressions = entry(program, stmt, AE)
        val exitExpressions = exit(program, stmt, AE)
        AE(stmt) = (entryExpressions, exitExpressions)
      }
      explore = lastRD != AE
    }

    AE
  }

  def entry(program: Stmt, stmt: Stmt, AE: AEResult): Set[Expression] = {
    if (stmt == initStmt(program)) {
      Set()
    } else {
      var res = allExpressions
      for ((from, to) <- cfg if to == SimpleNode(stmt)) {
        from match {
          case SimpleNode(s) => res = AE(s)._2 & res
          case _             => throw new RuntimeException("Error")
        }
      }
      res
    }
  }

  def exit(program: Stmt, stmt: Stmt, AE: AEResult): Set[Expression] = {
    (AE(stmt)._1 union gen(stmt)) diff kill(program, stmt)
  }

  def kill(program: Stmt, stmt: Stmt): Set[Expression] = stmt match {
    case AssignmentStmt(id, exp) =>
      allExpressions.filter(expDependsOn(_, id))
    case _ => Set()
  }

  def gen(stmt: Stmt): Set[Expression] = stmt match {
    case AssignmentStmt(_, exp) => genFromExps(exp)
    case WhileStmt(exp, _)      => genFromExps(exp)
    case IfElseStmt(exp, _, _)  => genFromExps(exp)
    case OutputStmt(exp)        => genFromExps(exp)
    case _                      => Set()
  }

  def genFromExps(exp: Expression): Set[Expression] = exp match {
    case AddExp(l, r)   => Set(exp) | genFromExps(l) | genFromExps(r)
    case SubExp(l, r)   => Set(exp) | genFromExps(l) | genFromExps(r)
    case MultiExp(l, r) => Set(exp) | genFromExps(l) | genFromExps(r)
    case DivExp(l, r)   => Set(exp) | genFromExps(l) | genFromExps(r)
    case EqExp(l, r)    => Set(exp) | genFromExps(l) | genFromExps(r)
    case GTExp(l, r)    => Set(exp) | genFromExps(l) | genFromExps(r)
    case BracketExp(e)  => genFromExps(e)
    case ConstExp(_)    => Set()
    case VariableExp(_) => Set()
    case InputExp       => Set()
    case _              => Set(exp)
  }

  def expDependsOn(exp: Expression, id: String): Boolean = exp match {
    case VariableExp(name) => name == id
    case AddExp(l, r)      => expDependsOn(l, id) || expDependsOn(r, id)
    case SubExp(l, r)      => expDependsOn(l, id) || expDependsOn(r, id)
    case MultiExp(l, r)    => expDependsOn(l, id) || expDependsOn(r, id)
    case DivExp(l, r)      => expDependsOn(l, id) || expDependsOn(r, id)
    case EqExp(l, r)       => expDependsOn(l, id) || expDependsOn(r, id)
    case GTExp(l, r)       => expDependsOn(l, id) || expDependsOn(r, id)
    case BracketExp(e)     => expDependsOn(e, id)
    case ConstExp(_)       => false
    case InputExp          => false
    case _                 => true
  }
}
