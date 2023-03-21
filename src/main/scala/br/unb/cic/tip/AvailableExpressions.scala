package br.unb.cic.tip

import br.unb.cic.tip.*
import br.unb.cic.tip.Stmt.*
import br.unb.cic.tip.Expression.*
import br.unb.cic.tip.Node.*

import scala.collection.mutable

type AEResult = mutable.HashMap[Stmt, mutable.Set[Expression]]

object AvailableExpressions {
  type StmtCFGEdges = mutable.HashMap[Stmt, mutable.ListBuffer[Stmt]]

  def run(program: Stmt): AEResult = {
    val pred: StmtCFGEdges = mutable.HashMap()
    val succ: StmtCFGEdges = mutable.HashMap()

    val workList: mutable.Set[Stmt] = mutable.Set()

    val result: AEResult = mutable.HashMap()

    for (stmt <- blocks(program)) {
      pred(stmt) = mutable.ListBuffer()
      succ(stmt) = mutable.ListBuffer()

      workList.addOne(stmt)

      result(stmt) = mutable.Set()
    }

    val cfg = flow(program)

    for (edge <- cfg) edge match {
      case (SimpleNode(v1), SimpleNode(v2)) => {
        pred(v2).addOne(v1)
        succ(v1).addOne(v2)
      }
      case _ => throw new RuntimeException("Error on CFG")
    }

    while (!workList.isEmpty) {
      val workItem: Stmt = workList.head
      workList.remove(workItem)

      val newState = pred(workItem)
        .map(pred => result(pred))
        .reduceLeftOption(_ & _)
        .getOrElse(mutable.Set())
        | stmtExpressions(workItem)

      assignedVar(workItem) match {
        case Some(id) => newState.filterInPlace(!expDependsOn(_, id))
        case _        =>
      }

      if (result(workItem) != newState) {
        result(workItem) = newState

        workList ++= succ(workItem)
      }

    }

    result
  }

  def stmtExpressions(stmt: Stmt): Set[Expression] = stmt match {
    case AssignmentStmt(_, exp) => subExpressions(exp)
    case WhileStmt(exp, _)      => subExpressions(exp)
    case IfElseStmt(exp, _, _)  => subExpressions(exp)
    case OutputStmt(exp)        => subExpressions(exp)
    case _                      => Set()
  }

  def subExpressions(exp: Expression): Set[Expression] = exp match {
    case AddExp(l, r)   => Set(exp) | subExpressions(l) | subExpressions(r)
    case SubExp(l, r)   => Set(exp) | subExpressions(l) | subExpressions(r)
    case MultiExp(l, r) => Set(exp) | subExpressions(l) | subExpressions(r)
    case DivExp(l, r)   => Set(exp) | subExpressions(l) | subExpressions(r)
    case EqExp(l, r)    => Set(exp) | subExpressions(l) | subExpressions(r)
    case GTExp(l, r)    => Set(exp) | subExpressions(l) | subExpressions(r)
    case BracketExp(e)  => subExpressions(e)
    case ConstExp(_)    => Set()
    case VariableExp(_) => Set()
    case InputExp       => Set()
    case _              => Set(exp)
  }

  def assignedVar(stmt: Stmt): Option[String] = stmt match {
    case AssignmentStmt(id, _) => Some(id)
    case _                     => None
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
