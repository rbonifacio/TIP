package br.unb.cic.tip

import br.unb.cic.tip.*
import br.unb.cic.tip.Stmt.*
import br.unb.cic.tip.Expression.*
import br.unb.cic.tip.Node.SimpleNode

import scala.collection.mutable

type DF = (Set[AssignmentStmt], Set[AssignmentStmt])
type Result = mutable.HashMap[Stmt, DF]

object ReachingDefinition {

    def run(program: Stmt): Result = {
      var explore = true

      val RD: Result = mutable.HashMap()
      var en = Set[AssignmentStmt]()
      var ex = Set[AssignmentStmt]()

      val cfg = flow(program)

      for (stmt <- blocks(program)) {
        RD(stmt) = (en, ex)
      }

      while (explore) {
        val lastRD = RD.clone()

        for (stmt <- blocks(program)) {

          en = entry(program, stmt, RD)
          ex = exit(program, stmt, RD)
          RD(stmt) = (en, ex)
        }
        explore = lastRD != RD
      }
      RD
    }

    def entry(program: Stmt, stmt: Stmt, RD: Result): Set[AssignmentStmt] = {
      if (stmt == initStmt(program)) {
        assignments(program).map({ case AssignmentStmt(r, _) => AssignmentStmt(r, NullExp)})
      }
      else {
        var res = Set[AssignmentStmt]()
        for ((from, to) <- flow(program) if to == SimpleNode(stmt)) {
          from match {
            case SimpleNode(s) => res = RD(s)._2 union res
          }
        }
        res
      }
    }

    def exit(program: Stmt, stmt: Stmt, RD: Result): Set[AssignmentStmt] = {
      (RD(stmt)._1 diff kill(program, stmt)) union gen(stmt)
    }

    def kill(program: Stmt, stmt: Stmt): Set[AssignmentStmt] = stmt match {
      case AssignmentStmt(id, exp) => assignments(program).filter(_.name == id) union Set(AssignmentStmt(id, NullExp))
      case _ => Set()
    }

    def gen(stmt: Stmt): Set[AssignmentStmt] = stmt match {
      case AssignmentStmt(id, exp) => Set(AssignmentStmt(id, exp))
      case _ => Set()
    }
}