package br.unb.cic.tip

import br.unb.cic.tip.*
import br.unb.cic.tip.Stmt.*
import br.unb.cic.tip.Expression.*
import br.unb.cic.tip.Node.SimpleNode

import scala.collection.mutable

type DF = (Set[AssignmentStmt], Set[AssignmentStmt])
type ResultDF = mutable.HashMap[Stmt, DF]

object ReachingDefinition {

    def run(program: Stmt): ResultDF = {
      var explore = true

      val RD: ResultDF = mutable.HashMap()
      var en = Set[AssignmentStmt]()
      var ex = Set[AssignmentStmt]()

      for (stmt <- blocks(program)) {
        RD(stmt) = (en, ex)
      }

      while (explore) {
        val lastRD = RD.clone()

        for (stmt <- blocks(program)) {
          en = entry(program, stmt, RD)
          ex = exit(stmt, RD)
          RD(stmt) = (en, ex)
        }
        explore = lastRD != RD
      }
      RD
    }

    def entry(program: Stmt, stmt: Stmt, RD: ResultDF): Set[AssignmentStmt] = {
        var res = Set[AssignmentStmt]()
        for ((from, to) <- flow(program) if to == SimpleNode(stmt)) {
          from match {
            case SimpleNode(s) => res = RD(s)._2 union res
          }
        }
        res
    }

    def exit(stmt: Stmt, RD: ResultDF): Set[AssignmentStmt] = stmt match {
      case AssignmentStmt(_, _) => (RD(stmt)._1 diff kill(RD(stmt)._1, stmt)) union gen(stmt)
      case _ => RD(stmt)._1
    }

    def kill(pre: Set[AssignmentStmt], stmt: Stmt): Set[AssignmentStmt] = stmt match {
      case AssignmentStmt(id, _) => pre.filter(_.name == id)
      case _ => Set()
    }

    def gen(stmt: Stmt): Set[AssignmentStmt] = stmt match {
      case AssignmentStmt(id, exp) => Set(AssignmentStmt(id, exp))
      case _ => Set()
    }
}