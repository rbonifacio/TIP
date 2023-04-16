package br.unb.cic.tip.df

import scala.collection.mutable
import br.unb.cic.tip.*
import br.unb.cic.tip.utils.Expression.*
import br.unb.cic.tip.utils.Node.SimpleNode
import br.unb.cic.tip.utils.Stmt.*
import br.unb.cic.tip.utils.Stmt

type RD = (Set[AssignmentStmt], Set[AssignmentStmt])
type ResultRD = mutable.HashMap[Stmt, RD]

object ReachingDefinition {

    def run(program: Stmt): ResultRD = {
      var explore = true

      val RD: ResultRD = mutable.HashMap()
      var en = Set[AssignmentStmt]() // it starts with empty Set
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

    def entry(program: Stmt, stmt: Stmt, RD: ResultRD): Set[AssignmentStmt] = {
      var res = Set[AssignmentStmt]()
      for ((from, to) <- flow(program) if to == SimpleNode(stmt)) {
        from match {
          case SimpleNode(s) => res = RD(s)._2 union res
        }
      }
      res
    }

    def exit(stmt: Stmt, RD: ResultRD): Set[AssignmentStmt] = stmt match {
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