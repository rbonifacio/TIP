package br.unb.cic.tip

import br.unb.cic.tip.*
import br.unb.cic.tip.Stmt.*
import br.unb.cic.tip.Expression.*
import br.unb.cic.tip.Node.SimpleNode

import scala.collection.mutable

type DF = (Set[AssignmentStmt], Set[AssignmentStmt])
type Result = mutable.HashMap[Stmt, DF]

object ReachingDefinition {

    def run(program: Program): Result = {
      run(getMethodBody(program), program)
    }
    def run(fBody: Stmt, program: Program = List[FunDecl]()): Result = {
      var explore = true

      var RD: Result = mutable.HashMap()
      var en = Set[AssignmentStmt]()
      var ex = Set[AssignmentStmt]()

      for (stmt <- blocks(fBody)) {
        RD(stmt) = (en, ex)
      }

      while (explore) {
        val lastRD = RD.clone()

        for (stmt <- blocks(fBody)) {

          en = Set()
          ex = Set()

          stmt match
            case AssignmentStmt(_, exp) => exp match
              case DirectFunctionCallExp(name, _) => RD = RD ++ run(getMethodBody(program, name), program)
              case _ => {
                  en = entry(fBody, stmt, RD)
                  ex = exit(fBody, stmt, RD)
              }
            case _ => {
              en = entry(fBody, stmt, RD)
              ex = exit(fBody, stmt, RD)
            }
          RD(stmt) = (en, ex)
        }
        explore = lastRD != RD
      }
//      println(RD)
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
//            case SimpleNode(s) => res = RD(s)._2 union res
            case SimpleNode(s) => s match
              case AfterCallStmt(function, name) => null
              case CallStmt(function, name) => null
              case _ => res = RD(s)._2 union res
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