package br.unb.cic.tip.df

import scala.collection.mutable
import br.unb.cic.tip.*
import br.unb.cic.tip.utils.Expression.*
import br.unb.cic.tip.utils.Node.SimpleNode
import br.unb.cic.tip.utils.Stmt.*
import br.unb.cic.tip.utils.{FunDecl, Program, Stmt}

type RD = (Set[AssignmentStmt], Set[AssignmentStmt])
type ResultRD = mutable.HashMap[Stmt, RD]

object ReachingDefinition {

    def run(program: Program): ResultRD = { 
      run(getMethodBody(program), program) 
    } 

    def run(fBody: Stmt, program: Program = List[FunDecl]()): ResultRD = {
      var explore = true

      var RD: ResultRD = mutable.HashMap()
      var en = Set[AssignmentStmt]() // it starts with empty Set
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
              case FunctionCallExp(NameExp(name), _) => {
                en = entry(fBody, stmt, RD)
                RD = RD ++ run(getMethodBody(program, name), program)
                // just for testing, a Interprocedural Exit Function was created in the next lines
                val in: Set[AssignmentStmt] = exit(stmt, RD)
                val gen: Set[AssignmentStmt] = finalStmt(getMethodBody(program, name)).map(s => RD(s)._2).foldLeft(Set())(_ union _)
                val kill = in.filter( i => gen.exists( g => g.name == i.name))
                ex = (in diff kill) union gen
              }
              case _ => { 
                en = entry(fBody, stmt, RD) 
                ex = exit(stmt, RD) 
              } 
            case _ => { 
              en = entry(fBody, stmt, RD) 
              ex = exit(stmt, RD) 
            } 
          RD(stmt) = (en, ex)
        }
        explore = lastRD != RD
      }
      RD
    }

    def entry(program: Stmt, stmt: Stmt, RD: ResultRD): Set[AssignmentStmt] = {
      var res = Set[AssignmentStmt]()
      //validate if it is a call =
      var _stmt: Stmt = NopStmt
      stmt match
        case AssignmentStmt(v, exp) => exp match
          case FunctionCallExp(NameExp(name), _) => {
            _stmt = CallStmt(v, name)
          }
          case _ => _stmt = stmt
        case _ => _stmt = stmt

      for ((from, to) <- flow(program) if to == SimpleNode(_stmt)) {
        from match {
          case SimpleNode(s) => s match
            case AfterCallStmt(_, _) => null // it is coming, why?
            case _ => res = RD(s)._2 union res
        }
      }
      res
    }

    def exit(stmt: Stmt, RD: ResultRD): Set[AssignmentStmt] = stmt match {
      case AssignmentStmt(v, exp) => exp match
        case FunctionCallExp(_, _) => {
            RD(stmt)._1
        }
        case _ => (RD(stmt)._1 diff kill(RD(stmt)._1, stmt)) union gen(stmt)
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