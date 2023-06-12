package br.unb.cic.tip.df

import scala.collection.mutable
import br.unb.cic.tip.*
import br.unb.cic.tip.utils.Expression
import br.unb.cic.tip.utils.Node.SimpleNode
import br.unb.cic.tip.utils.*
import br.unb.cic.tip.utils.Stmt.*

import scala.collection.immutable.Set

type ReachingDefinition = (Set[AssignmentStmt], Set[AssignmentStmt])
type ResultRD = mutable.HashMap[(Stmt, Stmt), ReachingDefinition]

object ReachingDefinition {

    var RD: ResultRD = mutable.HashMap()

    def run(program: Program): ResultRD = {
      RD = mutable.HashMap()
      run(getMethodBody(program), program) 
    } 

    def run(fBody: Stmt, program: Program = List[FunDecl](), predecessors: Set[AssignmentStmt] = Set(), context: Stmt = NopStmt): ResultRD = {
      var explore = true

      var en = Set[AssignmentStmt]() // it starts with empty Set
      var ex = Set[AssignmentStmt]()

      for (stmt <- blocks(fBody)) {
        RD((stmt, context)) = (en, ex)
      }

      while (explore) {
        val lastRD = RD.clone()

        for (stmt <- blocks(fBody)) {
          en = Set() 
          ex = Set()

          stmt match
            case AssignmentStmt(_, exp) => exp match {
              case FunctionCallExp(fName, args) =>
                en = entry(fBody, stmt, predecessors, context)
                // filter and get predecessor that are send as parameters to the function
                val usedPredecessors = en.filter(e => args.exists(_ == e.name))
                run(getMethodBody(program, fName), program, usedPredecessors, stmt)
                // a kind of Interprocedural Exit Function was created in the next lines
                val in: Set[AssignmentStmt] = exit(stmt)
                val RDCallee: Set[AssignmentStmt] = finalStmt(getMethodBody(program, fName)).map(s => RD((s, stmt))._2).foldLeft(Set())(_ union _)
                val g = RDCallee.filter(e => args.exists(_ == e.name)) // filter and keep just parameters that were sent
                val kill = in.filter(i => g.exists(g => g.name == i.name))
                ex = ((in diff kill) union g) union gen(stmt)
              case _ =>
                en = entry(fBody, stmt, predecessors, context)
                ex = exit(stmt, context)
            }
            case _ =>
              en = entry(fBody, stmt, predecessors, context)
              ex = exit(stmt, context)
          RD((stmt, context)) = (en, ex)
        }
        explore = lastRD != RD
      }
      RD
    }

    def entry(program: Stmt, stmt: Stmt, predecessors: Set[AssignmentStmt], context: Stmt = NopStmt): Set[AssignmentStmt] = {
      var res = Set[AssignmentStmt]()
      //validate if it is a call =
      var _stmt: Stmt = NopStmt
      stmt match
        case AssignmentStmt(v, exp) => exp match
          case FunctionCallExp(_, _) => {
            _stmt = CallStmt(AssignmentStmt(v, exp))
          }
          case _ => _stmt = stmt
        case _ => _stmt = stmt

      for ((from, to) <- flow(program) if to == SimpleNode(_stmt)) {
        from match {
          case SimpleNode(s) => s match
            case AfterCallStmt(ss) => res = RD((ss, context))._2 union res // it is coming, why?
            case _ => res = RD((s, context))._2 union res
        }
      }
      (predecessors diff predecessors.filter(i => res.exists(g => g.name == i.name))) union res
    }

    def exit(stmt: Stmt, context: Stmt = NopStmt): Set[AssignmentStmt] = stmt match {
      case AssignmentStmt(v, exp) => exp match
        case FunctionCallExp(_, _) => {
            RD((stmt, context))._1
        }
        case _ => (RD((stmt, context))._1 diff kill(RD((stmt, context))._1, stmt)) union gen(stmt)
      case _ => RD((stmt, context))._1
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