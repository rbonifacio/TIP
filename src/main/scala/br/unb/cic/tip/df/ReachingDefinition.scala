package br.unb.cic.tip.df

import scala.collection.mutable
import br.unb.cic.tip.*
import br.unb.cic.tip.utils.Expression.*
import br.unb.cic.tip.utils.Node.SimpleNode
import br.unb.cic.tip.utils._
import br.unb.cic.tip.utils.{FunDecl, Program, Stmt}

import br.unb.cic.tip.utils.LabelSensitiveStmt.given

// class Sla[K, V] extends mutable.HashMap[K, V] {
//   override def
// }

type RD = (Set[AssignmentStmt], Set[AssignmentStmt])
type ResultRD = mutable.HashMap[LabelSensitiveStmt, RD]

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

        labeledToStmt(stmt) match
          case AssignmentStmt(_, exp) => exp match
              case FunctionCallExp(NameExp(name), _) =>
                RD = RD ++ run(getMethodBody(program, name), program)
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
    for ((from, to) <- flow(program) if to == SimpleNode(stmt)) {
      from match {
        case SimpleNode(s) => s match
            case AfterCallStmt(_, _) => null
            case CallStmt(_, _)      => null
            case _                   => res = RD(s)._2 union res
        case _             => throw new Error("Match error")
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
      case _                     => Set()
    }

  def gen(stmt: Stmt): Set[AssignmentStmt] = stmt match {
    case assignmentStmt: AssignmentStmt => Set(assignmentStmt)
    case _                              => Set()
  }
}
