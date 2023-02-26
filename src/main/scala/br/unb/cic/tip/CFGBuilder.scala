package br.unb.cic.tip

type NodeType = String
type Edge = Integer
type CFG = List[Node]
//type CFG = Node

//case class Node(id: Label, stmt: Stmt, nType: NodeType, InputEdges: List[Edge], OutputEdges: List[Edge])
case class Node(id: Label, stmt: Stmt)

object CFGBuilder {

//  def generate(program: Program): CFG =
//    program.map(p => generate(p))
////
//  def generate(function: FunDecl): CFG =
//    generate(function.body)

  def generate(stmtL: List[Stmt], label: Label): CFG = {
//    (generate(stmtL.head) :+ generate(stmtL.tail))
      if (stmtL.size > 1) {
        generate(stmtL.head, stmtL.tail, label)
      }
      else {
        if (stmtL.size == 1) {
          generate(stmtL.head, List(), label)
        }
        else {
          List[Node]()
        }

      }
//    stmtL.map(stmt => generate(stmt))
  }

  def generate(head: Stmt, tail: List[Stmt], label: Label): CFG = {
    head match {
      case AssignmentStmt(_,_,l) => generate(tail, l) :+ Node(l, head)
      case OutputStmt(_,l) => generate(tail, l) :+ Node(l, head)
//      case ReturnStmt(_) => Node(counter, stmt)
      case _ => List[Node]()
    }
  }
}