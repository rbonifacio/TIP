package br.unb.cic.tip

type NodeType = String
type Edge = Integer
type CFG = List[Node]

//case class Node(id: Label, stmt: Stmt, nType: NodeType, InputEdges: List[Edge], OutputEdges: List[Edge])
case class Node(id: Label, stmt: Stmt, InputEdges: List[Edge])

object CFGBuilder {

//  def generate(program: Program): CFG =
//    program.map(p => generate(p))
////
//  def generate(function: FunDecl): CFG =
//    generate(function.body)

  def generate(stmtL: List[Stmt], label: Label = 0): CFG = {
    stmtL.size match {
      case 0 => List[Node]()
      case 1 => generate(stmtL.head, List(), label)
      case _ => generate(stmtL.head, stmtL.tail, label)
    }
  }

  def generate(head: Stmt, tail: List[Stmt], label: Label): CFG = {

    val pre = label match {
      case 0 => List[Edge]()
      case _ => List[Edge](label)
    }

    head match {
      case AssignmentStmt(_,_,l) => generate(tail, l) :+ Node(l, head, pre)
      case OutputStmt(_,l) => generate(tail, l) :+ Node(l, head, pre)
//      case ReturnStmt(_) => Node(counter, stmt)
      case _ => List[Node]()
    }
  }
}