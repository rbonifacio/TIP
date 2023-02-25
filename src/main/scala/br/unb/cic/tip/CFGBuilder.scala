package br.unb.cic.tip

type NodeType = String
type Label = Integer
type Edge = Integer
type CFG = List[Node]
//type CFG = Node

//case class Node(id: Label, stmt: Stmt, nType: NodeType, InputEdges: List[Edge], OutputEdges: List[Edge])
case class Node(id: Label, stmt: Stmt, InputEdges: List[Edge])

object CFGBuilder {

    var counter = 0
//  def generate(program: Program): CFG =
//    program.map(p => generate(p))
////
//  def generate(function: FunDecl): CFG =
//    generate(function.body)

  def generate(stmts: List[Stmt]): CFG =
    stmts.map(stmt => generate(stmt))

  def generate(stmt: Stmt): Node = {
    counter = counter + 1
    stmt match {
      case AssignmentStmt(_,_) => Node(counter, stmt)
      case OutputStmt(_) => Node(counter, stmt)
//      case ReturnStmt(_) => Node(counter, stmt)
//      case _ => List[Node]()
      case _ => Node(0, stmt)
    }
  }
}