package br.unb.cic.tip

type NodeType = String
type Edge = Integer
type CFG = List[Node]

//case class Node(id: Label, stmt: Stmt, nType: NodeType, InputEdges: List[Edge], OutputEdges: List[Edge])
case class Node(id: Label, stmt: Stmt, InputEdges: List[Edge])

object CFGBuilder {

//  def generate(program: Program): CFG =
//    program.map(p => generate(p))

  def generate(function: FunDecl): CFG =
    generate(function.body, List[Edge]())

  def generate(stmt: Stmt, edges: List[Edge]): CFG = {

    stmt match {
      case SequenceStmt(stmt1, stmt2) =>  generate(stmt1, edges) concat generate(stmt2, getLabel(stmt1))
      case AssignmentStmt(_, _, label) => List[Node](Node(label, stmt, edges))
      case OutputStmt(_, label) => List[Node](Node(label, stmt, edges))
      case ReturnStmt(_, label) => List[Node](Node(label, stmt, edges))
      case DeclarationStmt(_, label) => List[Node](Node(label, stmt, edges))
      case _ => List[Node]()
    }
  }

  def getLabel(stmt: Stmt): List[Edge] = {
    stmt match {
      case AssignmentStmt(_,_,label) => List[Edge](label)
      case OutputStmt(_,label) => List[Edge](label)
      case ReturnStmt(_, label) => List[Edge](label)
      case DeclarationStmt(_, label) => List[Edge](label)
      case _ => List[Edge]()
    }
  }
}