package br.unb.cic.tip

type NodeType = String
type Edge = Integer
type CFG = Set[Node]

//case class Node(id: Label, stmt: Stmt, nType: NodeType, InputEdges: Set[Edge], OutputEdges: Set[Edge])
case class Node(id: Label, stmt: Stmt, InputEdges: Set[Edge], OutputEdges: Set[Edge])

object CFGBuilder {

//  def generate(program: Program): CFG =
//    program.map(p => generate(p))

  def generate(function: FunDecl): CFG =
    generate(function.body, Set.empty, Set.empty)

  def generate(stmt: Stmt, InputEdges: Set[Edge], OutputEdges: Set[Edge]): CFG = {

    stmt match {
      case SequenceStmt(stmt1, stmt2) =>  generate(stmt1, InputEdges, getLabel(stmt2)) concat generate(stmt2, getLabel(stmt1), OutputEdges)
      case IfStmt(condition, stmt, label) => Set(Node(label, stmt, InputEdges, OutputEdges)) concat generate(stmt, Set(label), OutputEdges)
      case AssignmentStmt(_, _, label) => Set(Node(label, stmt, InputEdges,OutputEdges))
      case OutputStmt(_, label) => Set(Node(label, stmt, InputEdges, OutputEdges))
      case ReturnStmt(_, label) => Set(Node(label, stmt, InputEdges, OutputEdges))
      case DeclarationStmt(_, label) => Set(Node(label, stmt, InputEdges, OutputEdges))
      case _ => Set.empty
    }
  }

  def getLabel(stmt: Stmt): Set[Edge] = {
    stmt match {
      case AssignmentStmt(_,_,label) => Set(label)
      case OutputStmt(_,label) => Set(label)
      case ReturnStmt(_, label) => Set(label)
      case DeclarationStmt(_, label) => Set(label)
      case IfStmt(_, _, label) => Set(label)
      case _ => Set.empty
    }
  }
}