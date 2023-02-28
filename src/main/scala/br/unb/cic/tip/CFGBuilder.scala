package br.unb.cic.tip

type NodeType = String
type Edge = Integer
type CFG = Set[Node]

//case class Node(id: Label, stmt: Stmt, nType: NodeType, InputEdges: Set[Edge], OutputEdges: Set[Edge])
case class Node(id: Label, stmt: Stmt, inputEdges: Set[Edge], outputEdges: Set[Edge])

object CFGBuilder {

//  def generate(program: Program): CFG =
//    program.map(p => generate(p))

  def generate(function: FunDecl): CFG =
    generate(function.body, Set.empty, Set.empty)

  def generate(stmt: Stmt, inputEdges: Set[Edge], outputEdges: Set[Edge]): CFG = {

    stmt match {
      case SequenceStmt(stmt1, stmt2) =>  generate(stmt1, inputEdges, getLabel(stmt2)) concat generate(stmt2, getLabel(stmt1), outputEdges)
      case IfElseStmt(condition, stmt1, Some(stmt2), label) => Set(Node(label, stmt, inputEdges, outputEdges)) concat generate(stmt1, Set(label), outputEdges) concat generate(stmt2, Set(label), outputEdges)
      case IfElseStmt(condition, stmt1, None, label) => Set(Node(label, stmt, inputEdges, outputEdges)) concat generate(stmt1, Set(label), outputEdges)
      case AssignmentStmt(_, _, label) => Set(Node(label, stmt, inputEdges,outputEdges))
      case OutputStmt(_, label) => Set(Node(label, stmt, inputEdges, outputEdges))
      case ReturnStmt(_, label) => Set(Node(label, stmt, inputEdges, outputEdges))
      case DeclarationStmt(_, label) => Set(Node(label, stmt, inputEdges, outputEdges))
      case _ => Set.empty
    }
  }

  def getLabel(stmt: Stmt): Set[Edge] = {
    stmt match {
      case AssignmentStmt(_,_,label) => Set(label)
      case OutputStmt(_,label) => Set(label)
      case ReturnStmt(_, label) => Set(label)
      case DeclarationStmt(_, label) => Set(label)
      case IfElseStmt(_, _, _, label) => Set(label)
      case _ => Set.empty
    }
  }
}