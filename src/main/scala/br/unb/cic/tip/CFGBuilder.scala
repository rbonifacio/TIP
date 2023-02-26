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
  def generate(function: FunDecl): CFG =
    generate(function.body, List[Edge]())

  def generate(stmt: Stmt, edges: List[Edge]): CFG = {

    stmt match {
      case SequenceStmt(s1,s2) =>  generate(s1, edges) concat generate(s2, getLabel(s1))
      case AssignmentStmt(_,_,l) => List[Node](Node(l, stmt, edges))
      case OutputStmt(_,l) => List[Node](Node(l, stmt, edges))
//      case ReturnStmt(_) => Node(counter, stmt)
      case _ => List[Node]()
    }
  }

  def getLabel(stmt: Stmt): List[Edge] = {
    stmt match {
      case AssignmentStmt(_,_,label) => List[Edge](label)
      case OutputStmt(_,label) => List[Edge](label)
      case _ => List[Edge]()
    }
  }
}