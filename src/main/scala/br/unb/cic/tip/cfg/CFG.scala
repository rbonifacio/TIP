package br.unb.cic.tip

import br.unb.cic.tip.utils.Stmt.*
import br.unb.cic.tip.utils.Expression.*
import br.unb.cic.tip.utils.{Expression, FunDecl, Id, Node, Program, Stmt}
import br.unb.cic.tip.utils.Node.*

type Edge = (Node, Node)
type Graph = Set[Edge]

def initStmt(stmt: Stmt): Stmt = stmt match {
  case SequenceStmt(s1, _)      => initStmt(s1)
  case AssignmentStmt(id, exp)  => exp match {
    case FunctionCallExp(NameExp(function), _)  => CallStmt(id, function)
    case _                                      => stmt
  }
  case _                        => stmt
}

def finalStmt(stmt: Stmt): Set[Stmt] = stmt match {
  case SequenceStmt(_, s2)      => finalStmt(s2)
  case IfElseStmt(_, s1, s2)    => finalStmt(s1) union (if (s2.isDefined) finalStmt(s2.get) else Set[Stmt]())
  case AssignmentStmt(id, exp)  => exp match {
    case FunctionCallExp(NameExp(function), _)  => Set(AfterCallStmt(id, function))
    case _                                      => Set(stmt)
  }
  case _                        => Set(stmt)
}

def blocks(stmt: Stmt): Set[Stmt] = stmt match {
  case SequenceStmt(s1, s2)         => blocks(s1) union blocks(s2)
  case IfElseStmt(_, s1, Some(s2))  => Set(stmt) union blocks(s1) union blocks(s2)
  case IfElseStmt(_, s1, None)      => Set(stmt) union blocks(s1)
  case WhileStmt(_, s1)             => Set(stmt) union blocks(s1)
  case _                            => Set(stmt)
}

def flow(stmt: Stmt): Graph = stmt match {
  case SequenceStmt(s1, s2)         => flow(s1) union flow(s2) union finalStmt(s1).map(s => (SimpleNode(s),SimpleNode(initStmt(s2))))
  case IfElseStmt(_, s1, Some(s2))  => flow(s1) union flow(s2) union Set((SimpleNode(stmt), SimpleNode(initStmt(s1)))) union Set((SimpleNode(stmt), SimpleNode(initStmt(s2))))
  case IfElseStmt(_, s1, None)      => flow(s1) union Set((SimpleNode(stmt), SimpleNode(initStmt(s1))))
  case WhileStmt(_, s1)             => flow(s1) union Set((SimpleNode(stmt), SimpleNode(initStmt(s1)))) union finalStmt(s1).map(s => (SimpleNode(s),SimpleNode(stmt)))
    case AssignmentStmt(id, exp) => exp match {
      case FunctionCallExp(NameExp(function), _)  => Set((SimpleNode(CallStmt(id, function)), StartNode(function))) union Set((EndNode(function), SimpleNode(AfterCallStmt(id, function))))
      case _                                      => Set()
    }
  case _                            => Set()
}

def flow(p: Program): Graph =
  p.map(f => flow(f)).foldLeft(Set())(_ union _)

def flow(f: FunDecl): Graph =
  Set((StartNode(f.name), SimpleNode(initStmt(f.body)))) union flow(f.body) union finalStmt(f.body).map(s => (SimpleNode(s), EndNode(f.name)))

def flowR(stmt: Stmt): Graph =
  flow(stmt).map((from, to) => (to, from))

def assignments(stmt: Stmt): Set[AssignmentStmt] = stmt match {
  case SequenceStmt(s1, s2)         => assignments(s1) union assignments(s2)
  case IfElseStmt(_, s1, Some(s2))  => assignments(s1) union assignments(s2)
  case IfElseStmt(_, s1, None)      => assignments(s1)
  case WhileStmt(_, s1)             => assignments(s1)
  case AssignmentStmt(id, exp)      => Set(AssignmentStmt(id, exp))
  case _                            => Set()
}

def variables(exp: Expression): Set[VariableExp] = exp match {
  case VariableExp(name)      => Set(VariableExp(name))
  case AddExp(left, right)    => variables(left) union variables(right)
  case SubExp(left, right)    => variables(left) union variables(right)
  case MultiExp(left, right)  => variables(left) union variables(right)
  case DivExp(left, right)    => variables(left) union variables(right)
  case EqExp(left, right)     => variables(left) union variables(right)
  case GTExp(left, right)     => variables(left) union variables(right)
  case BracketExp(exp)        => variables(exp)
//    case ConstExp(_) => Set()
  case _                      => Set()
}

def successors(stmt: Stmt, cfg: Graph): Set[Stmt] = {
  var res = Set[Stmt]()
  for ((from, to) <- cfg if from == SimpleNode(stmt)) {
    to match {
      case SimpleNode(s) => res = Set(s) union res
    }
  }
  res
}

def getMethodBody(program: Program, methodName: Id = "main"): Stmt = {
  program.exists(f => f.name == methodName) match
    case true => program.filter(f => f.name == methodName).head.body
    case _ => null
}


def nonTrivialExps(exp: Expression): Set[Expression] = exp match {
  case AddExp(l, r)   => Set(exp) | nonTrivialExps(l) | nonTrivialExps(r)
  case SubExp(l, r)   => Set(exp) | nonTrivialExps(l) | nonTrivialExps(r)
  case MultiExp(l, r) => Set(exp) | nonTrivialExps(l) | nonTrivialExps(r)
  case DivExp(l, r)   => Set(exp) | nonTrivialExps(l) | nonTrivialExps(r)
  case EqExp(l, r)    => Set(exp) | nonTrivialExps(l) | nonTrivialExps(r)
  case GTExp(l, r)    => Set(exp) | nonTrivialExps(l) | nonTrivialExps(r)
  case BracketExp(e)  => nonTrivialExps(e)
  case ConstExp(_)    => Set()
  case VariableExp(_) => Set()
  case InputExp       => Set()
  case _              => Set(exp)
}

def expDependsOn(exp: Expression, id: String): Boolean = exp match {
  case VariableExp(name) => name == id
  case AddExp(l, r)      => expDependsOn(l, id) || expDependsOn(r, id)
  case SubExp(l, r)      => expDependsOn(l, id) || expDependsOn(r, id)
  case MultiExp(l, r)    => expDependsOn(l, id) || expDependsOn(r, id)
  case DivExp(l, r)      => expDependsOn(l, id) || expDependsOn(r, id)
  case EqExp(l, r)       => expDependsOn(l, id) || expDependsOn(r, id)
  case GTExp(l, r)       => expDependsOn(l, id) || expDependsOn(r, id)
  case BracketExp(e)     => expDependsOn(e, id)
  case ConstExp(_)       => false
  case InputExp          => false
  case _                 => true
}