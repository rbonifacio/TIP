package br.unb.cic.tip

import br.unb.cic.tip.svf.GraphSVF
import br.unb.cic.tip.utils.{AddExp, BasicExp, BracketExp, ConstExp, DivExp, EqExp, Expression, FunDecl, FunctionCallExp, GTExp, Id, InputExp, LoadExp, MultiExp, NameExp, Node, PointerExp, Program, Stmt, SubExp, VariableExp}
import br.unb.cic.tip.utils.Stmt.*
import br.unb.cic.tip.utils.Node.*

type Edge = (Node, Node)
type Graph = Set[Edge]

def initStmt(stmt: Stmt): Stmt = stmt match {
  case SequenceStmt(s1, _)      => initStmt(s1)
  case AssignmentStmt(id, exp)  => exp match {
    case FunctionCallExp(_, _)  => CallStmt(AssignmentStmt(id, exp))
    case _                               => stmt
  }
  case _                        => stmt
}

def finalStmt(stmt: Stmt): Set[Stmt] = stmt match {
  case SequenceStmt(_, s2)      => finalStmt(s2)
  case IfElseStmt(_, s1, s2)    => finalStmt(s1) union (if (s2.isDefined) finalStmt(s2.get) else Set())
  case AssignmentStmt(id, exp)  => exp match {
    case FunctionCallExp(_, _)  => Set(AfterCallStmt(AssignmentStmt(id, exp)))
    case _                                      => Set(stmt)
  }
  case _                        => Set(stmt)
}

def blocks(stmt: Stmt): Set[Stmt] = stmt match {
  case SequenceStmt(s1, s2)         => blocks(s1) union blocks(s2)
  case IfElseStmt(_, s1, Some(s2))  => blocks(s1) union blocks(s2) union Set(stmt) 
  case IfElseStmt(_, s1, None)      => blocks(s1) union Set(stmt) 
  case WhileStmt(_, s1)             => blocks(s1) union Set(stmt) 
  case _                            => Set(stmt)
}

/**
 * This function returns an intra procedural CFG
 * Which is generated according to each statement's type
 */
def flow(stmt: Stmt): Graph = stmt match {
  case SequenceStmt(s1, s2)         => flow(s1) union flow(s2) union finalStmt(s1).map(s => (SimpleNode(s),SimpleNode(initStmt(s2))))
  case IfElseStmt(_, s1, Some(s2))  => flow(s1) union flow(s2) union Set((SimpleNode(stmt), SimpleNode(initStmt(s1)))) union Set((SimpleNode(stmt), SimpleNode(initStmt(s2))))
  case IfElseStmt(_, s1, None)      => flow(s1) union Set((SimpleNode(stmt), SimpleNode(initStmt(s1))))
  case WhileStmt(_, s1)             => flow(s1) union Set((SimpleNode(stmt), SimpleNode(initStmt(s1)))) union finalStmt(s1).map(s => (SimpleNode(s),SimpleNode(stmt)))
    case AssignmentStmt(id, exp) => exp match {
      case FunctionCallExp(fName, _)  => Set((SimpleNode(CallStmt(AssignmentStmt(id, exp))), StartNode(fName))) union Set((EndNode(fName), SimpleNode(AfterCallStmt(AssignmentStmt(id, exp))))) union Set((SimpleNode(CallStmt(AssignmentStmt(id, exp))), SimpleNode(AfterCallStmt(AssignmentStmt(id, exp)))))
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

def variables(stmt: Stmt): Set[BasicExp] = stmt match {
  case SequenceStmt(s1, s2)   => variables(s1) union variables(s2)
  case AssignmentStmt(s1, s2) => variables(s1) union variables(s2)
  case IfElseStmt(condition, _, _) => variables(condition)
  case WhileStmt(condition, _) => variables(condition)
  case OutputStmt(exp) => variables(exp)
  case _ => Set()
}

def variables(exp: Expression): Set[BasicExp] = exp match {
  case VariableExp(name)      => Set(VariableExp(name))
  case PointerExp(name)       => Set(PointerExp(name))
  case AddExp(left, right)    => variables(left) union variables(right)
  case SubExp(left, right)    => variables(left) union variables(right)
  case MultiExp(left, right)  => variables(left) union variables(right)
  case DivExp(left, right)    => variables(left) union variables(right)
  case EqExp(left, right)     => variables(left) union variables(right)
  case GTExp(left, right)     => variables(left) union variables(right)
  case BracketExp(exp)        => variables(exp)
  case LoadExp(exp)           => variables(exp)
  case _                      => Set()
}

def successors(stmt: Stmt, cfg: Graph): Set[Stmt] = {
  var res = Set[Stmt]()
  for ((from, to) <- cfg if from == SimpleNode(stmt)) {
    to match {
      case SimpleNode(s) => res = res union Set(s)
      case _             => throw new Error("Match error")
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

def expDependsOn(exp: Expression, id: VariableExp): Boolean = exp match {
  case VariableExp(name) => name == id.name
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

def convertSVFtoGraph(svf: GraphSVF): Graph = svf.map { case(l,r) => (SVFNode(l._1, l._2), SVFNode(r._1, r._2)) }.foldLeft(Set())(_ + _)
