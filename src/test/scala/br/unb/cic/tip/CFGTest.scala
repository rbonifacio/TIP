package br.unb.cic.tip

import org.scalatest.funsuite.AnyFunSuite
import br.unb.cic.tip.Expression.*
import br.unb.cic.tip.Stmt.*
import br.unb.cic.tip.Node.*
import br.unb.cic.tip.*

class CFGTest extends AnyFunSuite {

//  test("final of sequence for simple stmts") {
//    val s1 = AssignmentStmt("x", ConstExp(1))
//    val s2 = AssignmentStmt("y", ConstExp(2))
//    val seq = SequenceStmt(s1, s2)
//
//    assert(Set[Stmt](s2) == finalStmt(seq))
//  }
//
//  test("final of sequence for if stmt") {
//    val s1 = AssignmentStmt("x", ConstExp(1))
//    val s2 = AssignmentStmt("y", ConstExp(2))
//    val s3 = AssignmentStmt("z", ConstExp(3))
//    val s4 = IfElseStmt(EqExp(ConstExp(1),ConstExp(2)), SequenceStmt(s1,s2), None)
//
//    assert(Set[Stmt](s2) == finalStmt(s4))
//  }
//
//  test("final of sequence for if else stmt") {
//    val s1 = AssignmentStmt("x", ConstExp(1))
//    val s2 = AssignmentStmt("y", ConstExp(2))
//    val s3 = AssignmentStmt("z", ConstExp(3))
//    val s4 = IfElseStmt(EqExp(ConstExp(1),ConstExp(2)), SequenceStmt(s1,s2), Some(s3))
//
//    assert(Set[Stmt](s2, s3) == finalStmt(s4))
//  }
//
  test("cfg simple stmts") {
    val s1 = AssignmentStmt("x", ConstExp(1))
    val s2 = AssignmentStmt("y", ConstExp(2))
    val seq = SequenceStmt(s1, s2)

    val expected = Set(
        (SimpleNode(s1), SimpleNode(s2)),
    )

    assert(expected == flow(seq))
  }
//
//  /**
//      f = 1;
//      while (n>0) {
//        f = f*n;
//        n = n-1;
//      }
//   */
//  test("cfg factorial") {
//    val s1 = AssignmentStmt("f", ConstExp(1))
//    val s2 = AssignmentStmt("f", MultiExp(VariableExp("f"), VariableExp("n")))
//    val s3 = AssignmentStmt("n", SubExp(VariableExp("n"), ConstExp(1)))
//    val s4 = SequenceStmt(s2,s3)
//    val s5 = WhileStmt(GTExp(VariableExp("n"), ConstExp(0)),s4)
//    val s6 = SequenceStmt(s1, s5)
//
//    val expected = Set(
//      (s1, s5),
//      (s5, s2),
//      (s2, s3),
//      (s3, s5)
//    )
//
//    assert(expected == flow(s6))
//  }

  test("Test CFG using function with only statements") {
    val s1 = AssignmentStmt("x", ConstExp(1))
    val s2 = AssignmentStmt("y", ConstExp(1))
    val s3 = AssignmentStmt("z", AddExp(VariableExp("x"),VariableExp("y")))

    val body = SequenceStmt(s1, SequenceStmt(s2, s3))
    val function = FunDecl("sum", List(), List(), body, VariableExp("z"))

    val expected = Set(
        (StartNode, SimpleNode(s1)),
        (SimpleNode(s1), SimpleNode(s2)),
        (SimpleNode(s2), SimpleNode(s3)),
        (SimpleNode(s3), EndNode),
      )

    assert(expected == flow(function) )
  }
}