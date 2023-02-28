package br.unb.cic.tip

import org.scalatest.funsuite.AnyFunSuite
import br.unb.cic.tip.Expression.*
import br.unb.cic.tip.Stmt.*
import br.unb.cic.tip.*

class CFGTest extends AnyFunSuite {

  test("final of sequence stmt") {
    val s1 = AssignmentStmt("x", ConstExp(1))
    val s2 = AssignmentStmt("y", ConstExp(2))
    val seq = SequenceStmt(s1, s2)

    assert(Set[Stmt](s2) == finalStmt(seq))
  }

  test("final of sequence if stmt") {
    val s1 = AssignmentStmt("x", ConstExp(1))
    val s2 = AssignmentStmt("y", ConstExp(2))
    val s3 = AssignmentStmt("z", ConstExp(3))
    val s4 = IfElseStmt(EqExp(ConstExp(1),ConstExp(2)), SequenceStmt(s1,s2), Some(s3))
    val s5 = IfElseStmt(EqExp(ConstExp(1),ConstExp(2)), SequenceStmt(s1,s2), None)

//    val seq = SequenceStmt(s1, s2)

    assert(Set[Stmt](s2, s3) == finalStmt(s4))
    assert(Set[Stmt](s2) == finalStmt(s5))
  }

  test("cfg stmts") {
    val s1 = AssignmentStmt("x", ConstExp(1))
    val s2 = AssignmentStmt("y", ConstExp(2))
    val seq = SequenceStmt(s1, s2)

    assert(Set((s1, s2)) == flow(seq))
  }

  test("cfg FACTORIAL") {
    val s1 = AssignmentStmt("f", ConstExp(1))
    val s2 = AssignmentStmt("f", MultiExp(VariableExp("f"), VariableExp("n")))
    val s3 = AssignmentStmt("n", SubExp(VariableExp("n"), ConstExp(1)))
    val s4 = SequenceStmt(s2,s3)
    val s5 = WhileStmt(GTExp(VariableExp("n"), ConstExp(0)),s4)
    val s6 = SequenceStmt(s1, s5)
//    val seq = SequenceStmt(s1, s2)
    val expexted = Set(
      (s1, s5),
      (s5, s2),
      (s2, s3),
      (s3, s5)
    )

    assert(expexted == flow(s6))
  }
}