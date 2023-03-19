package br.unb.cic.tip

import org.scalatest.funsuite.AnyFunSuite
import br.unb.cic.tip.Expression.{ConstExp, EqExp}
import br.unb.cic.tip.Stmt.{AssignmentStmt, IfElseStmt, SequenceStmt}

class FinalStmtTest extends AnyFunSuite {

  test("final of sequence for simple stmts") {
    val s1 = AssignmentStmt("x", ConstExp(1))
    val s2 = AssignmentStmt("y", ConstExp(2))
    val seq = SequenceStmt(s1, s2)

    assert(Set[Stmt](s2) == finalStmt(seq))
  }

  test("final of sequence for if stmt") {
    val s1 = AssignmentStmt("x", ConstExp(1))
    val s2 = AssignmentStmt("y", ConstExp(2))
    val s3 = AssignmentStmt("z", ConstExp(3))
    val s4 = IfElseStmt(EqExp(ConstExp(1),ConstExp(2)), SequenceStmt(s1,s2), None)

    assert(Set[Stmt](s2) == finalStmt(s4))
  }

  test("final of sequence for if else stmt") {
    val s1 = AssignmentStmt("x", ConstExp(1))
    val s2 = AssignmentStmt("y", ConstExp(2))
    val s3 = AssignmentStmt("z", ConstExp(3))
    val s4 = IfElseStmt(EqExp(ConstExp(1),ConstExp(2)), SequenceStmt(s1,s2), Some(s3))

    assert(Set[Stmt](s2, s3) == finalStmt(s4))
  }
}
