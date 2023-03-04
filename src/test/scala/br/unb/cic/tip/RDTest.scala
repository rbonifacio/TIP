package br.unb.cic.tip

import org.scalatest.funsuite.AnyFunSuite
import br.unb.cic.tip.Expression.*
import br.unb.cic.tip.Stmt.*
import br.unb.cic.tip.Node.*
import br.unb.cic.tip.*

class RDTest extends AnyFunSuite {

  test("test_get_assignment_from_stmts") {
    val s1 = AssignmentStmt("x", ConstExp(1))
    val s2 = AssignmentStmt("y", ConstExp(2))
    val seq = SequenceStmt(s1, s2)

    val expected = Set(
      s1,
      s2
    )

    assert( expected == statements(seq))
  }

  test("test_get_assignment_from_factorial") {
    val s1 = AssignmentStmt("f", ConstExp(1))
    val s2 = AssignmentStmt("f", MultiExp(VariableExp("f"), VariableExp("n")))
    val s3 = AssignmentStmt("n", SubExp(VariableExp("n"), ConstExp(1)))
    val s4 = SequenceStmt(s2,s3)
    val s5 = WhileStmt(GTExp(VariableExp("n"), ConstExp(0)),s4)
    val s6 = SequenceStmt(s1, s5)

    val expected = Set(
      s1,
      s2,
      s3
    )

    assert(expected == statements(s6))
  }
}