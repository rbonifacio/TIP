package br.unb.cic.tip.intraprocedural.df.mfp

import br.unb.cic.tip.*
import br.unb.cic.tip.utils.*
import br.unb.cic.tip.utils.Node.*
import br.unb.cic.tip.utils.Stmt.*
import br.unb.cic.tip.df.mfp.ReachingDefinitionMFP
import org.scalatest.funsuite.AnyFunSuite

class RDTest extends AnyFunSuite {

  /** X = 1 Y = 2 X = 3
    */
  test("test_rd_using_only_statements") {
    val s1 = AssignmentStmt(VariableExp("x"), ConstExp(1))
    val s2 = AssignmentStmt(VariableExp("y"), ConstExp(2))
    val s3 = AssignmentStmt(VariableExp("x"), ConstExp(3))
    val seq = SequenceStmt(s1, SequenceStmt(s2, s3))

    val RD = ReachingDefinitionMFP.run(seq)

    assert(
      RD(s1) == (
        Set(),
        Set(s1)
      )
    )

    assert(
      RD(s2) == (
        Set(s1),
        Set(s1, s2)
      )
    )

    assert(
      RD(s3) == (
        Set(s1, s2),
        Set(s2, s3)
      )
    )
  }

  /** x = 1 y = 2 x = x + y z = x + 1 z = y
    */
  test("test_rd_using_only_statements_complex") {
    val s1 = AssignmentStmt(VariableExp("x"), ConstExp(1))
    val s2 = AssignmentStmt(VariableExp("y"), ConstExp(2))
    val s3 = AssignmentStmt(VariableExp("x"), AddExp(VariableExp("x"), VariableExp("y")))
    val s4 = AssignmentStmt(VariableExp("z"), AddExp(VariableExp("x"), ConstExp(1)))
    val s5 = AssignmentStmt(VariableExp("z"), VariableExp("y"))
    val seq =
      SequenceStmt(s1, SequenceStmt(s2, SequenceStmt(s3, SequenceStmt(s4, s5))))

    val RD = ReachingDefinitionMFP.run(seq)

    assert(
      RD(s1) == (
        Set(),
        Set(s1)
      )
    )

    assert(
      RD(s2) == (
        Set(s1),
        Set(s1, s2)
      )
    )

    assert(
      RD(s3) == (
        Set(s1, s2),
        Set(s2, s3)
      )
    )

    assert(
      RD(s4) == (
        Set(s2, s3),
        Set(s2, s3, s4)
      )
    )

    assert(
      RD(s5) == (
        Set(s2, s3, s4),
        Set(s2, s3, s5)
      )
    )
  }

  test("test_rd_using_if") {
    val s1 = AssignmentStmt(VariableExp("x"), ConstExp(1))
    val s2 = AssignmentStmt(VariableExp("y"), ConstExp(2))
    val s3 = AssignmentStmt(VariableExp("z"), ConstExp(3))
    val s4 = IfElseStmt(
      EqExp(ConstExp(1), ConstExp(2)),
      SequenceStmt(s1, s2),
      Some(s3)
    )
    val s5 = AssignmentStmt(VariableExp("y"), ConstExp(0))
    val seq = SequenceStmt(s4, s5)

    val RD = ReachingDefinitionMFP.run(seq)

    assert(
      RD(s1) == (
        Set(),
        Set(s1)
      )
    )

    assert(
      RD(s2) == (
        Set(s1),
        Set(s1, s2)
      )
    )

    assert(
      RD(s3) == (
        Set(),
        Set(s3)
      )
    )

    assert(
      RD(s4) == (
        Set(),
        Set()
      )
    )

    assert(
      RD(s5) == (
        Set(s1, s2, s3),
        Set(s1, s3, s5)
      )
    )
  }

  /** f = 1; while (n>0) { f = f*n; n = n-1; }
    */
  test("test_rd_using_while") {
    val s1 = AssignmentStmt(VariableExp("f"), ConstExp(1))
    val s2 = AssignmentStmt(VariableExp("f"), MultiExp(VariableExp("f"), VariableExp("n")))
    val s3 = AssignmentStmt(VariableExp("n"), SubExp(VariableExp("n"), ConstExp(1)))
    val whileBody = SequenceStmt(s2, s3)
    val s4 = WhileStmt(GTExp(VariableExp("n"), ConstExp(0)), whileBody)
    val seq = SequenceStmt(s1, s4)

    val RD = ReachingDefinitionMFP.run(seq)

    assert(
      RD(s1) == (
        Set(),
        Set(s1)
      )
    )

    assert(
      RD(s4) == (
        Set(s1, s2, s3),
        Set(s1, s2, s3)
      )
    )

    assert(
      RD(s2) == (
        Set(s1, s2, s3),
        Set(s2, s3)
      )
    )

    assert(
      RD(s3) == (
        Set(s2, s3),
        Set(s2, s3)
      )
    )
  }
}
