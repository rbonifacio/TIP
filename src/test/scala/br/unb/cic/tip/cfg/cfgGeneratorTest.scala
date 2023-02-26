package br.unb.cic.tip.cfg

import br.unb.cic.tip._
import org.scalatest.funsuite.AnyFunSuite

class cfgGeneratorTest extends AnyFunSuite {

    test("Test simple CFG with only statements") {
      val s1 = AssignmentStmt("x", ConstExp(1), 1)
      val s2 = AssignmentStmt("y", ConstExp(1), 2)
      val s3 = AssignmentStmt("z", AddExp(VariableExp("x"),VariableExp("y")), 3)
      val s4 = ReturnStmt(VariableExp("z"), 4)

      val body = SequenceStmt(s1, SequenceStmt(s2, SequenceStmt(s3, s4)))
      val function = FunDecl("sum", List(), List(), body, VariableExp("z"))
      val cfg = CFGBuilder.generate(function)

      val pairs = CFGDrawer.pairs(cfg)

      val expected: Set[(Label, Label)] =
        Set(
          (1, 2),
          (2, 3),
          (3, 4)
        )

      assert(pairs == expected )
    }

  test("Test simple CFG with if condition") {
    val s1 = AssignmentStmt("x", ConstExp(1), 1)
    val s2IfExpression = EqExp(VariableExp("x"),ConstExp(0))
    val s2IfStmt = AssignmentStmt("z", AddExp(VariableExp("x"),ConstExp(2)), 3)
    val s2 = IfStmt(s2IfExpression, s2IfStmt, 2)
    val s3 = ReturnStmt(VariableExp("z"), 4)

    val body = SequenceStmt(s1, SequenceStmt(s2, s3))
    val function = FunDecl("sumIf", List(), List(), body, VariableExp("z"))
    val cfg = CFGBuilder.generate(function)

    val pairs = CFGDrawer.pairs(cfg)

    val expected: Set[(Label, Label)] =
      Set(
        (1, 2),
        (2, 3),
        (2, 4),
        (3, 4),
      )

    assert(pairs == expected )
  }

}
