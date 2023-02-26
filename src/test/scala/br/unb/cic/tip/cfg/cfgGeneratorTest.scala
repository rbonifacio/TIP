package br.unb.cic.tip.cfg

import br.unb.cic.tip._
import org.scalatest.funsuite.AnyFunSuite

class cfgGeneratorTest extends AnyFunSuite {

    test("Test simple CFG with only statements") {
      val s1 = AssignmentStmt("x", ConstExp(1), 1)
      val s2 = AssignmentStmt("y", ConstExp(1), 2)
      val s3 = AssignmentStmt("z", AddExp(VariableExp("x"),VariableExp("y")), 3)
      val s4 = ReturnStmt(VariableExp("z"), 4)

      val program = List(s1, s2, s3, s4)

      val cfg = CFGBuilder.generate(program)

      val pairs = CFGDrawer.pairs(cfg)

      val expected: List[(Label, Label)] =
        List(
          (1, 2),
          (2, 3)
        )

      assert(expected == pairs)
    }

}
