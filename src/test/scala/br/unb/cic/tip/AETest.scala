package br.unb.cic.tip

import br.unb.cic.tip.*
import br.unb.cic.tip.Expression.*
import br.unb.cic.tip.Node.*
import br.unb.cic.tip.Stmt.*
import org.scalatest.funsuite.AnyFunSuite

class AETest extends AnyFunSuite {

  test("example from Static Program Analysis p.67") {
    /*  z = a + b;              {a + b}
        y = a * b;              {a + b, a * b}
        while (y > a + b) {     {y > a + b, a + b}
          a = a + 1;            {}
          x = a + b;            {a + b}
        }
     */

    val s1 = AssignmentStmt("z", AddExp(VariableExp("a"), VariableExp("b")))
    val s2 = AssignmentStmt("y", MultiExp(VariableExp("a"), VariableExp("b")))

    val whileS1 = AssignmentStmt("a", AddExp(VariableExp("a"), ConstExp(1)))
    val whileS2 =
      AssignmentStmt("x", AddExp(VariableExp("a"), VariableExp("b")))
    val s3 = WhileStmt(
      GTExp(VariableExp("y"), AddExp(VariableExp("a"), VariableExp("b"))),
      SequenceStmt(whileS1, whileS2)
    )
    val seq = SequenceStmt(s1, SequenceStmt(s2, s3))

    val AE = AvailableExpressions.run(seq)

    assert(AE(s1) == Set(AddExp(VariableExp("a"), VariableExp("b"))))
    assert(
      AE(s2) == Set(
        AddExp(VariableExp("a"), VariableExp("b")),
        MultiExp(VariableExp("a"), VariableExp("b"))
      )
    )
    assert(
      AE(s3) == Set(
        GTExp(VariableExp("y"), AddExp(VariableExp("a"), VariableExp("b"))),
        AddExp(VariableExp("a"), VariableExp("b"))
      )
    )
    assert(AE(whileS1) == Set())
    assert(AE(whileS2) == Set(AddExp(VariableExp("a"), VariableExp("b"))))
  }

  test("simple statements") {
    /*  y = x               {}
        x = 5 + 2           {5 + 2}
        y = 1 + y           {5 + 2}
        z = y + x           {5 + 2, y + x}
        x = 3               {5 + 2}
     */

    val s1 = AssignmentStmt("y", VariableExp("x"))
    val s2 = AssignmentStmt("x", AddExp(ConstExp(5), ConstExp(2)))
    val s3 = AssignmentStmt("y", AddExp(ConstExp(1), VariableExp("y")))
    val s4 = AssignmentStmt("z", AddExp(VariableExp("y"), VariableExp("x")))
    val s5 = AssignmentStmt("x", ConstExp(3))

    val seq =
      SequenceStmt(s1, SequenceStmt(s2, SequenceStmt(s3, SequenceStmt(s4, s5))))

    val AE = AvailableExpressions.run(seq)

    assert(AE(s1) == Set())
    assert(AE(s2) == Set(AddExp(ConstExp(5), ConstExp(2))))
    assert(AE(s3) == Set(AddExp(ConstExp(5), ConstExp(2))))
    assert(
      AE(s4) == Set(
        AddExp(ConstExp(5), ConstExp(2)),
        AddExp(VariableExp("y"), VariableExp("x"))
      )
    )
    assert(AE(s5) == Set(AddExp(ConstExp(5), ConstExp(2))))
  }
}
