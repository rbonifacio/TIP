package br.unb.cic.tip.intraprocedural.df

import br.unb.cic.tip.*
import br.unb.cic.tip.utils.*
import br.unb.cic.tip.utils.Node.*
import br.unb.cic.tip.utils.Stmt.*
import br.unb.cic.tip.df.AvailableExpressions
import org.scalatest.funsuite.AnyFunSuite

class AETest extends AnyFunSuite {

  /**
   * y = x               {}
   * x = 5 + 2           {5 + 2}
   * y = 1 + y           {5 + 2}
   * z = y + x           {5 + 2, y + x}
   * x = 3               {5 + 2}
   */
  test("test_ae_stmts") {

    val s1 = AssignmentStmt(VariableExp("y"), VariableExp("x"))
    val s2 = AssignmentStmt(VariableExp("x"), AddExp(ConstExp(5), ConstExp(2)))
    val s3 = AssignmentStmt(VariableExp("y"), AddExp(ConstExp(1), VariableExp("y")))
    val s4 = AssignmentStmt(VariableExp("z"), AddExp(VariableExp("y"), VariableExp("x")))
    val s5 = AssignmentStmt(VariableExp("x"), ConstExp(3))

    val seq = SequenceStmt(s1, SequenceStmt(s2, SequenceStmt(s3, SequenceStmt(s4, s5))))

    val AE = AvailableExpressions.run(seq)

    assert(AE(s1) == (
      Set(),
      Set()
    ))

    assert(AE(s2) == (
      Set(),
      Set(AddExp(ConstExp(5), ConstExp(2)))
    ))

    assert(AE(s3) == (
      Set(AddExp(ConstExp(5), ConstExp(2))),
      Set(AddExp(ConstExp(5), ConstExp(2)))
    ))

    assert(AE(s4) == (
      Set(AddExp(ConstExp(5), ConstExp(2))),
      Set(AddExp(ConstExp(5), ConstExp(2)), AddExp(VariableExp("y"), VariableExp("x")))
    ))

    assert(AE(s5) == (
      Set(AddExp(ConstExp(5), ConstExp(2)), AddExp(VariableExp("y"), VariableExp("x"))),
      Set(AddExp(ConstExp(5), ConstExp(2)))
    ))
  }

  /**
      z = a + b;              {a + b}
      y = a * b;              {a + b, a * b}
      while (y > a + b) {     {y > a + b, a + b}
        a = a + 1;            {}
        x = a + b;            {a + b}
      }
   */
  test("test_ae_example_from_SPA_p.67") {
    val s1 = AssignmentStmt(VariableExp("z"), AddExp(VariableExp("a"), VariableExp("b")))
    val s2 = AssignmentStmt(VariableExp("y"), MultiExp(VariableExp("a"), VariableExp("b")))
    val s4 = AssignmentStmt(VariableExp("a"), AddExp(VariableExp("a"), ConstExp(1)))
    val s5 = AssignmentStmt(VariableExp("x"), AddExp(VariableExp("a"), VariableExp("b")))
    val s3 = WhileStmt(
      GTExp(VariableExp("y"), AddExp(VariableExp("a"), VariableExp("b"))),
      SequenceStmt(s4, s5)
    )
    val seq = SequenceStmt(s1, SequenceStmt(s2, s3))

    val AE = AvailableExpressions.run(seq)

    assert(
      AE(s1) == (
        Set(),
        Set(AddExp(VariableExp("a"), VariableExp("b"))))
    )

    assert(
      AE(s2) == (
        Set(AddExp(VariableExp("a"), VariableExp("b"))),
        Set(AddExp(VariableExp("a"), VariableExp("b")), MultiExp(VariableExp("a"), VariableExp("b")))
      )
    )

    assert(
      AE(s3) == (
        Set(AddExp(VariableExp("a"), VariableExp("b"))),
        Set(GTExp(VariableExp("y"), AddExp(VariableExp("a"), VariableExp("b"))), AddExp(VariableExp("a"), VariableExp("b")))
      )
    )

//    assert(
//      AE(s4) == (
//        Set(GTExp(VariableExp("y"), AddExp(VariableExp("a"), VariableExp("b"))), AddExp(VariableExp("a"), VariableExp("b"))),
//        Set()
//      )
//    )

//    assert(
//      AE(s5) == (
//        Set(),
//        Set(AddExp(VariableExp("a"), VariableExp("b")))
//      ),
//    )
  }
  
  ignore("repeated_statements") {
    /*  y = x + 3           {},       {x + 3}
        z = 8               {x + 3},  {x + 3}
        y = x + 3           {x + 3},  {x + 3}
     */

    val s1 = AssignmentStmt(VariableExp("y"), AddExp(VariableExp("x"), ConstExp(3)))
    val s2 = AssignmentStmt(VariableExp("z"), ConstExp(8))
    val s3 = AssignmentStmt(VariableExp("y"), AddExp(VariableExp("x"), ConstExp(3)))

    val seq = SequenceStmt(s1, SequenceStmt(s2, s3))

    val AE = AvailableExpressions.run(seq)

    assert(AE(s1) == (Set(), Set(AddExp(VariableExp("x"), ConstExp(3)))))
    assert(
      AE(s2) == (
        Set(AddExp(VariableExp("x"), ConstExp(3))),
        Set(AddExp(VariableExp("x"), ConstExp(3)))
      )
    )
    assert(
      AE(s3) == (
        Set(AddExp(VariableExp("x"), ConstExp(3))),
        Set(AddExp(VariableExp("x"), ConstExp(3)))
      )
    )
  }
}
