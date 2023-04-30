package br.unb.cic.tip.intraprocedural.df

import br.unb.cic.tip.*
import br.unb.cic.tip.utils.Expression.*
import br.unb.cic.tip.utils.Node.*
import br.unb.cic.tip.utils._
import br.unb.cic.tip.df.LiveVariable
import org.scalatest.funsuite.AnyFunSuite

class LVTest extends AnyFunSuite {

  /**
   * x = a; Entry: {a, b}  Exit: {b}
   * y = b; Entry: {b}     Exit: { }
   * x = 3; Entry: { }     Exit: { }
   */
  test("test_lv_with_only_statements") {

    val s1 = AssignmentStmt("x", VariableExp("a"))
    val s2 = AssignmentStmt("y", VariableExp("b"))
    val s3 = AssignmentStmt("x", ConstExp(3))

    val seq = SequenceStmt(s1, SequenceStmt(s2, s3))
    val LV =  LiveVariable.run(seq)

    assert(LV(s1) == (
      Set(VariableExp("a"), VariableExp("b")),
      Set(VariableExp("b"))
    ))

    assert(LV(s2) == (
      Set(VariableExp("b")),
      Set()
    ))

    assert(LV(s3) == (
      Set(),
      Set()
    ))
  }

  /**
   * x = a;     Entry: {a, b}   Exit: {b,x}
   * y = b+1;   Entry: {b, x}   Exit: {x}
   * output x;  Entry: {x}      Exit: { }
   */
  test("test_rd_with_output") {

    val s1 = AssignmentStmt("x", VariableExp("a"))
    val s2 = AssignmentStmt("y", AddExp(VariableExp("b"), ConstExp(1)))
    val s3 = OutputStmt(VariableExp("x"))

    val seq = SequenceStmt(s1, SequenceStmt(s2, s3))
    val LV =  LiveVariable.run(seq)

    assert(LV(s1) == (
      Set(VariableExp("a"), VariableExp("b")),
      Set(VariableExp("b"), VariableExp("x"))
    ))

    assert(LV(s2) == (
      Set(VariableExp("b"), VariableExp("x")),
      Set(VariableExp("x"))
    ))

    assert(LV(s3) == (
      Set(VariableExp("x")),
      Set()
    ))
  }

  /**
   * if (x < y) {   Entry: {a, x, y}  Exit: {a}
   *    x = a;      Entry: {a}        Exit: { }
   * }
   * y = 1;         Entry: { }        Exit: { }
   */
  test("test_lv_with_if") {

    val s2 = AssignmentStmt("x", VariableExp("a"))
    val s1 = IfElseStmt(GTExp(VariableExp("x"), VariableExp("y")), s2, None)
    val s3 = AssignmentStmt("y", ConstExp(1))
    val seq = SequenceStmt(s1, s3)
    val LV = LiveVariable.run(seq)

    assert(LV(s1) == (
      Set(VariableExp("a"), VariableExp("x"),VariableExp("y")),
      Set(VariableExp("a"))
    ))

    assert(LV(s2) == (
      Set(VariableExp("a")),
      Set()
    ))

    assert(LV(s3) == (
      Set(),
      Set()
    ))
  }

  /**
   * x = 2;         Entry: { }      Exit: {}
   * y = 4;         Entry: { }      Exit: {y}
   * x = 1;         Entry: {y}      Exit: {x, y}
   * if (y < x) {   Entry: {x, y}   Exit: {y}
   *  z = y;        Entry: {y}      Exit: {z}
   * }
   * else {
   *  z = y * y;    Entry: {y}      Exit: {z}
   * }
   * x = z;         Entry: {z}      Exit: {}
   */
  test("test_lv_using_if_else") {

    val s1 = AssignmentStmt("x", ConstExp(2))
    val s2 = AssignmentStmt("y", ConstExp(4))
    val s3 = AssignmentStmt("x", ConstExp(1))

    val s5 = AssignmentStmt("z", VariableExp("y"))
    val s6 = AssignmentStmt("z", MultiExp(VariableExp("y"), VariableExp("y")))
    val s4 = IfElseStmt(GTExp(VariableExp("y"), VariableExp("x")), s5, Option(s6))

    val s7 = AssignmentStmt("x", VariableExp("z"))

    val seq = SequenceStmt(s1,
      SequenceStmt(s2,
        SequenceStmt(s3,
          SequenceStmt(s4, s7))))

    val LV = LiveVariable.run(seq)

    assert(LV(s1) == (
      Set(),
      Set()
    ))

    assert(LV(s2) == (
      Set(),
      Set(VariableExp("y"))
    ))

    assert(LV(s3) == (
      Set(VariableExp("y")),
      Set(VariableExp("x"), VariableExp("y"))
    ))

    assert(LV(s4) == (
      Set(VariableExp("x"), VariableExp("y")),
      Set(VariableExp("y"))
    ))

    assert(LV(s5) == (
      Set(VariableExp("y")),
      Set(VariableExp("z"))
    ))

    assert(LV(s6) == (
      Set(VariableExp("y")),
      Set(VariableExp("z"))
    ))

    assert(LV(s7) == (
      Set(VariableExp("z")),
      Set()
    ))
  }

  /**
   * while(x < 10) {    Entry: {x}    Exit: {x}
   *  x = x+1;          Entry: {x}    Exit: {x}
   * }
   * output x;          Entry: {x}    Exit: { }
   */
  test("test_lv_using_while") {

    val s2 = AssignmentStmt("x", AddExp(VariableExp("x"), ConstExp(1)))
    val s1 = WhileStmt(GTExp(VariableExp("x"), ConstExp(10)), s2)
    val s3 = OutputStmt(VariableExp("x"))
    val seq = SequenceStmt(s1, s3)

    val LV = LiveVariable.run(seq)

    assert(LV(s1) == (
      Set(VariableExp("x")),
      Set(VariableExp("x"))
    ))

    assert(LV(s2) == (
      Set(VariableExp("x")),
      Set(VariableExp("x"))
    ))

    assert(LV(s3) == (
      Set(VariableExp("x")),
      Set()
    ))
  }
}