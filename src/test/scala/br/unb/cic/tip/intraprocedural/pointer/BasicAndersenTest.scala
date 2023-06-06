package br.unb.cic.tip.intraprocedural.pointer

import br.unb.cic.tip.blocks
import br.unb.cic.tip.pointer.BasicAndersen
import br.unb.cic.tip.utils.{AllocExp, ConstExp, FunDecl, LoadExp, LocationExp, NullExp, PointerExp, VariableExp}
import br.unb.cic.tip.utils.Stmt.{AssignmentStmt, SequenceStmt}
import org.scalatest.funsuite.AnyFunSuite

class BasicAndersenTest extends AnyFunSuite {

  /**
   *  s1: a = 1
   *  s2: b = alloc null
   *  s3: c = &b
   *  s4: d = b
   *  s5: e = *c
   *  s6: *b = d
   *  s7: f = null
   *
   */
  test("test_pt_all_andersen_rules") {

    val s1 = AssignmentStmt(VariableExp("a"), ConstExp(1))
    val s2 = AssignmentStmt(PointerExp("b"), AllocExp(NullExp))
    val s3 = AssignmentStmt(PointerExp("c"), LocationExp("b"))
    val s4 = AssignmentStmt(PointerExp("d"), PointerExp("b"))
    val s5 = AssignmentStmt(PointerExp("e"), LoadExp(PointerExp("c")))
    val s6 = AssignmentStmt(LoadExp(PointerExp("f")), PointerExp("e"))
    val s7 = AssignmentStmt(PointerExp("g"), NullExp)

    val mainBody = SequenceStmt(s1, SequenceStmt(s2, SequenceStmt(s3, SequenceStmt(s4, SequenceStmt(s5, SequenceStmt(s6, s7))))))

    val RD = BasicAndersen.pointTo(mainBody)

    assert(RD(VariableExp("a")) == (
      Set()
      )
    )

    assert(RD(PointerExp("b")) == (
      Set(AllocExp(NullExp))
      )
    )

    assert(RD(PointerExp("c")) == (
      Set(PointerExp("b"))
      )
    )

    assert(RD(PointerExp("d")) == (
      Set(AllocExp(NullExp))
      )
    )

    assert(RD(PointerExp("e")) == (
      Set(AllocExp(NullExp))
      )
    )

    assert(RD(PointerExp("f")) == (
      Set()
      )
    )

    assert(RD(PointerExp("g")) == (
      Set()
      )
    )
  }

  /**
   * s1: p = alloc null
   * s2: x = y
   * s3: x = z
   * s4: *p = z
   * s5: p = q
   * s6: q = &y
   * s7: x = *p
   * s8: p = &z
   *
   */
  test("test_pt_basic_sample_for_book") {

    val s1 = AssignmentStmt(PointerExp("p"), AllocExp(NullExp))
    val s2 = AssignmentStmt(PointerExp("x"), PointerExp("y"))
    val s3 = AssignmentStmt(PointerExp("x"), PointerExp("z"))
    val s4 = AssignmentStmt(LoadExp(PointerExp("p")), PointerExp("z"))
    val s5 = AssignmentStmt(PointerExp("p"), PointerExp("q"))
    val s6 = AssignmentStmt(PointerExp("q"), LocationExp("y"))
    val s7 = AssignmentStmt(PointerExp("x"), LoadExp(PointerExp("p")))
    val s8 = AssignmentStmt(PointerExp("p"), LocationExp("z"))

    val mainBody = SequenceStmt(s1, SequenceStmt(s2, SequenceStmt(s3, SequenceStmt(s4, SequenceStmt(s5, SequenceStmt(s6, SequenceStmt(s7, s8)))))))

    val RD = BasicAndersen.pointTo(mainBody)

    assert(RD(PointerExp("p")) == (
      Set(AllocExp(NullExp), PointerExp("y"), PointerExp("z"))
      )
    )

    assert(RD(PointerExp("q")) == (
      Set(PointerExp("y"))
      )
    )

    assert(RD(PointerExp("x")) == (
      Set()
      )
    )

    assert(RD(PointerExp("y")) == (
      Set()
      )
    )

    assert(RD(PointerExp("z")) == (
      Set()
      )
    )
  }
}
