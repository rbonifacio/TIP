package br.unb.cic.tip.intraprocedural.pointer

import br.unb.cic.tip.blocks
import br.unb.cic.tip.pointer.BasicAndersen
import br.unb.cic.tip.utils.{AllocExp, ConstExp, FunDecl, LocationExp, NullExp, PointerExp, VariableExp}
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
  test("test_stmt_for_point") {

    val s1 = AssignmentStmt(VariableExp("a"), ConstExp(1))
    val s2 = AssignmentStmt(PointerExp("b"), AllocExp(NullExp))
    val s3 = AssignmentStmt(PointerExp("c"), LocationExp("b"))
//    val s4 = AssignmentStmt(VariableExp("d"), VariableExp("b"))
//    val s5 = AssignmentPointerStmt(VariableExp("e"), LoadExp(PointerExp("c")))
//    val s6 = AssignmentPointerStmt(LoadExp(PointerExp("f")), VariableExp("e"))
//    val s7 = AssignmentPointerStmt(VariableExp("g"), NullExp)

//    val mainBody = SequenceStmt(s1, SequenceStmt(s2, SequenceStmt(s3, SequenceStmt(s4, SequenceStmt(s5, SequenceStmt(s6, s7))))))
    val mainBody = SequenceStmt(s1, SequenceStmt(s2, s3))

    val RD = BasicAndersen.pointTo(mainBody)

    println(RD)

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

//    assert(RD(VariableExp("d")) == (
//      Set(AllocExp(NullExp))
//      )
//    )
//
//    assert(RD(s5.name.asInstanceOf[VariableExp]) == (
//      Set(AllocExp(NullExp))
//      )
//    )
//
//    assert(RD(VariableExp("f")) == (
//      Set()
//      )
//    )

//    assert(RD(s7.name.asInstanceOf[VariableExp]) == (
//      Set()
//      )
//    )
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
//  test("test_sample_for_point") {
//
//    val s1 = AssignmentPointerStmt(VariableExp("p"), AllocExp(NullExp))
//    val s2 = AssignmentPointerStmt(VariableExp("x"), PointerExp("y"))
//    val s3 = AssignmentPointerStmt(VariableExp("x"), PointerExp("z"))
//    val s4 = AssignmentPointerStmt(LoadExp(PointerExp("p")), VariableExp("z"))
//    val s5 = AssignmentPointerStmt(VariableExp("p"), PointerExp("q"))
//    val s6 = AssignmentPointerStmt(VariableExp("q"), LocationExp("y"))
//    val s7 = AssignmentPointerStmt(VariableExp("x"), LoadExp(PointerExp("p")))
//    val s8 = AssignmentPointerStmt(VariableExp("p"), LocationExp("z"))
//
//    val mainBody = SequenceStmt(s1, SequenceStmt(s2, SequenceStmt(s3, SequenceStmt(s4, SequenceStmt(s5, SequenceStmt(s6, SequenceStmt(s7, s8)))))))
//
//    val RD = BasicAndersen.pointTo(mainBody)
//
//    assert(RD(VariableExp("p")) == (
//      Set(AllocExp(NullExp), VariableExp("y"), VariableExp("z"))
//      )
//    )
//
//    assert(RD(VariableExp("q")) == (
//      Set(VariableExp("y"))
//      )
//    )
//
//    assert(RD(VariableExp("x")) == (
//      Set()
//      )
//    )
//
//    assert(RD(VariableExp("y")) == (
//      Set()
//      )
//    )
//
//    assert(RD(VariableExp("z")) == (
//      Set()
//      )
//    )
//  }
}
