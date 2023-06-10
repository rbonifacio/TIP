package br.unb.cic.tip.svf

import br.unb.cic.tip.utils.Node.SimpleNode
import br.unb.cic.tip.utils.Stmt.*
import br.unb.cic.tip.utils.*
import br.unb.cic.tip.{convertSVFtoGraph, exportDot}
import org.scalatest.funsuite.AnyFunSuite

class SVFRuleStoreTest extends AnyFunSuite {
  
  /**
   * s1: p = alloc i1
   * s2: q = alloc i2
   * s3: r = alloc i3
   * s4: p = &r
   * s5: *p = q
   *
   */
  test("test_svf_store_rule_simple") {
    val s1 = AssignmentStmt(PointerExp("p"), AllocExp(ConstExp(1)))
    val s2 = AssignmentStmt(PointerExp("q"), AllocExp(ConstExp(2)))
    val s3 = AssignmentStmt(PointerExp("r"), AllocExp(ConstExp(3)))
    val s4 = AssignmentStmt(PointerExp("p"), LocationExp("r"))
    val s5 = AssignmentStmt(LoadExp(PointerExp("p")), PointerExp("q"))

    val mainBody = SequenceStmt(s1, SequenceStmt(s2, SequenceStmt(s3, SequenceStmt(s4, s5))))
    val mainFunction = FunDecl("main", List(), List(), mainBody, NullExp)

    val program = List(mainFunction)

    val svf = SVF.run(program)

    val expected = Set(
      ((s2, PointerExp("q")), (s5, PointerExp("r")))
    )
    assert(svf == expected)
  }

  /**
   * s1: p = alloc i1
   * s2: q = alloc i2
   * s3: r = alloc i3
   * s4: s = alloc i4
   * s5: p = &q
   * s6: p = &s
   * s7: *p = r
   */
  test("test_svf_store_rule_many_allocations") {
    val s1 = AssignmentStmt(PointerExp("p"), AllocExp(ConstExp(1)))
    val s2 = AssignmentStmt(PointerExp("q"), AllocExp(ConstExp(2)))
    val s3 = AssignmentStmt(PointerExp("r"), AllocExp(ConstExp(3)))
    val s4 = AssignmentStmt(PointerExp("s"), AllocExp(ConstExp(4)))
    val s5 = AssignmentStmt(PointerExp("p"), LocationExp("q"))
    val s6 = AssignmentStmt(PointerExp("p"), LocationExp("s"))
    val s7 = AssignmentStmt(LoadExp(PointerExp("p")), PointerExp("r"))

    val mainBody = SequenceStmt(s1, SequenceStmt(s2, SequenceStmt(s3, SequenceStmt(s4, SequenceStmt(s5, SequenceStmt(s6, s7))))))
    val mainFunction = FunDecl("main", List(), List(), mainBody, NullExp)

    val program = List(mainFunction)

    val svf = SVF.run(program)

    val expected = Set(
      ((s3, PointerExp("r")), (s7, PointerExp("q"))),
      ((s3, PointerExp("r")), (s7, PointerExp("s")))
    )
    assert(svf == expected)
  }

  /**
   * s1: p = alloc i1
   * s2: q = alloc i2
   * s3: r = alloc i3
   * s4: s = alloc i4
   * s5: r = s
   * s6: p = &r
   * s7: *p = q
   *
   */
  test("test_svf_store_rule_weak_and_strong_update") {
    val s1 = AssignmentStmt(PointerExp("p"), AllocExp(ConstExp(1)))
    val s2 = AssignmentStmt(PointerExp("q"), AllocExp(ConstExp(2)))
    val s3 = AssignmentStmt(PointerExp("r"), AllocExp(ConstExp(3)))
    val s4 = AssignmentStmt(PointerExp("s"), AllocExp(ConstExp(4)))
    val s5 = AssignmentStmt(PointerExp("r"), PointerExp("s"))
    val s6 = AssignmentStmt(PointerExp("p"), LocationExp("r"))
    val s7 = AssignmentStmt(LoadExp(PointerExp("p")), PointerExp("q"))

    val mainBody = SequenceStmt(s1, SequenceStmt(s2, SequenceStmt(s3, SequenceStmt(s4, SequenceStmt(s5, SequenceStmt(s6, s7))))))
    val mainFunction = FunDecl("main", List(), List(), mainBody, NullExp)

    val program = List(mainFunction)

    val svf = SVF.run(program)

    val expected = Set(
      ((s4, PointerExp("s")), (s5, PointerExp("r"))),
      ((s2, PointerExp("q")), (s7, PointerExp("r")))
    )
    assert(svf == expected)
  }
}
