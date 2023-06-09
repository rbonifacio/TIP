package br.unb.cic.tip.svf

import br.unb.cic.tip.utils.{AddExp, AllocExp, ConstExp, FunDecl, LoadExp, LocationExp, NullExp, PointerExp, VariableExp}
import br.unb.cic.tip.{convertSVFtoGraph, exportDot}
import br.unb.cic.tip.utils.Node.SimpleNode
import br.unb.cic.tip.utils.Stmt.*
import org.scalatest.funsuite.AnyFunSuite

class SVFTest extends AnyFunSuite {

  /**
   * s1: a = 1
   * s2: b = 2
   * s3: c = a
   * s4: d = b + c
   * s5: e = c
   */
  test("test_svf_rule_copy_variables") {
    val s1 = AssignmentStmt(VariableExp("a"), ConstExp(1))
    val s2 = AssignmentStmt(VariableExp("b"), ConstExp(2))
    val s3 = AssignmentStmt(VariableExp("c"), VariableExp("a"))
    val s4 = AssignmentStmt(VariableExp("d"), AddExp(VariableExp("b"), VariableExp("c")))
    val s5 = AssignmentStmt(VariableExp("e"), VariableExp("c"))

    val mainBody = SequenceStmt(s1, SequenceStmt(s2, SequenceStmt(s3, SequenceStmt(s4, s5))))
    val mainFunction = FunDecl("main", List(), List(), mainBody, NullExp)

    val program = List(mainFunction)

    val svf = SVF.run(program)

    val expected = Set(
      ((s1, VariableExp("a")), (s3, VariableExp("c"))),
      ((s2, VariableExp("b")), (s4, VariableExp("d"))),
      ((s3, VariableExp("c")), (s4, VariableExp("d"))),
      ((s3, VariableExp("c")), (s5, VariableExp("e")))
    )
    assert(svf == expected)
//    println(exportDot(convertSVFtoGraph(svf)))
  }

  /**
   * p = alloc i1
   * q = alloc i2
   * p = q
   */
  test("test_svf_rule_copy_pointers") {
    val s1 = AssignmentStmt(PointerExp("p"), AllocExp(NullExp))
    val s2 = AssignmentStmt(PointerExp("q"), AllocExp(ConstExp(1)))
    val s3 = AssignmentStmt(PointerExp("p"), PointerExp("q"))

    val mainBody = SequenceStmt(s1, SequenceStmt(s2, s3))
    val mainFunction = FunDecl("main", List(), List(), mainBody, NullExp)

    val program = List(mainFunction)

    val svf = SVF.run(program)

    val expected = Set(
      ((s2, PointerExp("q")), (s3, PointerExp("p")))
    )
    assert(svf == expected)
  }

  /**
   * s1: a = 1
   * s2: b = 2
   * s3: p = alloc i1
   * s4: q = alloc i2
   * s5: c = a
   * s6: q = p
   * s7: d = b + c
   * s8: e = c
   *
   */
  test("test_svf_rule_copy") {
    val s1 = AssignmentStmt(VariableExp("a"), ConstExp(1))
    val s2 = AssignmentStmt(VariableExp("b"), ConstExp(2))
    val s3 = AssignmentStmt(PointerExp("p"), AllocExp(ConstExp(1)))
    val s4 = AssignmentStmt(PointerExp("q"), AllocExp(ConstExp(2)))
    val s5 = AssignmentStmt(VariableExp("c"), VariableExp("a"))
    val s6 = AssignmentStmt(PointerExp("q"), PointerExp("p"))
    val s7 = AssignmentStmt(VariableExp("d"), AddExp(VariableExp("b"), VariableExp("c")))
    val s8 = AssignmentStmt(VariableExp("e"), VariableExp("c"))

    val mainBody = SequenceStmt(s1, SequenceStmt(s2, SequenceStmt(s3, SequenceStmt(s4, SequenceStmt(s5, SequenceStmt(s6, SequenceStmt(s7, s8)))))))
    val mainFunction = FunDecl("main", List(), List(), mainBody, NullExp)

    val program = List(mainFunction)

    val svf = SVF.run(program)

    val expected = Set(
      ((s1, VariableExp("a")), (s5, VariableExp("c"))),
      ((s3, PointerExp("p")), (s6, PointerExp("q"))),
      ((s2, VariableExp("b")), (s7, VariableExp("d"))),
      ((s5, VariableExp("c")), (s7, VariableExp("d"))),
      ((s5, VariableExp("c")), (s8, VariableExp("e")))
    )
    assert(svf == expected)
    //    println(exportDot(convertSVFtoGraph(svf)))
  }

  /**
   * s1: p = alloc i1
   * s2: q = alloc i2
   * s3: r = alloc i3
   * s4: p = &r
   * s5: q = *p
   *
   */
  test("test_svf_load_rule_simple") {
    val s1 = AssignmentStmt(PointerExp("p"), AllocExp(ConstExp(1)))
    val s2 = AssignmentStmt(PointerExp("q"), AllocExp(ConstExp(2)))
    val s3 = AssignmentStmt(PointerExp("r"), AllocExp(ConstExp(3)))
    val s4 = AssignmentStmt(PointerExp("p"), LocationExp("r"))
    val s5 = AssignmentStmt(PointerExp("q"), LoadExp(PointerExp("p")))

    val mainBody = SequenceStmt(s1, SequenceStmt(s2, SequenceStmt(s3, SequenceStmt(s4, s5))))
    val mainFunction = FunDecl("main", List(), List(), mainBody, NullExp)

    val program = List(mainFunction)

    val svf = SVF.run(program)

    val expected = Set(
//      ((NopStmt, AllocExp(ConstExp(1))), (s5, PointerExp("q"))),
      ((s3, PointerExp("r")), (s5, PointerExp("q")))
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
   * s7: r = *p
   */
  test("test_svf_load_rule_many_allocations") {
    val s1 = AssignmentStmt(PointerExp("p"), AllocExp(ConstExp(1)))
    val s2 = AssignmentStmt(PointerExp("q"), AllocExp(ConstExp(2)))
    val s3 = AssignmentStmt(PointerExp("r"), AllocExp(ConstExp(3)))
    val s4 = AssignmentStmt(PointerExp("s"), AllocExp(ConstExp(4)))
    val s5 = AssignmentStmt(PointerExp("p"), LocationExp("q"))
    val s6 = AssignmentStmt(PointerExp("p"), LocationExp("s"))
    val s7 = AssignmentStmt(PointerExp("r"), LoadExp(PointerExp("p")))

    val mainBody = SequenceStmt(s1, SequenceStmt(s2, SequenceStmt(s3, SequenceStmt(s4, SequenceStmt(s5, SequenceStmt(s6, s7))))))
    val mainFunction = FunDecl("main", List(), List(), mainBody, NullExp)

    val program = List(mainFunction)

    val svf = SVF.run(program)

    val expected = Set(
      ((s2, PointerExp("q")), (s7, PointerExp("r"))),
      ((s4, PointerExp("s")), (s7, PointerExp("r")))
    )
    assert(svf == expected)
  }

  /**
   * s1: p = alloc i1
   * s2: q = alloc i2
   * s3: r = alloc i3
   * s4: s = alloc i4
   * s5: p = &r
   * s6: q = *p
   * s7: q = &p
   * s8: q = &r
   * s9: s = *q
   */
  test("test_svf_load_rule") {
    val s1 = AssignmentStmt(PointerExp("p"), AllocExp(ConstExp(1)))
    val s2 = AssignmentStmt(PointerExp("q"), AllocExp(ConstExp(2)))
    val s3 = AssignmentStmt(PointerExp("r"), AllocExp(ConstExp(3)))
    val s4 = AssignmentStmt(PointerExp("s"), AllocExp(ConstExp(4)))
    val s5 = AssignmentStmt(PointerExp("p"), LocationExp("r"))
    val s6 = AssignmentStmt(PointerExp("q"), LoadExp(PointerExp("p")))
    val s7 = AssignmentStmt(PointerExp("q"), LocationExp("p"))
    val s8 = AssignmentStmt(PointerExp("q"), LocationExp("r"))
    val s9 = AssignmentStmt(PointerExp("s"), LoadExp(PointerExp("q")))

    val mainBody = SequenceStmt(s1, SequenceStmt(s2, SequenceStmt(s3, SequenceStmt(s4, SequenceStmt(s5, SequenceStmt(s6, SequenceStmt(s7, SequenceStmt(s8, s9))))))))
    val mainFunction = FunDecl("main", List(), List(), mainBody, NullExp)

    val program = List(mainFunction)

    val svf = SVF.run(program)

    val expected = Set(
      ((s3, PointerExp("r")), (s6, PointerExp("q"))),
      ((s5, PointerExp("p")), (s9, PointerExp("s"))),
      ((s3, PointerExp("r")), (s9, PointerExp("s")))
    )
    assert(svf == expected)
  }

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
}
