package br.unb.cic.tip.svf

import br.unb.cic.tip.{convertSVFtoGraph, exportDot}
import br.unb.cic.tip.utils.{AssignmentPointerStmt, AssignmentStmt, FunDecl, SequenceStmt}
import br.unb.cic.tip.utils.Expression.{AddExp, AllocExp, ConstExp, NullExp, PointerExp, VariableExp}
import br.unb.cic.tip.utils.Node.SimpleNode
import org.scalatest.funsuite.AnyFunSuite

class SVFTest extends AnyFunSuite {

  /**
   * will it be solved by reaching definition?
   * s1: a = 1
   * s2: b = 2
   * s3: c = a
   * s4: d = b + c
   * s5: e = c
   */
  test("test_without_pointer") {
    val s1 = AssignmentStmt("a", ConstExp(1))
    val s2 = AssignmentStmt("b", ConstExp(2))
    val s3 = AssignmentStmt("c", VariableExp("a"))
    val s4 = AssignmentStmt("d", AddExp(VariableExp("b"), VariableExp("c")))
    val s5 = AssignmentStmt("e", VariableExp("c"))

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
    assert(expected == svf)

//    println(exportDot(convertSVFtoGraph(svf)))
  }

  /**
   * p = alloc i1
   * q = alloc i2
   * p = q
   */
  test("test_simple_rule_copy") {
    val s1 = AssignmentPointerStmt(PointerExp("p"), AllocExp(NullExp))
    val s2 = AssignmentPointerStmt(PointerExp("q"), AllocExp(ConstExp(1)))
    val s3 = AssignmentPointerStmt(PointerExp("p"), PointerExp("q"))

    val mainBody = SequenceStmt(s1, SequenceStmt(s2, s3))
    val mainFunction = FunDecl("main", List(), List(), mainBody, NullExp)

    val program = List(mainFunction)

    val svf = SVF.run(program)

    val expected = Set(
      ((s1, PointerExp("q")), (s3, PointerExp("p")))
    )
    assert(expected == svf)

//    println(exportDot(convertSVFtoGraph(svf)))
  }

}
