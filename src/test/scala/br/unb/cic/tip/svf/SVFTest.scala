package br.unb.cic.tip.svf

import br.unb.cic.tip.{convertSVFtoGraph, exportDot}
import br.unb.cic.tip.utils.{AssignmentStmt, FunDecl, SequenceStmt}
import br.unb.cic.tip.utils.Expression.{AddExp, ConstExp, NullExp, VariableExp}
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
      (VariableExp("a"), VariableExp("c")),
      (VariableExp("b"), VariableExp("d")),
      (VariableExp("c"), VariableExp("d")),
      (VariableExp("c"), VariableExp("e"))
    )
    assert(expected == svf)

    println(exportDot(convertSVFtoGraph(svf)))
  }
}
