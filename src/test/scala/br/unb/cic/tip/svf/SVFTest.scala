package br.unb.cic.tip.svf

import br.unb.cic.tip.utils.{AssignmentStmt, FunDecl, SequenceStmt}
import br.unb.cic.tip.utils.Expression.{AddExp, ConstExp, VariableExp, NullExp}
import br.unb.cic.tip.utils.Node.SimpleNode
import org.scalatest.funsuite.AnyFunSuite

class SVFTest extends AnyFunSuite {

  /**
   * will it be solved by reaching definition?
   * s1: a = 1
   * s2: b = 2
   * s3: c = a
//   * s4: d = b + c
   */
  test("test_without_pointer") {
    val s1 = AssignmentStmt("a", ConstExp(1))
    val s2 = AssignmentStmt("b", ConstExp(2))
    val s3 = AssignmentStmt("c", VariableExp("b"))
//    val s4 = AssignmentStmt("d", AddExp(VariableExp("b"), VariableExp("c")))

    //main function
//    val mainBody = SequenceStmt(s1, SequenceStmt(s2, SequenceStmt(s3, s4)))
    val mainBody = SequenceStmt(s1, SequenceStmt(s2, s3))
    val mainFunction = FunDecl("main", List(), List(), mainBody, NullExp)

    val program = List(mainFunction)

    val svf = SVF.run(program)

    val expected = Set(
        (VariableExp("b"), VariableExp("c"))
    )
    assert(expected == svf)
  }

}
