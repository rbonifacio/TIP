package br.unb.cic.tip.svf

import br.unb.cic.tip.utils.{AddExp, AllocExp, ConstExp, FunDecl, FunctionCallExp, LoadExp, LocationExp, MultiExp, NameExp, NullExp, PointerExp, VariableExp}
import br.unb.cic.tip.{convertSVFtoGraph, exportDot}
import br.unb.cic.tip.utils.Node.SimpleNode
import br.unb.cic.tip.utils.Stmt.*
import org.scalatest.funsuite.AnyFunSuite

class SVF5RuleReturnTest extends AnyFunSuite {

  /**
   *  s1: a = 1
   *  s2: return a
   */
  test("test_svf_return_rule_in_main") {
    val s1 = AssignmentStmt(VariableExp("a"), ConstExp(1))
    val s2 = ReturnStmt(VariableExp("a"))

    //main function
    val fMainBody = SequenceStmt(s1, s2)

    val fMain = FunDecl("main", List(), List("a"), fMainBody, NullExp)
    val program = List(fMain)
    val svf = SVF.run(program)

    val expected = Set()

    assert(svf == expected)
  }

  /**
   * fx: sign(a) {
   * f1:  y = a * -1
   * f2:  return y
   * fx: }
   *
   * sx: main() {
   * s1:   a = 1
   * s2:   b = sign(a)
   * s3:   print b
   * s4: }
   */
  test("test_svf_return_rule") {

    val f1 = AssignmentStmt(VariableExp("y"), MultiExp(VariableExp("a"), ConstExp(1)))
    val f2 = ReturnStmt(VariableExp("y"))
    val fSignBody = SequenceStmt(f1, f2)
    val fSign = FunDecl("fSign", List("x"), List("a"), fSignBody, VariableExp("y"))

    val s1 = AssignmentStmt(VariableExp("a"), ConstExp(1))
    val s2 = AssignmentStmt(VariableExp("b"), FunctionCallExp(NameExp(fSign.name), List(VariableExp("a"))))
    val s3 = OutputStmt(VariableExp("b"))

    //main function
    val fMainBody = SequenceStmt(s1, SequenceStmt(s2, s3))

    val fMain = FunDecl("main", List(), List("a", "b"), fMainBody, NullExp)

    val program = List(fSign, fMain)

    val svf = SVF.run(program)

    val expected = Set(
      ((s1, VariableExp("a")), (f1, VariableExp("y"))),
      ((f2, VariableExp("y")), (s2, VariableExp("b")))
    )

    assert(svf == expected)
  }
}