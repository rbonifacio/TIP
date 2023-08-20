package br.unb.cic.tip.svf

import br.unb.cic.tip.utils.*
import br.unb.cic.tip.utils.Node.SimpleNode
import br.unb.cic.tip.utils.Stmt.*
import br.unb.cic.tip.{convertSVFtoGraph, exportDot}
import org.scalatest.funsuite.AnyFunSuite

class SVFSamplesTest extends AnyFunSuite {

  /**
   * sx: main() {
   * s1:  a = 1
   * s2:  b = 2
   * s3:  c = a
   * s4:  d = c + b
   * s5:  print d
   * sx: }
   */
  test("test_case_study_1") {
    val s1 = AssignmentStmt(VariableExp("a"), ConstExp(1))
    val s2 = AssignmentStmt(VariableExp("b"), ConstExp(2))
    val s3 = AssignmentStmt(VariableExp("c"), VariableExp("a"))
    val s4 = AssignmentStmt(VariableExp("d"), AddExp(VariableExp("c"), VariableExp("b")))
    val s5 = OutputStmt(VariableExp("d"))

    val mainBody = SequenceStmt(s1, SequenceStmt(s2, SequenceStmt(s3, SequenceStmt(s4, s5))))
    val mainFunction = FunDecl("main", List(), List(), mainBody, NullExp)

    val program = List(mainFunction)

    val svf = SVF.run(program)

    val expected = Set(
      ((s1, VariableExp("a")), (s3, VariableExp("c"))),
      ((s3, VariableExp("c")), (s4, VariableExp("d"))),
      ((s2, VariableExp("b")), (s4, VariableExp("d"))),
//      ((s4, VariableExp("d")), (s5, VariableExp("d")))
    )
    assert(svf == expected)
//    println(exportDot(convertSVFtoGraph(svf)))
  }

  /**
   * mx: identity(x) {
   * m1:  return x
   * mx: }
   *
   * sx: main() {
   * s1:  a = 1
   * s2:  b = 2
   * s3:  c = identity(a)
   * s4:  d = c + b
   * s5:  print d
   * sx: }
   */
  test("test_case_study_2") {

    val f1 = ReturnStmt(VariableExp("x"))
    val fIdentityBody = f1
    val fIdentity = FunDecl("fIdentity", List(VariableExp("x")), List(), fIdentityBody, VariableExp("x"))

    val s1 = AssignmentStmt(VariableExp("a"), ConstExp(1))
    val s2 = AssignmentStmt(VariableExp("b"), ConstExp(2))
    val s3 = AssignmentStmt(VariableExp("c"), FunctionCallExp(fIdentity.name, List(VariableExp("a"))))
    val s4 = AssignmentStmt(VariableExp("d"), AddExp(VariableExp("c"), VariableExp("b")))
    val s5 = OutputStmt(VariableExp("d"))

    //main function
    val fMainBody = SequenceStmt(s1, SequenceStmt(s2, SequenceStmt(s3, SequenceStmt(s4, s5))))

    val fMain = FunDecl("main", List(), List(), fMainBody, NullExp)

    val program = List(fIdentity, fMain)

    val svf = SVF.run(program)

    val expected = Set(
//      ((s1, VariableExp("a")), (f1, VariableExp("x"))),
      ((NopStmt, VariableExp("x")), (f1, VariableExp("x"))),
      ((f1, VariableExp("x")), (s3, VariableExp("c"))),
      ((s3, VariableExp("c")), (s4, VariableExp("d"))),
      //      ((s4, VariableExp("d")), (s5, VariableExp("d"))),
      ((s2, VariableExp("b")), (s4, VariableExp("d")))
    )

    assert(svf == expected)
  }
}
