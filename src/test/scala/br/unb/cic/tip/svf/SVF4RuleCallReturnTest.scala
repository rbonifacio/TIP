package br.unb.cic.tip.svf

import br.unb.cic.tip.utils.Node.SimpleNode
import br.unb.cic.tip.utils.Stmt.*
import br.unb.cic.tip.utils.*
import br.unb.cic.tip.{convertSVFtoGraph, exportDot}
import org.scalatest.funsuite.AnyFunSuite

class SVF4RuleCallReturnTest extends AnyFunSuite {
  
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

  test("test_svf_call_rule") {

    val f1 = AssignmentStmt(VariableExp("y"), MultiExp(VariableExp("a"), ConstExp(-1)))
    val f2 = ReturnStmt(VariableExp("y"))
    val fSignBody = SequenceStmt(f1, f2)
    val fSign = FunDecl("fSign", List(VariableExp("x")), List(VariableExp("y")), fSignBody, VariableExp("y"))

    val s1 = AssignmentStmt(VariableExp("a"), ConstExp(1))
    val s2 = AssignmentStmt(VariableExp("b"), FunctionCallExp(fSign.name, List(VariableExp("a"))))
    val s3 = OutputStmt(VariableExp("b"))

    //main function
    val fMainBody = SequenceStmt(s1, SequenceStmt(s2, s3))

    val fMain = FunDecl("main", List(), List(VariableExp("a"), VariableExp("b")), fMainBody, NullExp)

    val program = List(fSign, fMain)

    val svf = SVF.run(program)

    val expected = Set(
      ((s1, VariableExp("a")), (f1, VariableExp("y"))),
      ((f1, VariableExp("y")), (f2, VariableExp("y"))),
      ((f2, VariableExp("y")), (s2, VariableExp("b")))
    )

    assert(svf == expected)
  }

  /**
   * fx: sign(a) {
   * f1:  a = a * -1
   * f2:  return a
   * fx: }
   *
   * sx: main() {
   * s1:   a = 1
   * s2:   b = sign(a)
   * s3:   print b
   * s4: }
   */

  test("test_svf_call_rule_with_return") {

    val f1 = AssignmentStmt(VariableExp("a"), MultiExp(VariableExp("a"), ConstExp(-1)))
    val f2 = ReturnStmt(VariableExp("a"))
    val fSignBody = SequenceStmt(f1, f2)
    val fSign = FunDecl("fSign", List(VariableExp("x")), List(VariableExp("y")), fSignBody, VariableExp("y"))

    val s1 = AssignmentStmt(VariableExp("a"), ConstExp(1))
    val s2 = AssignmentStmt(VariableExp("b"), FunctionCallExp(fSign.name, List(VariableExp("a"))))
    val s3 = OutputStmt(VariableExp("b"))

    //main function
    val fMainBody = SequenceStmt(s1, SequenceStmt(s2, s3))

    val fMain = FunDecl("main", List(), List(VariableExp("a"), VariableExp("b")), fMainBody, NullExp)

    val program = List(fSign, fMain)

    val svf = SVF.run(program)

    val expected = Set(
      ((f1, VariableExp("a")), (f1, VariableExp("a"))),
      ((f1, VariableExp("a")), (f2, VariableExp("a"))),
      ((f2, VariableExp("a")), (s2, VariableExp("b")))
    )

    assert(svf == expected)
  }

  /**
   * fx: sign(p) {
   * f1:  t = p
   * f2:  return t
   * fx: }
   *
   * sx: main() {
   * s1:  p = alloc 1
   * s1:  q = alloc 2
   * s3:  p = &q
   * s:   s = sign(p)
   * s:   print s
   * s: }
   */

  test("test_svf_call_return_rule_pointers") {

    val f1 = AssignmentStmt(PointerExp("t"), PointerExp("p"))
    val f2 = ReturnStmt(PointerExp("t"))
    val fSignBody = SequenceStmt(f1, f2)
    val fSign = FunDecl("fSign", List(PointerExp("p")), List(PointerExp("t")), fSignBody, VariableExp("y"))

    val s1 = AssignmentStmt(PointerExp("p"), AllocExp(ConstExp(1)))
    val s2 = AssignmentStmt(PointerExp("q"), AllocExp(ConstExp(2)))
    val s3 = AssignmentStmt(PointerExp("p"), LocationExp(PointerExp("q")))
    val s4 = AssignmentStmt(PointerExp("s"), FunctionCallExp(fSign.name, List(PointerExp("p"))))
    val s5 = OutputStmt(PointerExp("s"))

    //main function
    val fMainBody = SequenceStmt(s1, SequenceStmt(s2, SequenceStmt(s3, SequenceStmt(s4, s5))))

    val fMain = FunDecl(
      "main",
      List(),
      List(PointerExp("p"), PointerExp("q"), PointerExp("r"), PointerExp("s")),
      fMainBody,
      NullExp
    )

    val program = List(fSign, fMain)

    val svf = SVF.run(program)

    val expected = Set(
      ((s3, PointerExp("p")), (f1, PointerExp("t"))),
      ((f1, PointerExp("t")), (f2, PointerExp("t"))),
      ((f2, PointerExp("t")), (s4, PointerExp("s")))
    )

    assert(svf == expected)
  }

  /**
   * fx: sign(p) {
   * f1:  t = *p
   * f2:  return t
   * fx: }
   *
   * sx: main() {
   * s1:  p = alloc 1
   * s1:  q = alloc 2
   * s3:  p = &q
   * s:   s = sign(p)
   * s:   print s
   * s: }
   */

  test("test_svf_call_return_rule_pointers_loads") {

    val f1 = AssignmentStmt(PointerExp("t"), LoadExp(PointerExp("p")))
    val f2 = ReturnStmt(PointerExp("t"))
    val fSignBody = SequenceStmt(f1, f2)
    val fSign = FunDecl("fSign", List(PointerExp("p")), List(PointerExp("t")), fSignBody, VariableExp("y"))

    val s1 = AssignmentStmt(PointerExp("p"), AllocExp(ConstExp(1)))
    val s2 = AssignmentStmt(PointerExp("q"), AllocExp(ConstExp(2)))
    val s3 = AssignmentStmt(PointerExp("p"), LocationExp(PointerExp("q")))
    val s4 = AssignmentStmt(PointerExp("s"), FunctionCallExp(fSign.name, List(PointerExp("p"))))
    val s5 = OutputStmt(PointerExp("s"))

    //main function
    val fMainBody = SequenceStmt(s1, SequenceStmt(s2, SequenceStmt(s3, SequenceStmt(s4, s5))))

    val fMain = FunDecl(
      "main",
      List(),
      List(PointerExp("p"), PointerExp("q"), PointerExp("r"), PointerExp("s")),
      fMainBody,
      NullExp
    )

    val program = List(fSign, fMain)

    val svf = SVF.run(program)

    val expected = Set(
        ((s2, PointerExp("q")), (f1, PointerExp("t"))),
        ((f1, PointerExp("t")), (f2, PointerExp("t"))),
        ((f2, PointerExp("t")), (s4, PointerExp("s")))
    )

    assert(svf == expected)
  }
}
