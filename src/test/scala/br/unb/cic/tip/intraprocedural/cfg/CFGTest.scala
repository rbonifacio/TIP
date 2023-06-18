package br.unb.cic.tip.intraprocedural.cfg

import br.unb.cic.tip.*
import br.unb.cic.tip.utils.*
import br.unb.cic.tip.utils.Node.*
import br.unb.cic.tip.utils.Stmt.*
import br.unb.cic.tip.utils.FunDecl
import org.scalatest.funsuite.AnyFunSuite

class CFGTest extends AnyFunSuite {

  /**
   * s1: x = 1
   * s2: y = 2
   */
  test("test_cfg_simple_statements") {
    val s1 = AssignmentStmt(VariableExp("x"), ConstExp(1))
    val s2 = AssignmentStmt(VariableExp("y"), ConstExp(2))
    val seq = SequenceStmt(s1, s2)

    val expected = Set(
        (SimpleNode(s1), SimpleNode(s2)),
    )

    assert(expected == flow(seq))
  }

  /**
   *  s1: f = 1;
   *  s2: while (n>0) {
   *  s3:   f = f*n;
   *  s4:   n = n-1;
   *  s5: }
   */
  test("test_cfg_factorial") {
    val s1 = AssignmentStmt(VariableExp("f"), ConstExp(1))
    val s2 = AssignmentStmt(VariableExp("f"), MultiExp(VariableExp("f"), VariableExp("n")))
    val s3 = AssignmentStmt(VariableExp("n"), SubExp(VariableExp("n"), ConstExp(1)))
    val s4 = SequenceStmt(s2,s3)
    val s5 = WhileStmt(GTExp(VariableExp("n"), ConstExp(0)),s4)
    val s6 = SequenceStmt(s1, s5)

    val expected = Set(
      (SimpleNode(s1), SimpleNode(s5)),
      (SimpleNode(s5), SimpleNode(s2)),
      (SimpleNode(s2), SimpleNode(s3)),
      (SimpleNode(s3), SimpleNode(s5))
    )
    assert(expected == flow(s6))
  }

  /**
   * s1: sum() {
   * s2:  x = 1
   * s3:  y = 1
   * s4:  z = x + y
   * s5: }
   */
  test("test_cfg_using_function_with_only_statements") {
    val s1 = AssignmentStmt(VariableExp("x"), ConstExp(1))
    val s2 = AssignmentStmt(VariableExp("y"), ConstExp(1))
    val s3 = AssignmentStmt(VariableExp("z"), AddExp(VariableExp("x"),VariableExp("y")))

    val body = SequenceStmt(s1, SequenceStmt(s2, s3))
    val function = FunDecl("sum", List(), List(), body, VariableExp("z"))

    val expected = Set(
        (StartNode(function.name), SimpleNode(s1)),
        (SimpleNode(s1), SimpleNode(s2)),
        (SimpleNode(s2), SimpleNode(s3)),
        (SimpleNode(s3), EndNode(function.name)),
      )

    val cfg = flow(function)
    assert(expected == cfg)
//    println(exportDot(cfg))
  }


  /**
   * s1: f = 1;
   * s2: while (n>0) {
   * s3:   f = f*n;
   * s4:   n = n-1;
   * s5: }
   */
  test("test_cfg_function_factorial") {
    val s1 = AssignmentStmt(VariableExp("f"), ConstExp(1))
    val s2 = AssignmentStmt(VariableExp("f"), MultiExp(VariableExp("f"), VariableExp("n")))
    val s3 = AssignmentStmt(VariableExp("n"), SubExp(VariableExp("n"), ConstExp(1)))
    val s4 = SequenceStmt(s2,s3)
    val s5 = WhileStmt(GTExp(VariableExp("n"), ConstExp(0)),s4)
    val s6 = SequenceStmt(s1, s5)

    val function = FunDecl("sum", List(), List(), s6, VariableExp("z"))

    val expected = Set(
      (StartNode(function.name), SimpleNode(s1)),
      (SimpleNode(s1), SimpleNode(s5)),
      (SimpleNode(s5), SimpleNode(s2)),
      (SimpleNode(s2), SimpleNode(s3)),
      (SimpleNode(s3), SimpleNode(s5)),
      (SimpleNode(s5), EndNode(function.name))
    )

    assert(expected == flow(function))
  }

  /**
   * s1:  r = 0
   * s2:  if ( a > 0) {
   * s3:    r = a + b
   * s?:  } else {
   * s4:    r = a - b
   * s:  }
   * s5:  print(r)
   */
  test("test_cfg_if_else_simple_stmt") {
    val s1 = AssignmentStmt(VariableExp("r"), ConstExp(0))
    val s3 = AssignmentStmt(VariableExp("r"), AddExp(VariableExp("a"), VariableExp("b")))
    val s4 = AssignmentStmt(VariableExp("r"), SubExp(VariableExp("a"), VariableExp("b")))
    val s2 = IfElseStmt(GTExp(VariableExp("a"), ConstExp(0)), s3, Some(s4))
    val s5 = AssignmentStmt(VariableExp("d"), ConstExp(4))

    val body = SequenceStmt(s2, s5)
    val function = FunDecl("main", List(), List(), body, NullExp)

//    val expected = Set(
//      (StartNode(function.name), SimpleNode(s1)),
//      (SimpleNode(s1), SimpleNode(s5)),
//      (SimpleNode(s5), SimpleNode(s2)),
//      (SimpleNode(s2), SimpleNode(s3)),
//      (SimpleNode(s3), EndNode(function.name)),
//      (SimpleNode(s5), SimpleNode(s4)),
//      (SimpleNode(s4), EndNode(function.name))
//    )

    val cfg = flow(function)
//    assert(expected == cfg)
        println(exportDot(cfg))
  }


  /**
   * s1:  a = 1
   * s2:  if (a > 2) {
   * s3:    b = 2
   * s4:    c = 3
   * s5:  } else {
   * s6:    d = 4
   * s7:    e = 5
   * s8: }
   */
  test("test_cfg_if_else_stmt") {
    val s1 = AssignmentStmt(VariableExp("a"), ConstExp(1))
    val s2 = AssignmentStmt(VariableExp("b"), ConstExp(2))
    val s3 = AssignmentStmt(VariableExp("c"), ConstExp(3))
    val s4 = AssignmentStmt(VariableExp("d"), ConstExp(4))
    val s5 = IfElseStmt(EqExp(VariableExp("x"),ConstExp(2)), SequenceStmt(s2,s3), Some(s4))

    val body = SequenceStmt(s1, s5)
    val function = FunDecl("ifelse", List(), List(), body, VariableExp("z"))

    val expected = Set(
      (StartNode(function.name), SimpleNode(s1)),
      (SimpleNode(s1), SimpleNode(s5)),
      (SimpleNode(s5), SimpleNode(s2)),
      (SimpleNode(s2), SimpleNode(s3)),
      (SimpleNode(s3), EndNode(function.name)),
      (SimpleNode(s5), SimpleNode(s4)),
      (SimpleNode(s4), EndNode(function.name))
    )

    val cfg = flow(function)
    assert(expected == cfg)
//    println(exportDot(cfg))
  }

  /**
   * s1: a = 1
   * s2: if (x == 2) {
   * s3:  b = 2
   * s4:  c = 3
   * s5: } else {
   * s6:  d = 4
   * s7: }
   */
  test("test_cfg_using_function") {
    val s1 = AssignmentStmt(VariableExp("a"), ConstExp(1))
    val s2 = AssignmentStmt(VariableExp("b"), ConstExp(2))
    val s3 = AssignmentStmt(VariableExp("c"), ConstExp(3))
    val s4 = AssignmentStmt(VariableExp("d"), ConstExp(4))
    val s5 = IfElseStmt(EqExp(VariableExp("x"),ConstExp(2)), SequenceStmt(s2,s3), Some(s4))

    val body = SequenceStmt(s1, s5)
    val function = FunDecl("ifelse", List(), List(), body, VariableExp("z"))

    val expected = Set(
      (StartNode(function.name), SimpleNode(s1)),
      (SimpleNode(s1), SimpleNode(s5)),
      (SimpleNode(s5), SimpleNode(s2)),
      (SimpleNode(s2), SimpleNode(s3)),
      (SimpleNode(s3), EndNode(function.name)),
      (SimpleNode(s5), SimpleNode(s4)),
      (SimpleNode(s4), EndNode(function.name))
    )

    val cfg = flow(function)
    assert(expected == cfg)
//    println(exportDot(cfg))
  }
}








