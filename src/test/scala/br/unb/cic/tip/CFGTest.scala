package br.unb.cic.tip

import org.scalatest.funsuite.AnyFunSuite
import br.unb.cic.tip.Expression.*
import br.unb.cic.tip.Stmt.*
import br.unb.cic.tip.Node.*
import br.unb.cic.tip.*

class CFGTest extends AnyFunSuite {

  test("cfg simple stmts") {
    val s1 = AssignmentStmt("x", ConstExp(1))
    val s2 = AssignmentStmt("y", ConstExp(2))
    val seq = SequenceStmt(s1, s2)

    val expected = Set(
        (SimpleNode(s1), SimpleNode(s2)),
    )

    assert(expected == flow(seq))
  }

  /**
      f = 1;
      while (n>0) {
        f = f*n;
        n = n-1;
      }
   */
  test("cfg factorial") {
    val s1 = AssignmentStmt("f", ConstExp(1))
    val s2 = AssignmentStmt("f", MultiExp(VariableExp("f"), VariableExp("n")))
    val s3 = AssignmentStmt("n", SubExp(VariableExp("n"), ConstExp(1)))
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

  test("Test CFG using function with only statements") {
    val s1 = AssignmentStmt("x", ConstExp(1))
    val s2 = AssignmentStmt("y", ConstExp(1))
    val s3 = AssignmentStmt("z", AddExp(VariableExp("x"),VariableExp("y")))

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
  f = 1;
      while (n>0) {
        f = f*n;
        n = n-1;
      }
   */
  test("Test CFG using function: Factorial") {
    val s1 = AssignmentStmt("f", ConstExp(1))
    val s2 = AssignmentStmt("f", MultiExp(VariableExp("f"), VariableExp("n")))
    val s3 = AssignmentStmt("n", SubExp(VariableExp("n"), ConstExp(1)))
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
   * a = 1
   * if(a > 2)
   *  b = 2
   *  c = 3
   * else
   *  d = 4
   * e = 5
   */
  test("Function for if else stmt") {
    val s1 = AssignmentStmt("a", ConstExp(1))
    val s2 = AssignmentStmt("b", ConstExp(2))
    val s3 = AssignmentStmt("c", ConstExp(3))
    val s4 = AssignmentStmt("d", ConstExp(4))
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
    println(exportDot(cfg))
  }

  test("cfg using function") {
    val s1 = AssignmentStmt("a", ConstExp(1))
    val s2 = AssignmentStmt("b", ConstExp(2))
    val s3 = AssignmentStmt("c", ConstExp(3))
    val s4 = AssignmentStmt("d", ConstExp(4))
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
    println(exportDot(cfg))
  }
}








