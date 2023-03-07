package br.unb.cic.tip

import org.scalatest.funsuite.AnyFunSuite
import br.unb.cic.tip.Expression.*
import br.unb.cic.tip.Stmt.*
import br.unb.cic.tip.Node.{SimpleNode, *}
import br.unb.cic.tip.*

class RDTest extends AnyFunSuite {

//  test("test_get_assignment_from_stmts") {
//    val s1 = AssignmentStmt("x", ConstExp(1))
//    val s2 = AssignmentStmt("y", ConstExp(2))
//    val seq = SequenceStmt(s1, s2)
//
//    val expected = Set(
//      s1,
//      s2
//    )
//
//    assert( expected == statements(seq))
//  }

  test("test_get_assignment_from_factorial") {
    val s1 = AssignmentStmt("f", ConstExp(1))
    val s2 = AssignmentStmt("f", MultiExp(VariableExp("f"), VariableExp("n")))
    val s3 = AssignmentStmt("n", SubExp(VariableExp("n"), ConstExp(1)))
    val s4 = SequenceStmt(s2,s3)
    val s5 = WhileStmt(GTExp(VariableExp("n"), ConstExp(0)),s4)
    val s6 = SequenceStmt(s1, s5)

    val expected = Set(
      s1,
      s2,
      s3
    )

    assert(expected == statements(s6))
  }

  test("test_get_pre_from_stmts") {
    val s1 = AssignmentStmt("x", ConstExp(1))
    val s2 = AssignmentStmt("y", ConstExp(2))
    val seq = SequenceStmt(s1, s2)

    val function = FunDecl("xxx", List(), List(), seq, VariableExp("z"))
    val cfg = flow(function)


    val expected = Set(
      (SimpleNode(s1), SimpleNode(s2))
    )

    assert( expected == predecessor.run(cfg, SimpleNode(s2)))
  }

  test("test_get_pre_from_factorial") {
    val s1 = AssignmentStmt("f", ConstExp(1))
    val s2 = AssignmentStmt("f", MultiExp(VariableExp("f"), VariableExp("n")))
    val s3 = AssignmentStmt("n", SubExp(VariableExp("n"), ConstExp(1)))
    val s4 = WhileStmt(GTExp(VariableExp("n"), ConstExp(0)),SequenceStmt(s2,s3))
    val seq = SequenceStmt(s1, s4)

    val function = FunDecl("xxx", List(), List(), seq, VariableExp("z"))
    val cfg = flow(function)

    val expectedS4= Set(
      (SimpleNode(s1), SimpleNode(s4)),
      (SimpleNode(s3), SimpleNode(s4)),
      (SimpleNode(s2), SimpleNode(s3)),
      (SimpleNode(s4), SimpleNode(s2))
    )

    assert( expectedS4 == predecessor.run(cfg, SimpleNode(s4)))
  }

  test("test_get_pre_from_if") {
    val s0 = AssignmentStmt("m", ConstExp(0))
    val s1 = AssignmentStmt("x", ConstExp(1))
    val s2 = AssignmentStmt("y", ConstExp(2))
    val s3 = AssignmentStmt("z", ConstExp(3))
    val s4 = IfElseStmt(EqExp(ConstExp(1),ConstExp(2)), SequenceStmt(s1,s2), Some(s3))
    val s5 = AssignmentStmt("f", ConstExp(5))

    val seq = SequenceStmt(s0, SequenceStmt(s4,s5))

    val function = FunDecl("xxx", List(), List(), seq, VariableExp("z"))
    val cfg = flow(function)

    val expectedS1 = Set(
      (SimpleNode(s0), SimpleNode(s4)),
      (SimpleNode(s4), SimpleNode(s1))
    )

    assert( expectedS1 == predecessor.run(cfg, SimpleNode(s1)))

    val expectedS3 = Set(
      (SimpleNode(s0), SimpleNode(s4)),
      (SimpleNode(s4), SimpleNode(s3))
    )

    assert( expectedS3 == predecessor.run(cfg, SimpleNode(s3)))

    val expectedS2 = Set(
      (SimpleNode(s0), SimpleNode(s4)),
      (SimpleNode(s4), SimpleNode(s1)),
      (SimpleNode(s1), SimpleNode(s2))
    )

    assert( expectedS2 == predecessor.run(cfg, SimpleNode(s2)))

    val expectedS5 = Set(
      (SimpleNode(s0), SimpleNode(s4)),
      (SimpleNode(s4), SimpleNode(s1)),
      (SimpleNode(s1), SimpleNode(s2)),
      (SimpleNode(s2), SimpleNode(s5)),
      (SimpleNode(s4), SimpleNode(s3)),
      (SimpleNode(s3), SimpleNode(s5))
    )

    assert( expectedS5 == predecessor.run(cfg, SimpleNode(s5)))
  }

  test("test_rd_from_stmts") {
    val s1 = AssignmentStmt("x", ConstExp(1))
    val s2 = AssignmentStmt("y", ConstExp(2))
    val s3 = AssignmentStmt("y", ConstExp(3))
    val seq = SequenceStmt(s1, SequenceStmt(s2, s3))

    val cfg = flow(seq)
    val rd = ReachingDefinitions(cfg)

    rd.map(v => {
        println(s"node -> ${v._1}")
        println(s"RD -> ${v._2}")
    })
  }
}