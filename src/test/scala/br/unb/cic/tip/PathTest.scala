package br.unb.cic.tip

import org.scalatest.funsuite.AnyFunSuite
import br.unb.cic.tip.Expression.*
import br.unb.cic.tip.Node.*
import br.unb.cic.tip.Stmt.{AssignmentStmt, *}

class PathTest extends AnyFunSuite {

  test("cfg simple stmts") {
    val s1 = AssignmentStmt("x", ConstExp(1))
    val s2 = AssignmentStmt("z", ConstExp(3))
    val s3 = OutputStmt(VariableExp("x"))
    val seq = SequenceStmt(s1, SequenceStmt(s2, s3))

    val cfg = flow(seq)

    println(createPath(s1, s3, cfg))

//    val expected = Set(
//      (SimpleNode(s1), SimpleNode(s2)),
//    )
//
//    assert(expected == flow(seq))
  }
}