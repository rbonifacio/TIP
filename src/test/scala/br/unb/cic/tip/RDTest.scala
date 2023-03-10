package br.unb.cic.tip

import br.unb.cic.tip.*
import br.unb.cic.tip.Expression.*
import br.unb.cic.tip.Node.*
import br.unb.cic.tip.Stmt.*
import org.scalatest.funsuite.AnyFunSuite

class RDTest extends AnyFunSuite {

  test("test_rd_using_only_statements") {
    val s1 = AssignmentStmt("x", ConstExp(1))
    val s2 = AssignmentStmt("y", ConstExp(2))
    val s3 = AssignmentStmt("x", ConstExp(3))
    val seq = SequenceStmt(s1, SequenceStmt(s2, s3))

    val RD = ReachingDefinition.run(seq)

    assert( RD(s1) == (
      Set(AssignmentStmt("x",NullExp), AssignmentStmt("y",NullExp)),
      Set(AssignmentStmt("y",NullExp), s1)
    ))

    assert( RD(s2) == (
      Set(AssignmentStmt("y",NullExp), s1),
      Set(s1, s2)
    ))

    assert( RD(s3) == (
      Set(s1, s2),
      Set(s2, s3)
    ))
  }

  test("test_rd_using_only_statements_complex") {
    val s1 = AssignmentStmt("x", ConstExp(1))
    val s2 = AssignmentStmt("y", ConstExp(2))
    val s3 = AssignmentStmt("x", AddExp(VariableExp("x"), VariableExp("y")))
    val s4 = AssignmentStmt("z", AddExp(VariableExp("x"), ConstExp(1)))
    val s5 = AssignmentStmt("z", VariableExp("y"))
    val seq = SequenceStmt(s1, SequenceStmt(s2, SequenceStmt(s3, SequenceStmt(s4, s5))))

    val RD = ReachingDefinition.run(seq)

    assert( RD(s1) == (
      Set(AssignmentStmt("x", NullExp), AssignmentStmt("y", NullExp), AssignmentStmt("z", NullExp)),
      Set(s1, AssignmentStmt("y", NullExp), AssignmentStmt("z", NullExp))
    ))

    assert( RD(s2) == (
      Set(s1, AssignmentStmt("y", NullExp), AssignmentStmt("z", NullExp)),
      Set(s1, s2, AssignmentStmt("z", NullExp))
    ))

    assert( RD(s3) == (
      Set(s1, s2, AssignmentStmt("z", NullExp)),
      Set(s3, s2, AssignmentStmt("z", NullExp))
    ))

    assert( RD(s4) == (
      Set(s3, s2, AssignmentStmt("z", NullExp)),
      Set(s3, s2, s4)
    ))

    assert( RD(s5) == (
      Set(s3, s2, s4),
      Set(s3, s2, s5)
    ))
  }

  test("test_rd_using_if") {
    val s1 = AssignmentStmt("x", ConstExp(1))
    val s2 = AssignmentStmt("y", ConstExp(2))
    val s3 = AssignmentStmt("z", ConstExp(3))
    val s4 = IfElseStmt(EqExp(ConstExp(1),ConstExp(2)), SequenceStmt(s1,s2), Some(s3))
    val s5 = AssignmentStmt("y", ConstExp(0))
    val seq = SequenceStmt(s4, s5)

    val RD = ReachingDefinition.run(seq)

    assert( RD(s1) == (
      Set(AssignmentStmt("x", NullExp), AssignmentStmt("y", NullExp), AssignmentStmt("z", NullExp)),
      Set(s1, AssignmentStmt("y", NullExp), AssignmentStmt("z", NullExp))
    ))

    assert( RD(s2) == (
      Set(s1, AssignmentStmt("y", NullExp), AssignmentStmt("z", NullExp)),
      Set(s1, s2, AssignmentStmt("z", NullExp))
    ))

    assert( RD(s3) == (
      Set(AssignmentStmt("x", NullExp), AssignmentStmt("y", NullExp), AssignmentStmt("z", NullExp)),
      Set(AssignmentStmt("x", NullExp), AssignmentStmt("y", NullExp), s3)
    ))

    assert( RD(s4) == (
      Set(AssignmentStmt("x", NullExp), AssignmentStmt("y", NullExp), AssignmentStmt("z", NullExp)),
      Set(AssignmentStmt("x", NullExp), AssignmentStmt("y", NullExp), AssignmentStmt("z", NullExp))
    ))

    assert( RD(s5) == (
      Set(s1, s2, AssignmentStmt("z", NullExp), AssignmentStmt("x", NullExp), AssignmentStmt("y", NullExp), s3),
      Set(s1, s5, AssignmentStmt("z", NullExp), AssignmentStmt("x", NullExp), s3)
    ))
  }

  /**
  f = 1;
  while (n>0) {
    f = f*n;
    n = n-1;
  }
   */
  test("test_rd_using_while") {
    val s1 = AssignmentStmt("f", ConstExp(1))
    val s2 = AssignmentStmt("f", MultiExp(VariableExp("f"), VariableExp("n")))
    val s3 = AssignmentStmt("n", SubExp(VariableExp("n"), ConstExp(1)))
    val whileBody = SequenceStmt(s2,s3)
    val s4 = WhileStmt(GTExp(VariableExp("n"), ConstExp(0)),whileBody)
    val seq = SequenceStmt(s1, s4)

    val RD = ReachingDefinition.run(seq)

    assert( RD(s1) == (
      Set(AssignmentStmt("f", NullExp), AssignmentStmt("n", NullExp)),
      Set(s1, AssignmentStmt("n", NullExp))
    ))

    assert( RD(s4) == (
      Set(s1, AssignmentStmt("n", NullExp), s2, s3),
      Set(s1, AssignmentStmt("n", NullExp), s2, s3)
    ))

    assert( RD(s2) == (
      Set(s1, AssignmentStmt("n", NullExp), s2, s3),
      Set(AssignmentStmt("n", NullExp), s2, s3)
    ))

    assert( RD(s3) == (
      Set(AssignmentStmt("n", NullExp), s2, s3),
      Set(s2, s3)
    ))
  }
}
//    ReachingDefinition.run(seq).foreach {
//      case (key, value) => println(s"$key -> [Entry] ${value._1} [Exit] ${value._2}")
//    }