package br.unb.cic.tip.intraprocedural.pointer

import br.unb.cic.tip.pointer.AliasAnalysis
import br.unb.cic.tip.utils.Stmt.{AssignmentStmt, SequenceStmt}
import br.unb.cic.tip.utils.{AllocExp, ConstExp, NullExp, PointerExp}
import org.scalatest.funsuite.AnyFunSuite

class AliasAnalysisTest extends AnyFunSuite {

  /**
   * s1: p = alloc null
   * s2: p = alloc null
   * s3: q = alloc 1
   *
   */
  test("test_pt_all_andersen_rules") {

    val s1 = AssignmentStmt(PointerExp("a"), AllocExp(NullExp))
    val s2 = AssignmentStmt(PointerExp("b"), AllocExp(NullExp))
    val s3 = AssignmentStmt(PointerExp("c"), AllocExp(ConstExp(1)))

    val mainBody = SequenceStmt(s1, SequenceStmt(s2, s3))

    assert(AliasAnalysis.run(mainBody, PointerExp("a"), PointerExp("b")) == true)
    assert(AliasAnalysis.run(mainBody, PointerExp("a"), PointerExp("c")) == false)
  }

}
