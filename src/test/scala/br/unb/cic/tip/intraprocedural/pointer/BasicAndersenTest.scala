package br.unb.cic.tip.intraprocedural.pointer

import br.unb.cic.tip.blocks
import br.unb.cic.tip.pointer.BasicAndersen
import br.unb.cic.tip.utils.{AssignmentPointerStmt, FunDecl, SequenceStmt}
import br.unb.cic.tip.utils.Expression.*
import org.scalatest.funsuite.AnyFunSuite

class BasicAndersenTest extends AnyFunSuite {

  /**
   *  s1: a = 1
   *  s2: b = alloc null
   *  s3: c = &b
   *  s4: d = b
   *  s5: e = *c
//   *  s6: *b = d
   *  s7: f = null
   *
   */
  test("test_rd_calling_function_one_time") {

    val s1 = AssignmentPointerStmt(VariableExp("a"), ConstExp(1))
    val s2 = AssignmentPointerStmt(VariableExp("b"), AllocExp(NullExp))
    val s3 = AssignmentPointerStmt(VariableExp("c"), LocationExp("b"))
    val s4 = AssignmentPointerStmt(VariableExp("d"), PointerExp("b"))
    val s5 = AssignmentPointerStmt(VariableExp("e"), LoadExp(PointerExp("c")))
    val s7 = AssignmentPointerStmt(VariableExp("g"), NullExp)
    //main function7u
    val mainBody = SequenceStmt(s1, SequenceStmt(s2, SequenceStmt(s3, SequenceStmt(s4, SequenceStmt(s5, s7)))))

//    val mainFunction = FunDecl("main", List(), List(), mainBody, NullExp)

//    val program = List(mainFunction)

    val RD = BasicAndersen.run(mainBody)

//    println(RD)

    assert(RD(s1.name.asInstanceOf[VariableExp]) == (
      Set(ConstExp(1))
      )
    )

    assert(RD(s2.name.asInstanceOf[VariableExp]) == (
      Set(AllocExp(NullExp))
      )
    )

    assert(RD(s3.name.asInstanceOf[VariableExp]) == (
      Set(VariableExp("b"))
      )
    )

    assert(RD(s4.name.asInstanceOf[VariableExp]) == (
      Set(AllocExp(NullExp))
      )
    )

    assert(RD(s5.name.asInstanceOf[VariableExp]) == (
      Set(AllocExp(NullExp))
      )
    )

    assert(RD(s7.name.asInstanceOf[VariableExp]) == (
      Set()
      )
    )
  }
}
