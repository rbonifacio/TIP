package br.unb.cic.tip

import br.unb.cic.tip.*
import br.unb.cic.tip.Expression.*
import br.unb.cic.tip.Node.*
import br.unb.cic.tip.Stmt.*
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.Set

class RDInterproceduralTest extends AnyFunSuite {

  /**
   * mymethod(a)
   *  a = 999
   *
   * main()
   *  x = 1
   *  y = 2
   *  mymethod(x)
   *  z = 3
   */
  test("test_rd_using_only_statements") {

    val m1 = AssignmentStmt("a", ConstExp(999))
    val myFunction = FunDecl("myFunction", List("x"), List(), m1, NullExp)

    val s1 = AssignmentStmt("x", ConstExp(1))
    val s2 = AssignmentStmt("y", ConstExp(2))
    val s3 = AssignmentStmt("_m1", DirectFunctionCallExp(myFunction.name, List(VariableExp("x"))))
    val s4 = AssignmentStmt("z", ConstExp(3))

    //main function
    val mainBody = SequenceStmt(s1, SequenceStmt(s2, SequenceStmt(s3, s4)))

    val mainFunction = FunDecl("main", List(), List("x", "y", "z"), mainBody, NullExp)

    val program = List(myFunction, mainFunction)

    val RD = ReachingDefinition.run(mainBody, program)

    assert( RD(s1) == (
      Set(AssignmentStmt("x", NullExp), AssignmentStmt("y", NullExp), AssignmentStmt("z", NullExp), AssignmentStmt("_m1", NullExp)),
      Set(s1, AssignmentStmt("y", NullExp), AssignmentStmt("z", NullExp), AssignmentStmt("_m1", NullExp))
    ))

    assert( RD(s2) == (
      Set(s1, AssignmentStmt("y", NullExp), AssignmentStmt("z", NullExp), AssignmentStmt("_m1", NullExp)),
      Set(s1, s2, AssignmentStmt("z", NullExp), AssignmentStmt("_m1", NullExp))
    ))

    assert(RD(s3) == (
      Set(),
      Set()
    ))

    assert( RD(m1) == (
      Set(AssignmentStmt("a", NullExp)),
      Set(AssignmentStmt("a", ConstExp(999)))
    ))

    assert( RD(s4) == (
      Set(),
      Set(AssignmentStmt("z", ConstExp(3)))
    ))
  }
}