package br.unb.cic.tip.interprocedural.df

import br.unb.cic.tip.*
import br.unb.cic.tip.df.ReachingDefinition
import br.unb.cic.tip.utils.*
import br.unb.cic.tip.utils.Node.*
import br.unb.cic.tip.utils.Stmt.*
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.Set

/**
 * P(var, stmt)
 */
class RDInterproceduralComplexTest extends AnyFunSuite {

  /**
   * mymethod(x)
   * m1(s3):  x = 5         entry:{(x,s1)} U {}                   exit:{(x,m1)} // entry gets the values from the CALLER
   * m1(s7):  x = 5         entry:{(x,s6)} U {}                   exit:{(x,m1)} // entry gets the values from the CALLER
   *
   * main()
   * s1:  x = 1             entry:{}                                                          exit:{(x,s1)}
   * s2:  y = 2             entry:{(x,s1)}                                                    exit:{(x,s1), (y,s2)}
   * s3:  z = mymethod(x)   entry:{(x,s1), (y,s2)}                                            exit:{(x,s1), (y,s2), (z,s3)} U  {(x,m1)} = {(y,s2), (z,s3), (x, m1)} // exit get the values from the CALLEE
   * s4:  a = y + 1         entry:{(y,s2), (z,s3), (x, m1)}                                   exit:{(y,s2), (z,s3), (x,m1), (a,s4)}
   * s5:  b = x + 1         entry:{(y,s2), (z,s3), (x, m1), (a, s4)}                          exit:{(y,s2), (z,s3), (x,m1), (a,s4), (b,s5)}
   * s6:  x = 100           entry:{(y,s2), (z,s3), (x,m1), (a,s4), (b,s5)}                    exit:{(y,s2), (z,s3), (x,s6), (a,s4), (b,s5)}
   * s7:  c = mymethod(x)   entry:{(y,s2), (z,s3), (x,s6), (a,s4), (b,s5)}                    exit:{(y,s2), (z,s3), (x,s6), (a,s4), (b,s5), (c,s7)} U {(x,m1)} = {(y,s2), (x,m1), (a,s4), (b,s5), (c,s7)}
   * s8:  aa = y + 11       entry:{(y,s2), (z,s3), (x,m1), (a,s4), (b,s5), (c,s7)}            exit:{(y,s2), (z,s3), (x,m1), (a,s4), (b,s5), (c,s7), (aa,s8)}
   * s9:  bb = x + 11       entry:{(y,s2), (z,s3), (x,m1), (a,s4), (b,s5), (c,s7), (aa,s8)}   exit:{(y,s2), (z,s3), (x,m1), (a,s4), (b,s5), (c,s7), (aa,s8), (bb, s9)}
   */
  test("test_rd_calling_function_two_times") {

    val m1 = AssignmentStmt(VariableExp("x"), ConstExp(5))
    val myFunction = FunDecl("myFunction", List(VariableExp("x")), List(), m1, NullExp)

    val s1 = AssignmentStmt(VariableExp("x"), ConstExp(1))
    val s2 = AssignmentStmt(VariableExp("y"), ConstExp(2))
    val s3 = AssignmentStmt(VariableExp("z"), FunctionCallExp(myFunction.name, List(VariableExp("x"))))
    val s4 = AssignmentStmt(VariableExp("a"), AddExp(VariableExp("y"), ConstExp(1)))
    val s5 = AssignmentStmt(VariableExp("b"), AddExp(VariableExp("x"), ConstExp(1)))
    val s6 = AssignmentStmt(VariableExp("x"), ConstExp(100))
    val s7 = AssignmentStmt(VariableExp("c"), FunctionCallExp(myFunction.name, List(VariableExp("x"))))
    val s8 = AssignmentStmt(VariableExp("aa"), AddExp(VariableExp("y"), ConstExp(11)))
    val s9 = AssignmentStmt(VariableExp("bb"), AddExp(VariableExp("x"), ConstExp(11)))

    //main function
    val mainBody =
      SequenceStmt(s1,
        SequenceStmt(s2,
          SequenceStmt(s3,
            SequenceStmt(s4,
              SequenceStmt(s5,
                SequenceStmt(s6,
                  SequenceStmt(s7,
                    SequenceStmt(s8, s9
                    ))))))))

    val mainFunction = FunDecl("main", List(), List(VariableExp("x"), VariableExp("y"), VariableExp("z")), mainBody, NullExp)

    val program = List(myFunction, mainFunction)

    val RD = ReachingDefinition.run(mainBody, program)

    assert(RD((s1, NopStmt)) == (
      Set(),
      Set(s1)
    ))

    assert(RD((s2, NopStmt)) == (
      Set(s1),
      Set(s1, s2)
    ))

    assert(RD((s3, NopStmt)) == (
      Set(s1, s2),
      Set(s2, s3, m1)
    ))

    // for the first call
    assert(RD((m1, s3)) == (
      Set(s1),
      Set(m1),
    ))

    assert(RD((s4, NopStmt)) == (
      Set(s2, s3, m1),
      Set(s2, s3, m1, s4)
    ))

    assert(RD((s5, NopStmt)) == (
      Set(s2, s3, m1, s4),
      Set(s2, s3, m1, s4, s5)
    ))

    assert(RD((s6, NopStmt)) == (
      Set(s2, s3, m1, s4, s5),
      Set(s2, s3, s4, s5, s6)
    ))

    assert(RD((s7, NopStmt)) == (
      Set(s2, s3, s4, s5, s6),
      Set(s2, s3, s4, s5, s7, m1)
    ))

    // for the second call
    assert(RD((m1, s7)) == (
      Set(s6),
      Set(m1),
    ))

    assert(RD((s8, NopStmt)) == (
      Set(s2, s3, s4, s5, s7, m1),
      Set(s2, s3, s4, s5, s7, m1, s8)
    ))

    assert(RD((s9, NopStmt)) == (
      Set(s2, s3, s4, s5, s7, m1, s8),
      Set(s2, s3, s4, s5, s7, m1, s8, s9)
    ))
  }
}