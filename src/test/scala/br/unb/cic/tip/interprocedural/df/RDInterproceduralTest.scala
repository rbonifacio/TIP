package br.unb.cic.tip.interprocedural.df

import br.unb.cic.tip.*
import br.unb.cic.tip.df.ReachingDefinition
import br.unb.cic.tip.utils.{Expression}
import br.unb.cic.tip.utils.FunDecl
import br.unb.cic.tip.utils.Node.*
import br.unb.cic.tip.utils.Stmt.*
import br.unb.cic.tip.utils._
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.Set

/**
 * P(var, stmt)
 */
class RDInterproceduralTest extends AnyFunSuite {

  /**
   * fx: show(x) {
   * f1:   print x          entry: {(a, s1)} U {}                exit: {(a, s1)}
   * fx: }
   *
   * sx: main() {
   * s1:   a = 1            entry: {}                    exit: {(a, s1)}
   * s2:   b = show(a)      entry: {(a, s1)}             exit: {(a, s1), (b, s2)} U {}
   * s3:   print b          entry: {(a, s1), (b, s2)}    exit: {(a, s1), (b, s2)}
   * s4: }
   */
  test("test_rd_call_output_function") {

    val f1 = OutputStmt(VariableExp("x"))
    val fShowBody = f1
    val fShow = FunDecl("fShow", List(VariableExp("x")), List(), fShowBody, NullExp)

    val s1 = AssignmentStmt(VariableExp("a"), ConstExp(1))
    val s2 = AssignmentStmt(VariableExp("b"), FunctionCallExp(NameExp(fShow.name), List(VariableExp("a"))))
    val s3 = OutputStmt(VariableExp("b"))

    //main function
    val fMainBody = SequenceStmt(s1, SequenceStmt(s2, s3))

    val fMain = FunDecl("main", List(), List(VariableExp("a"), VariableExp("b")), fMainBody, NullExp)

    val program = List(fShow, fMain)

    val RD = ReachingDefinition.run(fMainBody, program)

    assert(RD((s1, NopStmt)) == (
      Set(),
      Set(s1)
    ))

    assert(RD((s2, NopStmt)) == (
      Set(s1),
      Set(s1, s2)
    ))

    assert(RD((f1, s2)) == (
      Set(s1),
      Set(s1)
    ))

    assert(RD((s3, NopStmt)) == (
      Set(s1, s2),
      Set(s1, s2)
    ))
  }

  /**
   * fx: identity(a) {
   * f1:   return a           entry: {(a, s1)} U {}      exit: {(a, s1)}
   * fx: }
   *
   * sx: main() {
   * s1:   a = 1              entry: {}                    exit: {(a, s1)}
   * s2:   b = identity(a)    entry: {(a, s1)}             exit: {(a, s1), (b, s2)} U {(a, s1)}
   * s3:   print b            entry: {(a, s1), (b, s2)}    exit: {(a, s1), (b, s2)}
   * s4: }
   */
  test("test_rd_call_identity_function_same_parameter") {

    val f1 = ReturnStmt(VariableExp("a"))
    val fIdentityBody = f1
    val fIdentity = FunDecl("fIdentity", List(VariableExp("a")), List(), fIdentityBody, NullExp)

    val s1 = AssignmentStmt(VariableExp("a"), ConstExp(1))
    val s2 = AssignmentStmt(VariableExp("b"), FunctionCallExp(NameExp(fIdentity.name), List(VariableExp("a"))))
    val s3 = OutputStmt(VariableExp("b"))

    //main function
    val fMainBody = SequenceStmt(s1, SequenceStmt(s2, s3))

    val fMain = FunDecl("main", List(), List(VariableExp("a"), VariableExp("b")), fMainBody, NullExp)

    val program = List(fIdentity, fMain)

    val RD = ReachingDefinition.run(fMainBody, program)

    assert(RD((s1, NopStmt)) == (
      Set(),
      Set(s1)
    ))

    assert(RD((s2, NopStmt)) == (
      Set(s1),
      Set(s1, s2)
    ))

    assert(RD((f1, s2)) == (
      Set(s1),
      Set(s1)
    ))

    assert(RD((s3, NopStmt)) == (
      Set(s1, s2),
      Set(s1, s2)
    ))
  }

  /**
   * fx: identity(x) {
   * f1:   return x           entry: {(a, s1)} U {}      exit: {(a, s1)}
   * fx: }
   *
   * sx: main() {
   * s1:   a = 1              entry: {}                    exit: {(a, s1)}
   * s2:   b = identity(a)    entry: {(a, s1)}             exit: {(a, s1), (b, s2)} U {(a, s1)}
   * s3:   print b            entry: {(a, s1), (b, s2)}    exit: {(a, s1), (b, s2)}
   * s4: }
   */
  test("test_rd_call_identity_function") {

    val f1 = ReturnStmt(VariableExp("x"))
    val fIdentityBody = f1
    val fIdentity = FunDecl("fIdentity", List(VariableExp("x")), List(), fIdentityBody, NullExp)

    val s1 = AssignmentStmt(VariableExp("a"), ConstExp(1))
    val s2 = AssignmentStmt(VariableExp("b"), FunctionCallExp(NameExp(fIdentity.name), List(VariableExp("a"))))
    val s3 = OutputStmt(VariableExp("b"))

    //main function
    val fMainBody = SequenceStmt(s1, SequenceStmt(s2, s3))

    val fMain = FunDecl("main", List(), List(VariableExp("a"), VariableExp("b")), fMainBody, NullExp)

    val program = List(fIdentity, fMain)

    val RD = ReachingDefinition.run(fMainBody, program)

    assert(RD((s1, NopStmt)) == (
      Set(),
      Set(s1)
    ))

    assert(RD((s2, NopStmt)) == (
      Set(s1),
      Set(s1, s2)
    ))

    assert(RD((f1, s2)) == (
      Set(s1),
      Set(s1)
    ))

    assert(RD((s3, NopStmt)) == (
      Set(s1, s2),
      Set(s1, s2)
    ))
  }

  /**
   * fx: returnY(x) {
   * f1:  y = 99              entry: {(a, s1)} U {}       exit: {(a, s1), (y, f1)}
   * f1:  return y            entry: {(a, s1), (y, f1)}   exit: {(a, s1), (y, f1)}
   * fx: }
   *
   * sx: main() {
   * s1:   a = 1              entry: {}                    exit: {(a, s1)}
   * s2:   b = returnY(a)     entry: {(a, s1)}             exit: {(a, s1), (b, s2)} U {(a, s1)}
   * s3:   print b            entry: {(a, s1), (b, s2)}    exit: {(a, s1), (b, s2)}
   * s4: }
   */
  test("test_rd_call_return_function_another_parameter") {

    val f1 = AssignmentStmt(VariableExp("y"), ConstExp(1))
    val f2 = ReturnStmt(VariableExp("y"))
    val fReturnYBody = SequenceStmt(f1, f2)
    val fReturnY = FunDecl("fIdentity", List(VariableExp("x")), List(), fReturnYBody, NullExp)

    val s1 = AssignmentStmt(VariableExp("a"), ConstExp(1))
    val s2 = AssignmentStmt(VariableExp("b"), FunctionCallExp(NameExp(fReturnY.name), List(VariableExp("a"))))
    val s3 = OutputStmt(VariableExp("b"))

    //main function
    val fMainBody = SequenceStmt(s1, SequenceStmt(s2, s3))

    val fMain = FunDecl("main", List(), List(VariableExp("a"), VariableExp("b")), fMainBody, NullExp)

    val program = List(fReturnY, fMain)

    val RD = ReachingDefinition.run(fMainBody, program)

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
      Set(s1, s2)
    ))

    assert(RD((f1, s2)) == (
      Set(s1),
      Set(s1, f1)
    ))

    assert(RD((f2, s2)) == (
      Set(s1, f1),
      Set(s1, f1)
    ))
  }

  /**
   * fx: sign(a) {
   * f1:  y = a * -1        entry: {(a, s1)} U {}                exit: {(a, s1), (y, f1)}
   * f2:  return y          entry: {(a, s1), (y, f1)}            exit: {(a, s1), (y, f1)}
   * fx: }
   *
   * sx: main() {
   * s1:   a = 1            entry: {}                    exit: {(a, s1)}
   * s2:   b = sign(a)      entry: {(a, s1)}             exit: {(a, s1), (b, s2)} U {(a, s1)}
   * s3:   print b          entry: {(a, s1), (b, s2)}    exit: {(a, s1), (b, s2)}
   * s4: }
   */
  test("test_rd_function_var_not_assigned") {

    val f1 = AssignmentStmt(VariableExp("y"), MultiExp(VariableExp("y"), ConstExp(1)))
    val f2 = ReturnStmt(VariableExp("y"))
    val fSignBody = SequenceStmt(f1, f2)
    val fSign = FunDecl("fSign", List(VariableExp("x")), List(VariableExp("y")), fSignBody, VariableExp("y"))

    val s1 = AssignmentStmt(VariableExp("a"), ConstExp(1))
    val s2 = AssignmentStmt(VariableExp("b"), FunctionCallExp(NameExp(fSign.name), List(VariableExp("a"))))
    val s3 = OutputStmt(VariableExp("b"))

    //main function
    val fMainBody = SequenceStmt(s1, SequenceStmt(s2, s3))

    val fMain = FunDecl("main", List(), List(VariableExp("a"), VariableExp("b")), fMainBody, NullExp)

    val program = List(fSign, fMain)

    val RD = ReachingDefinition.run(fMainBody, program)

     assert( RD((s1, NopStmt)) == (
       Set(),
       Set(s1)
     ))

    assert(RD((s2, NopStmt)) == (
      Set(s1),
      Set(s1, s2)
    ))

    assert(RD((s3, NopStmt)) == (
      Set(s1, s2),
      Set(s1, s2)
    ))

    assert(RD((f1, s2)) == (
      Set(s1),
      Set(s1, f1)
    ))

    assert(RD((f2, s2)) == (
      Set(s1, f1),
      Set(s1, f1)
    ))
  }

  /**
   * fx: sign(a) {
   * f1:  a = a * -1        entry: {(a, s1)} U {}       exit: {(a, f1)}
   * f2:  return a          entry: {(a, f1)}            exit: {(a, f1)}
   * fx: }
   *
   * sx: main() {
   * s1:   a = 1            entry: {}                    exit: {(a, s1)}
   * s2:   b = sign(a)      entry: {(a, s1)}             exit: {(a, s1), (b, s2)} U {(a, f1)}
   * s3:   print b          entry: {(a, f1), (b, s2)}    exit: {(a, f1), (b, s2)}
   * s4: }
   */
  test("test_rd_function_var_reassigned") {

    val f1 = AssignmentStmt(VariableExp("a"), MultiExp(VariableExp("a"), ConstExp(1)))
    val f2 = ReturnStmt(VariableExp("a"))
    val fSignBody = SequenceStmt(f1, f2)
    val fSign = FunDecl("fSign", List(VariableExp("a")), List(), fSignBody, VariableExp("a"))

    val s1 = AssignmentStmt(VariableExp("a"), ConstExp(1))
    val s2 = AssignmentStmt(VariableExp("b"), FunctionCallExp(NameExp(fSign.name), List(VariableExp("a"))))
    val s3 = OutputStmt(VariableExp("b"))

    //main function
    val fMainBody = SequenceStmt(s1, SequenceStmt(s2, s3))

    val fMain = FunDecl("main", List(), List(VariableExp("a"), VariableExp("b")), fMainBody, NullExp)

    val program = List(fSign, fMain)

    val RD = ReachingDefinition.run(fMainBody, program)

    assert(RD((s1, NopStmt)) == (
      Set(),
      Set(s1)
    ))

    assert(RD((s2, NopStmt)) == (
      Set(s1),
      Set(s2, f1)
    ))

    assert(RD((s3, NopStmt)) == (
      Set(s2, f1),
      Set(s2, f1)
    ))

    assert(RD((f1, s2)) == (
      Set(s1),
      Set(f1)
    ))

    assert(RD((f2, s2)) == (
      Set(f1),
      Set(f1)
    ))
  }

  /**
   *
   * mymethod(x)
   * m1:  x = 5               entry:{(x,s1)} U {}                 exit:{(x,m1)} // entry gets the values from the CALLER
   *
   * main()
   * s1:   x = 1              entry:{}                                    exit:{(x,s1)}
   * s2:   y = 2              entry:{(x,s1)}                              exit:{(x,s1), (y,s2)}
   * s3:  z = mymethod(x)     entry:{(x,s1), (y,s2)}                      exit:{(x,s1), (y,s2), (z,s3)} U  {(x,m1)} = {(y,s2), (x, m1)} // exit get the values from the CALLEE
   * s4:  a = y + 1           entry:{(y,s2), (z,s3), (x, m1)}             exit:{(y,s2), (z,s3), (x,m1), (a,s4)}
   * s5:  b = x + 1           entry:{(y,s2), (z,s3), (x, m1), (a, s4)}    exit:{(y,s2), (z,s3), (x,m1), (a,s4), (b,s5)}
   *
   */
  test("test_rd_calling_function_one_time") {

    val m1 = AssignmentStmt(VariableExp("x"), ConstExp(5))
    val myFunction = FunDecl("myFunction", List(VariableExp("x")), List(), m1, NullExp)

    val s1 = AssignmentStmt(VariableExp("x"), ConstExp(1))
    val s2 = AssignmentStmt(VariableExp("y"), ConstExp(2))
    val s3 = AssignmentStmt(VariableExp("_m1"), FunctionCallExp(NameExp(myFunction.name), List(VariableExp("x"))))
    val s4 = AssignmentStmt(VariableExp("a"), AddExp(VariableExp("y"), ConstExp(1)))
    val s5 = AssignmentStmt(VariableExp("b"), AddExp(VariableExp("x"), ConstExp(1)))

    //main function
    val mainBody = SequenceStmt(s1, SequenceStmt(s2, SequenceStmt(s3, SequenceStmt(s4, s5))))

    val mainFunction = FunDecl("main", List(), List(VariableExp("x"), VariableExp("y"), VariableExp("z")), mainBody, NullExp)

    val program = List(myFunction, mainFunction)

    val RD = ReachingDefinition.run(mainBody, program)

    assert( RD((s1, NopStmt)) == (
      Set(),
      Set(s1)
    ))

    assert( RD((s2, NopStmt)) == (
      Set(s1),
      Set(s1, s2)
    ))

    assert(RD((s3, NopStmt)) == (
      Set(s1, s2),
      Set(s2, s3, m1)
    ))

    assert(RD((m1, s3)) == (
      Set(s1),
      Set(m1),
    ))

    assert( RD((s4, NopStmt)) == (
      Set(s2, s3, m1),
      Set(s2, s3, m1, s4)
    ))

    assert(RD((s5, NopStmt)) == (
      Set(s2, s3, m1, s4),
      Set(s2, s3, m1, s4, s5)
    ))
  }
}