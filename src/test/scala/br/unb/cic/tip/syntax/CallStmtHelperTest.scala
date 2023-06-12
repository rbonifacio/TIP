package br.unb.cic.tip.syntax

import br.unb.cic.tip.{callStatement, flow, functions}
import br.unb.cic.tip.utils.{FunDecl}
import br.unb.cic.tip.utils.*
import br.unb.cic.tip.utils.Node.*
import br.unb.cic.tip.utils.Stmt.*
import org.scalatest.funsuite.AnyFunSuite

class CallStmtHelperTest extends AnyFunSuite {

  test("find_call_stmt") {
    val stmt = CallStmt(AssignmentStmt(VariableExp("_m1"), FunctionCallExp("my_function", List(VariableExp("x")))))
    val node = SimpleNode(stmt)
    assert(callStatement(node) == Set(stmt))
  }

  test("find_call_stmt_using_simple_node") {
    val stmt = AssignmentStmt(VariableExp("x"), ConstExp(2))
    val node = SimpleNode(stmt)
    assert(callStatement(node) == Set())
  }

  test("find_call_stmt_using_start_node") {
    val node = StartNode("my_function")
    assert(callStatement(node) == Set())
  }

  test("find_call_stmt_using_end_node") {
    val node = EndNode("my_function")
    assert(callStatement(node) == Set())
  }

  /**
   * mymethod(a)
   * a = 999
   *
   * main()
   * x = 1
   * y = 2
   * mymethod(x)
   * z = 3
   */
  test("test_rd_using_only_statements") {

    val m1 = AssignmentStmt(VariableExp("a"), ConstExp(999))
    val myFunction = FunDecl("myFunction", List(VariableExp("x")), List(), m1, NullExp)

    val s1 = AssignmentStmt(VariableExp("x"), ConstExp(1))
    val s2 = AssignmentStmt(VariableExp("y"), ConstExp(2))
    val s3 = AssignmentStmt(VariableExp("_m1"), FunctionCallExp(myFunction.name, List(VariableExp("x"))))
    val s4 = AssignmentStmt(VariableExp("z"), ConstExp(3))

    //main function
    val mainBody = SequenceStmt(s1, SequenceStmt(s2, SequenceStmt(s3, s4)))

    val mainFunction = FunDecl("main", List(), List(VariableExp("x"), VariableExp("y"), VariableExp("z")), mainBody, NullExp)

    val program = List(myFunction, mainFunction)

    assert(callStatement(flow(program)) == Set(
      CallStmt(s3),
      AfterCallStmt(s3)
    ))
  }

  test("test_get_functions_names") {

    val myFunction1 = FunDecl("my_function_1", List(), List(), NopStmt, NullExp)

    val mainFunction = FunDecl("main", List(), List(), NopStmt, NullExp)

    val program = List(mainFunction, myFunction1)

    assert(functions(program) == Set(
      myFunction1.name,
      mainFunction.name
    ))
  }
}
