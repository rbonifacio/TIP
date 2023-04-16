package br.unb.cic.tip.parser

import org.scalatest.funsuite.AnyFunSuite
import br.unb.cic.tip.*
import br.unb.cic.tip.utils.Stmt.*
import br.unb.cic.tip.utils.Expression.*
import br.unb.cic.tip.utils.StatementParser

class StatementParserTest extends AnyFunSuite {
  test("should parse empty strings") {
    val result = StatementParser.parse("")

    assert(result.successful)
  }

  test("should parse assignments") {
    val result = StatementParser.parse("v = exp;")

    assert(result.get == AssignmentStmt("v", VariableExp("exp")))
  }

  test("should parse output statements") {
    val result = StatementParser.parse("output exp ;")

    assert(result.get == OutputStmt(VariableExp("exp")))
  }

  test("should parse statement sequences") {
    val result = StatementParser.parse("output exp ; x = 3;")

    assert(
      result.get == SequenceStmt(
        OutputStmt(VariableExp("exp")),
        AssignmentStmt("x", ConstExp(3))
      )
    )
  }

  test("should parse ifs") {
    val input = """
      if (v) {
        x = 0;
      }
    """

    val result = StatementParser.parse(input)

    assert(
      result.get == IfElseStmt(
        VariableExp("v"),
        AssignmentStmt("x", ConstExp(0)),
        None
      )
    )
  }

  test("should parse if with else") {
    val input = """
      if (v) {
        x = 0;
      } else {
        output 8;
      }
    """

    val result = StatementParser.parse(input)

    assert(
      result.get == IfElseStmt(
        VariableExp("v"),
        AssignmentStmt("x", ConstExp(0)),
        Some(OutputStmt(ConstExp(8)))
      )
    )
  }

  test("should parse while") {
    val input = """
      while(8 == 5) {
        id = v + 7;
      }
    """

    val result = StatementParser.parse(input)

    assert(
      result.get == WhileStmt(
        EqExp(ConstExp(8), ConstExp(5)),
        AssignmentStmt("id", AddExp(VariableExp("v"), ConstExp(7)))
      )
    )
  }

  test("should parse store") {
    val input = """
      * v = 5;
    """

    val result = StatementParser.parse(input)

    assert(
      result.get == StoreStmt(VariableExp("v"), ConstExp(5))
    )
  }

  test("should parse store with expression") {
    val input = """
      *val.b  = 5;
    """

    val result = StatementParser.parse(input)

    assert(
      result.get == StoreStmt(FieldAccess(VariableExp("val"), "b"), ConstExp(5))
    )
  }

  test("should parse record assignmment") {
    val input = """
      id . a = 8;
    """

    val result = StatementParser.parse(input)

    assert(
      result.get == RecordAssignmentStmt("id", "a", ConstExp(8))
    )
  }

  test("should parse record store") {
    val input = """
      (*val).b  = 6;
    """

    val result = StatementParser.parse(input)

    assert(
      result.get == RecordStoreStmt(VariableExp("val"), "b", ConstExp(6))
    )
  }

  def failureHelper(input: String) =
    val result = StatementParser.parse(input)
    try {
      assert(result.successful == false)
    } catch {
      case e => {
        println("Failure ----------")
        println(input + ">>>>>>>" + result)
        throw e
      }
    }

  test("should fail on missing if condition") {
    failureHelper("if() {}")
  }

  test("should fail on missing if parenthesis") {
    failureHelper("if ")
  }

  test("should fail on missing if closing parenthesis") {
    failureHelper("if ( ")
  }

  test("should fail on missing if brackets") {
    failureHelper("if()")
  }

  test("should fail on missing if closing brackets") {
    failureHelper("if(0) {")
  }

  test("should fail on missing else brackets") {
    failureHelper("if(0) {} else ")
  }

  test("should fail on missing else closing brackets") {
    failureHelper("if(0) {} else {")
  }

  test("should fail on missing while condition") {
    failureHelper("while() {}")
  }

  test("should fail on missing while parenthesis") {
    failureHelper("while")
  }

  test("should fail on missing while closing parenthesis") {
    failureHelper("while(")
  }

  test("should fail on missing while brackets") {
    failureHelper("while(0)")
  }

  test("should fail on missing while closing brackets") {
    failureHelper("while(0) {")
  }

  test("should fail on missing expression in assignment") {
    failureHelper("e = ;")
  }

  test("should fail on asterisc only store") {
    failureHelper("* ")
  }

  test("should fail on missing first expression in store") {
    failureHelper("* = 3;")
  }

  test("should fail on missing second expression in store") {
    failureHelper("* e = ;")
  }

  test("should fail on missing second id in record assignment") {
    failureHelper("sla . = 3;")
  }

  test("should fail on missing expression in record assignment") {
    failureHelper("sla . a = ;")
  }

  test("should fail on missing first expression in record store") {
    failureHelper("(* ) . id = 0;")
  }

  test("should fail on missing id in record store") {
    failureHelper("(* exp) . = 0;")
  }

  test("should fail on missing second expression in record store") {
    failureHelper("( * exp ) . id = ;")
  }
}
