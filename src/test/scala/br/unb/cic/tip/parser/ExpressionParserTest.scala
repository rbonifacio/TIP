package br.unb.cic.tip.parser

import org.scalatest.funsuite.AnyFunSuite
import br.unb.cic.tip.*
import br.unb.cic.tip.utils.Stmt.*
import br.unb.cic.tip.utils.Expression.*
import br.unb.cic.tip.utils.ExpressionParser

/* Exp → Int
 *     | Id
 *     | Exp + Exp | Exp - Exp | Exp * Exp | Exp / Exp | Exp > Exp | Exp == Exp
 *     | ( Exp )
 *     | input
 *     | Id ( Exp,. . .,Exp )
 *     | Exp ( Exp , . . ., Exp )
 *     | alloc Exp
 *     | & Id
 *     | * Exp
 *     | null.
 *     | { Id : Exp , . . ., Id : Exp }
 *     | Exp . Id
 */
class ExpressionParserTest extends AnyFunSuite {

  test("should fail to parse an empty string") {
    val result = ExpressionParser.parse("")

    assert(result.successful == false)
  }

  test("should parse a positive number") {
    val result = ExpressionParser.parse("123456")

    assert(result.get == ConstExp(123456))
  }

  test("should parse a negative number") {
    val result = ExpressionParser.parse("-7890")

    assert(result.get == ConstExp(-7890))
  }

  test("should parse variable expression") {
    val result = ExpressionParser.parse("variable")

    assert(result.get == VariableExp("variable"))
  }

  test("should parse addition operation") {
    val result = ExpressionParser.parse("8 + variable")

    assert(result.get == AddExp(ConstExp(8), VariableExp("variable")))
  }

  test("should parse subtraction operation") {
    val result = ExpressionParser.parse("8 - variable")

    assert(result.get == SubExp(ConstExp(8), VariableExp("variable")))
  }

  test("should parse multiplication operation") {
    val result = ExpressionParser.parse("8 * variable")

    assert(result.get == MultiExp(ConstExp(8), VariableExp("variable")))
  }

  test("should parse division operation") {
    val result = ExpressionParser.parse("8 / variable")

    assert(result.get == DivExp(ConstExp(8), VariableExp("variable")))
  }

  test("should parse greater than operation") {
    val result = ExpressionParser.parse("8 > variable")

    assert(result.get == GTExp(ConstExp(8), VariableExp("variable")))
  }

  test("should parse equals operation") {
    val result = ExpressionParser.parse("8 == variable")

    assert(result.get == EqExp(ConstExp(8), VariableExp("variable")))
  }

  test("should parse equation with varibale and brackets") {
    val result = ExpressionParser.parse("(8) - (variable * 3)")

    assert(
      result.get ==
        SubExp(
          BracketExp(ConstExp(8)),
          BracketExp(MultiExp(VariableExp("variable"), ConstExp(3)))
        )
    )
  }

  test("should parse equation with complex expression") {
    val result = ExpressionParser.parse("8 + {a: 3}.b(5) * 2")

    assert(
      result.get == AddExp(
        ConstExp(8),
        MultiExp(
          FunctionCallExp(
            FieldAccess(RecordExp(List(("a", ConstExp(3)))), "b"),
            List(ConstExp(5))
          ),
          ConstExp(2)
        )
      )
    )
  }

  test("should parse input expression") {
    val result = ExpressionParser.parse("input")

    assert(result.get == InputExp)
  }

  test("should parse allocation expression") {
    val result = ExpressionParser.parse("alloc 3")

    assert(result.get == AllocExp(ConstExp(3)))
  }

  test("should parse location expression") {
    val result = ExpressionParser.parse("& variable")

    assert(result.get == LocationExp("variable"))

  }

  test("should parse load expression") {
    val result = ExpressionParser.parse("* (variable + 1)")

    assert(
      result.get ==
        LoadExp(
          BracketExp(AddExp(VariableExp("variable"), ConstExp(1)))
        )
    )
  }

  test("should parse null expression") {
    val result = ExpressionParser.parse("null")

    assert(result.get == NullExp)
  }

  test("should parse record expression") {
    val result = ExpressionParser.parse("{a : 3}")

    assert(result.get == RecordExp(List(("a", ConstExp(3)))))
  }

  test("should parse complex record expression") {
    val result = ExpressionParser.parse("{a : 3, subRecord: ({b: 65})}")

    assert(
      result.get == RecordExp(
        List(
          ("a", ConstExp(3)),
          ("subRecord", BracketExp(RecordExp(List(("b", ConstExp(65))))))
        )
      )
    )
  }

  test("should parse field access") {
    val result = ExpressionParser.parse("variable.name")

    assert(result.get == FieldAccess(VariableExp("variable"), "name"))
  }

  test("should parse field access on field access") {
    val result = ExpressionParser.parse("a.b.c")

    assert(result.get == FieldAccess(FieldAccess(VariableExp("a"), "b"), "c"))
  }

  test("should parse complex field access") {
    val result = ExpressionParser.parse("{a : 4}.a + b.c")

    assert(
      result.get ==
        AddExp(
          FieldAccess(RecordExp(List(("a", ConstExp(4)))), "a"),
          FieldAccess(VariableExp("b"), "c")
        )
    )
  }

  test("should parse direct function call no args") {
    val result = ExpressionParser.parse("foo()")

    assert(result.get == FunctionCallExp(VariableExp("foo"), Nil))
  }

  test("failure: should parse direct function call with args") {
    val result = ExpressionParser.parse("foo(1, b, {j : 0})")

    assert(
      result.get == FunctionCallExp(
        VariableExp("foo"),
        List(ConstExp(1), VariableExp("b"), RecordExp(List(("j", ConstExp(0)))))
      )
    )
  }

  test("should parse indirect function call no args") {
    val result = ExpressionParser.parse("foo.a()")

    assert(
      result.get == FunctionCallExp(
        FieldAccess(VariableExp("foo"), "a"),
        List()
      )
    )
  }

  test("failure: should parse indirect higher order function call") {
    val result = ExpressionParser.parse("foo()(a)(b, c)")

    assert(
      result.get == FunctionCallExp(
        FunctionCallExp(
          FunctionCallExp(
            VariableExp("foo"),
            Nil
          ),
          List(VariableExp("a"))
        ),
        List(VariableExp("b"), VariableExp("c"))
      )
    )
  }

  test("failure: should parse indirect function call curried with args") {
    val result = ExpressionParser.parse("foo.a().b(a, b, c)")

    assert(
      result.get == FunctionCallExp(
        FieldAccess(
          FunctionCallExp(
            FieldAccess(VariableExp("foo"), "a"),
            Nil
          ),
          "b"
        ),
        List(VariableExp("a"), VariableExp("b"), VariableExp("c"))
      )
    )
  }

  def failureHelper(input: String) =
    val result = ExpressionParser.parse(input)
    try {
      assert(result.successful == false)
    } catch {
      case e => {
        println("Failure ----------")
        println(input + ">>>>>>>" + result)
        throw e
      }
    }

  test("should fail on missing id for location") {
    failureHelper("& ")
  }

  test("should fail on missing expression for load") {
    failureHelper("* ")
  }

  test("should fail on dangling comma") {
    failureHelper("call(test,)")
  }

  test("should fail on missing second argument") {
    failureHelper("call (test, ")
  }

  test("should fail on missing closing parenthesis") {
    failureHelper("call (test")

  }

  test("should fail on trailing comma") {
    failureHelper("call(, test)")
  }

  test("should fail on missing closing parenthesis without args") {
    failureHelper("call (")
  }

  test("should fail on missing closing parenthesis on DirectFunctionCall") {
    failureHelper("call (test")
  }

  test("should fail on missing closing parenthesis on IndirectFunctionCall") {
    failureHelper("a.b (test")
  }

  test("should fail on missing closing parenthesis on Brackets") {
    failureHelper("(test")
  }

  test("should fail on empty record") {
    failureHelper("{}")
  }

  test("should fail on empty brackets") {
    failureHelper("()")
  }

  test("should fail on missing second operand") {
    failureHelper("3 + ")
  }

  test("should fail on value missing on record") {
    failureHelper("{sla: }")
  }

  test("should fail on missing id on field access") {
    failureHelper("{sla: 3} . ")
  }
}
