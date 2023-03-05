package br.unb.cic.tip

import org.scalatest.funsuite.AnyFunSuite
import br.unb.cic.tip.*

import br.unb.cic.tip.Stmt.*
import br.unb.cic.tip.Expression.*
import java.lang.reflect.Field

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
        null
        // FieldAccess(
        //   FieldAccess(
        //     AddExp(RecordExp(List(("a", ConstExp(4)))), ConstExp(2)),
        //     "b"
        //   ),
        //   "c"
        // )
    )
  }

  test("should parse direct function call no args") {
    val result = ExpressionParser.parse("foo()")

    assert(result.get == DirectFunctionCallExp("foo", Nil))
  }

  test("should parse direct function call with args") {
    val result = ExpressionParser.parse("foo(1, b, {j : 0})")

    assert(
      result.get == DirectFunctionCallExp(
        "foo",
        List(ConstExp(1), VariableExp("b"), RecordExp(List(("j", ConstExp(0)))))
      )
    )
  }

  test("should parse indirect function call no args") {
    val result = ExpressionParser.parse("foo.a()")

    assert(
      result.get == IndirectFunctionCallExp(
        FieldAccess(VariableExp("foo"), "a"),
        List()
      )
    )
  }

  test("should parse indirect function call curried no args") {
    val result = ExpressionParser.parse("foo.a().b()")

    assert(
      result.get == IndirectFunctionCallExp(
        FieldAccess(
          IndirectFunctionCallExp(
            FieldAccess(VariableExp("foo"), "a"),
            List()
          ),
          "b"
        ),
        Nil
      )
    )
  }
}
/* Exp → Id ( Exp,. . .,Exp )
 *     | Exp ( Exp , . . ., Exp )
 */

class TipParserTest extends AnyFunSuite {
  test("should parse successfully an empty string") {
    val result = TipParser.parse("")

    assert(result.get == List())
  }

}
