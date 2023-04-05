package br.unb.cic.tip.parser

import org.scalatest.funsuite.AnyFunSuite
import br.unb.cic.tip.*

import br.unb.cic.tip.Stmt.*
import br.unb.cic.tip.Expression.*

class FunctionParserTest extends AnyFunSuite {
  test("should fail to parse empty strings") {
    val result = FunctionParser.parse("")

    assert(result.successful == false)
  }

  test("should parse simple function") {
    val result = FunctionParser.parse("a () { return 0; }")

    assert(result.get == FunDecl("a", List(), List(), NopStmt, ConstExp(0)))
  }

  test("should parse function with argument") {
    val result = FunctionParser.parse("a (a) { return 0; }")

    assert(result.get == FunDecl("a", List("a"), List(), NopStmt, ConstExp(0)))
  }

  test("should parse function with arguments") {
    val result = FunctionParser.parse("a (a, b, c) { return 0; }")

    assert(
      result.get == FunDecl(
        "a",
        List("a", "b", "c"),
        List(),
        NopStmt,
        ConstExp(0)
      )
    )
  }

  test("should parse function with body") {
    val input = """
        a (a, b, c) {
            i = 3;
            return 0;
        }
    """

    val result = FunctionParser.parse(input)

    assert(
      result.get == FunDecl(
        "a",
        List("a", "b", "c"),
        List(),
        AssignmentStmt("i", ConstExp(3)),
        ConstExp(0)
      )
    )
  }

  test("should parse function with local variable") {
    val input = """
        a (a) {
            var i;
            return 0;
        }
    """

    val result = FunctionParser.parse(input)

    assert(
      result.get == FunDecl("a", List("a"), List("i"), NopStmt, ConstExp(0))
    )
  }

  test("should parse function with local variables") {
    val input = """
        a (a) {
            var i, j, k;
            return 0;
        }
    """

    val result = FunctionParser.parse(input)

    assert(
      result.get == FunDecl(
        "a",
        List("a"),
        List("i", "j", "k"),
        NopStmt,
        ConstExp(0)
      )
    )
  }

  test("should parse complex function") {
    val input = """
        fact (n) {
            var i, ret;
            i = n;
            ret = n;

            if(i == 0) {
                ret = 1;
            }

            while(i > 1) {
                ret = ret * i;
                i = i - 1;
            }

            return ret;
        }
    """

    val result = FunctionParser.parse(input)

    assert(
      result.get == FunDecl(
        "fact",
        List("n"),
        List("i", "ret"),
        SequenceStmt(
          AssignmentStmt("i", VariableExp("n")),
          SequenceStmt(
            AssignmentStmt("ret", VariableExp("n")),
            SequenceStmt(
              IfElseStmt(
                EqExp(VariableExp("i"), ConstExp(0)),
                AssignmentStmt("ret", ConstExp(1)),
                None
              ),
              WhileStmt(
                GTExp(VariableExp("i"), ConstExp(1)),
                SequenceStmt(
                  AssignmentStmt(
                    "ret",
                    MultiExp(VariableExp("ret"), VariableExp("i"))
                  ),
                  AssignmentStmt("i", SubExp(VariableExp("i"), ConstExp(1)))
                )
              )
            )
          )
        ),
        VariableExp("ret")
      )
    )
  }

  def failureHelper(input: String) =
    val result = FunctionParser.parse(input)
    try {
      assert(result.successful == false)
    } catch {
      case e => {
        println("Failure ----------")
        println(input + ">>>>>>>" + result)
        throw e
      }
    }

  test("should fail on missing id") {
    failureHelper("() {}")
  }

  test("should fail on missing brackets") {
    failureHelper("foo")
  }

  test("should fail on missing closing brackets") {
    failureHelper("foo( {}")
  }

  test("should fail on missing closing brackets with args") {
    failureHelper("foo(test {}")
  }

  test("should fail on missing second argument") {
    failureHelper("foo(test,) {}")
  }

  test("should fail on missing first argument") {
    failureHelper("foo(,test) {}")
  }

  test("should fail on missing return statement") {
    val input = """
        fact (n) {}
    """
    failureHelper(input)
  }

  test("should fail on bad return statement") {
    val input = """
        fact (n) {
          return
        }
    """
    failureHelper(input)
  }

  test("should fail on missing return semicolon") {
    val input = """
        fact (n) {
          return 0
        }
    """
    failureHelper(input)
  }

  test("should fail on missing variables on declaration") {
    val input = """
        fact (n) {
          var 
          return 0;
        }
    """
    failureHelper(input)
  }

  test("should fail on missing second variable") {
    val input = """
        fact (n) {
          var a,
          return 0;
        }
    """
    failureHelper(input)
  }

  test("should fail on missing semicolon on variable declaration") {
    val input = """
        fact (n) {
          var a, b
          return 0;
        }
    """
    failureHelper(input)
  }

  test("should fail on failing statement") {
    val input = """
        fact (n) {
          failureStmt
          return 0;
        }
    """
    failureHelper(input)
  }

}
