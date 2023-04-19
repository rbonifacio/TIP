package br.unb.cic.tip.parser

import org.scalatest.funsuite.AnyFunSuite
import br.unb.cic.tip.*
import br.unb.cic.tip.utils._
import br.unb.cic.tip.utils.Expression.*
import br.unb.cic.tip.utils.{FunDecl, TipParser}

class TipParserTest extends AnyFunSuite {
  test("should parse successfully an empty string") {
    val result = TipParser.parse("")

    assert(result.get == List())
  }

  test("should parse example program 1") {
    val input = """
        iterate(n) {
            var f;
            f = 1;
            while (n>0) {
                f = f*n;
                n = n-1;
            }
            return f;
        }
    """

    val result = TipParser.parse(input)

    assert(
      result.get == List(
        FunDecl(
          "iterate",
          List("n"),
          List("f"),
          SequenceStmt(
            AssignmentStmt("f", ConstExp(1)),
            WhileStmt(
              GTExp(VariableExp("n"), ConstExp(0)),
              SequenceStmt(
                AssignmentStmt(
                  "f",
                  MultiExp(VariableExp("f"), VariableExp("n"))
                ),
                AssignmentStmt("n", SubExp(VariableExp("n"), ConstExp(1)))
              )
            )
          ),
          VariableExp("f")
        )
      )
    )
  }

  test("should parse example program 2") {
    val input = """
        recurse(n) {
            var f;
            if (n==0) { f=1; }
            else { f=n*recurse(n-1); }
            return f;
        }
    """

    val result = TipParser.parse(input)

    assert(
      result.get == List(
        FunDecl(
          "recurse",
          List("n"),
          List("f"),
          IfElseStmt(
            EqExp(VariableExp("n"), ConstExp(0)),
            AssignmentStmt("f", ConstExp(1)),
            Some(
              AssignmentStmt(
                "f",
                MultiExp(
                  VariableExp("n"),
                  FunctionCallExp(
                    VariableExp("recurse"),
                    List(SubExp(VariableExp("n"), ConstExp(1)))
                  )
                )
              )
            )
          ),
          VariableExp("f")
        )
      )
    )
  }

  test("should parse example program 3") {
    val input = """
        foo(p,x) {
            var f,q;
            if ((*p)==0) { f=1; }
            else {
                q = alloc 0;
                *q = (*p)-1;
                f=(*p)*(x(q,x));
            }
            return f;
        }

        main() {
            var n;
            n = input;
            return foo(&n,foo);
        }
    """

    val result = TipParser.parse(input)

    assert(
      result.get == List(
        FunDecl(
          "foo",
          List("p", "x"),
          List("f", "q"),
          IfElseStmt(
            EqExp(BracketExp(LoadExp(VariableExp("p"))), ConstExp(0)),
            AssignmentStmt("f", ConstExp(1)),
            Some(
              SequenceStmt(
                AssignmentStmt("q", AllocExp(ConstExp(0))),
                SequenceStmt(
                  StoreStmt(
                    VariableExp("q"),
                    SubExp(BracketExp(LoadExp(VariableExp("p"))), ConstExp(1))
                  ),
                  AssignmentStmt(
                    "f",
                    MultiExp(
                      BracketExp(LoadExp(VariableExp("p"))),
                      BracketExp(
                        FunctionCallExp(
                          VariableExp("x"),
                          List(VariableExp("q"), VariableExp("x"))
                        )
                      )
                    )
                  )
                )
              )
            )
          ),
          VariableExp("f")
        ),
        FunDecl(
          "main",
          List(),
          List("n"),
          AssignmentStmt("n", InputExp),
          FunctionCallExp(
            VariableExp("foo"),
            List(LocationExp("n"), VariableExp("foo"))
          )
        )
      )
    )
  }
}
