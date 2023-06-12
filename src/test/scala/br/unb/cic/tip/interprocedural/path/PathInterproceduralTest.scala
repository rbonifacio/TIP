package br.unb.cic.tip.interprocedural.path

import br.unb.cic.tip.{exportDot, flow, isValidPath, path}
import br.unb.cic.tip.utils.{Expression, FunDecl, Stmt, VariableExp, *}
import br.unb.cic.tip.utils.Node.*
import br.unb.cic.tip.utils.Stmt.*
import org.scalatest.funsuite.AnyFunSuite

class PathInterproceduralTest extends AnyFunSuite {

  /**
   * sum(x, y) {
   * z = x + y
   * return z
   * }
   *
   * main() {
   * a = 1
   * b = 1
   * c = sum(a, b)
   * }
   */

  test("path_f-sum_simple_call") {
    //sum function
    val sumS1 = AssignmentStmt(VariableExp("z"), AddExp(VariableExp("x"), VariableExp("y")))
    val sumS2 = ReturnStmt(VariableExp("z"))
    val sumBody = SequenceStmt(sumS1, sumS2)
    val sumFunction = FunDecl("sum", List(VariableExp("x"), VariableExp("y")), List(VariableExp("z")), sumBody, VariableExp("z"))

    //main function
    val mainS1 = AssignmentStmt(VariableExp("a"), ConstExp(1))
    val mainS2 = AssignmentStmt(VariableExp("b"), ConstExp(1))
    val mainS3 = AssignmentStmt(VariableExp("c"), FunctionCallExp(NameExp(sumFunction.name), List(VariableExp("a"), VariableExp("b"))))
    val mainS4 = OutputStmt(VariableExp("c"))
    val mainBody =
      SequenceStmt(mainS1,
        SequenceStmt(mainS2,
          SequenceStmt(mainS3, mainS4)
        )
      )

    val mainFunction = FunDecl("main", List(), List(VariableExp("a"), VariableExp("b"), VariableExp("c")), mainBody, NullExp)

    val program = List(sumFunction, mainFunction)

    val cfg = flow(program)

    val paths = path(cfg, mainFunction.name)

    println(s"${paths.size} paths where found.")

//    paths.foreach(x => {
//      println(x.size)
//      println(s"is valid path: ${isValidPath(x)}")
//      println(exportDot(cfg, x))
//    })
  }

  /**
   sum(x, y) {
    z = x + y
    return z
   }

   main() {
    a = 1
    b = 1
    c = sum(a, b)
    print c
    d = 1
    e = 1
    f = sum(d, e)
    print f
   }
   */

  test("path_f-sum_two_calls\"") {
    //sum function
    val sumS1 = AssignmentStmt(VariableExp("z"), AddExp(VariableExp("x"), VariableExp("y")))
    val sumS2 = ReturnStmt(VariableExp("z"))
    val sumBody = SequenceStmt(sumS1, sumS2)
    val sumFunction = FunDecl("sum", List(VariableExp("x"), VariableExp("y")), List(VariableExp("z")), sumBody, VariableExp("z"))

    //main function
    val mainS1 = AssignmentStmt(VariableExp("a"), ConstExp(1))
    val mainS2 = AssignmentStmt(VariableExp("b"), ConstExp(1))
    val mainS3 = AssignmentStmt(VariableExp("c"), FunctionCallExp(NameExp(sumFunction.name), List(VariableExp("a"), VariableExp("b"))))
    val mainS4 = OutputStmt(VariableExp("c"))
    val mainS5 = AssignmentStmt(VariableExp("d"), ConstExp(1))
    val mainS6 = AssignmentStmt(VariableExp("e"), ConstExp(1))
    val mainS7 = AssignmentStmt(VariableExp("f"), FunctionCallExp(NameExp(sumFunction.name), List(VariableExp("d"), VariableExp("e"))))
    val mainS8 = OutputStmt(VariableExp("f"))
    val mainBody =
      SequenceStmt(mainS1,
        SequenceStmt(mainS2,
          SequenceStmt(mainS3,
            SequenceStmt(mainS4,
              SequenceStmt(mainS5,
                SequenceStmt(mainS6,
                  SequenceStmt(mainS7, mainS8
                    )))))))

    val mainFunction = FunDecl(
      "main",
      List(),
      List(VariableExp("a"), VariableExp("b"), VariableExp("c"), VariableExp("d"), VariableExp("e"), VariableExp("f")),
      mainBody,
      NullExp
    )

    val program = List(sumFunction, mainFunction)

    val cfg = flow(program)

    val paths = path(cfg, mainFunction.name)

    println(s"${paths.size} paths where found.")

    paths.foreach(x => {
      println(x.size)
      println(s"is valid path: ${isValidPath(x)}")
      println(exportDot(cfg, x))
    })
  }

  test("path_from_fibonacci") {
    //fibonacci function
    val fibonacciBodyIf: Stmt = AssignmentStmt(VariableExp("v"), AddExp(VariableExp("u"), ConstExp(1)))
    val fibonacciBodyElseS1: Stmt = AssignmentStmt(VariableExp("_f1"), FunctionCallExp(NameExp("fibonacci"), List(SubExp(VariableExp("z"), ConstExp(1)), VariableExp("u"), VariableExp("v"))))
    val fibonacciBodyElseS2: Stmt = AssignmentStmt(VariableExp("_f2"), FunctionCallExp(NameExp("fibonacci"), List(SubExp(VariableExp("z"), ConstExp(2)), VariableExp("u"), VariableExp("v"))))
    val fibonacciBodyElse: Stmt = SequenceStmt(fibonacciBodyElseS1, fibonacciBodyElseS2)
    val fibonacciBody: Stmt = IfElseStmt(GTExp(VariableExp("z"), ConstExp(3)), fibonacciBodyIf, Some(fibonacciBodyElse))

    val fibonacciFunction = FunDecl("fibonacci", List(VariableExp("z"), VariableExp("u"), VariableExp("v")), List(), fibonacciBody, VariableExp("v"))

    //main function
    val mainBody = AssignmentStmt(VariableExp("_m1"), FunctionCallExp(NameExp(fibonacciFunction.name), List(VariableExp("x"), ConstExp(0), VariableExp("y"))))

    val mainFunction = FunDecl(
      "main",
      List(),
      List(VariableExp("a"), VariableExp("b"), VariableExp("c"), VariableExp("d"), VariableExp("e"), VariableExp("f")),
      mainBody,
      NullExp
    )

    val program = List(fibonacciFunction, mainFunction)

    val cfg = flow(program)

    val paths = path(cfg, mainFunction.name)
//    paths.foreach(x => println(exportDot(cfg, x)))
  }
}