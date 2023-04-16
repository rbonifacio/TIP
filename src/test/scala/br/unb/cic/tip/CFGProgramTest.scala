package br.unb.cic.tip

import br.unb.cic.tip.utils.*
import br.unb.cic.tip.utils.Expression.*
import br.unb.cic.tip.utils.Node.*
import br.unb.cic.tip.utils.Stmt.*
import org.scalatest.funsuite.AnyFunSuite

class CFGProgramTest extends AnyFunSuite {

  test("sum program") {
    //sum function
    val sumS1 = AssignmentStmt("z", AddExp(VariableExp("x"), VariableExp("y")))
    val sumS2 = ReturnStmt(VariableExp("z"))
    val sumBody = SequenceStmt(sumS1, sumS2)
    val sumFunction = FunDecl("sum", List("x", "y"), List("z"), sumBody, VariableExp("z"))

    //main function
    val mainS1 = AssignmentStmt("a", ConstExp(1))
    val mainS2 = AssignmentStmt("b", ConstExp(1))
    val mainS3 = AssignmentStmt("c", FunctionCallExp(NameExp(sumFunction.name), List(VariableExp("a"), VariableExp("b"))))
    val mainS4 = OutputStmt(VariableExp("c"))
    val mainS5 = AssignmentStmt("d", ConstExp(1))
    val mainS6 = AssignmentStmt("e", ConstExp(1))
    val mainS7 = AssignmentStmt("f", FunctionCallExp(NameExp(sumFunction.name), List(VariableExp("d"), VariableExp("e"))))
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

    val mainFunction = FunDecl("main", List(), List("a", "b", "c","d", "e", "f"), mainBody, NullExp)

    val program = List(sumFunction, mainFunction)

    val cfg = flow(program)
//    println(exportDot(cfg))
  }

  test("fibonacci program") {
    //fibonacci function
    val fibonacciBodyIf: Stmt = AssignmentStmt("v", AddExp(VariableExp("u"), ConstExp(1)))
    val fibonacciBodyElseS1: Stmt = AssignmentStmt("_f1", FunctionCallExp(NameExp("fibonacci"), List(SubExp(VariableExp("z"), ConstExp(1)), VariableExp("u"), VariableExp("v"))))
    val fibonacciBodyElseS2: Stmt = AssignmentStmt("_f2", FunctionCallExp(NameExp("fibonacci"), List(SubExp(VariableExp("z"), ConstExp(2)), VariableExp("u"), VariableExp("v"))))
    val fibonacciBodyElse: Stmt = SequenceStmt(fibonacciBodyElseS1, fibonacciBodyElseS2)
    val fibonacciBody: Stmt = IfElseStmt(GTExp(VariableExp("z"), ConstExp(3)), fibonacciBodyIf, Some(fibonacciBodyElse))

    val fibonacciFunction = FunDecl("fibonacci", List("z", "u", "v"), List(), fibonacciBody, VariableExp("v"))

    //main function
    val mainBody = AssignmentStmt("_m1", FunctionCallExp(NameExp(fibonacciFunction.name), List(VariableExp("x"), ConstExp(0), VariableExp("y"))))

    val mainFunction = FunDecl("main", List(), List("a", "b", "c","d", "e", "f"), mainBody, NullExp)

    val program = List(fibonacciFunction, mainFunction)

    val cfg = flow(program)
    println(exportDot(cfg))
  }
}