package br.unb.cic.tip

import org.scalatest.funsuite.AnyFunSuite
import br.unb.cic.tip.Expression.*
import br.unb.cic.tip.Node.*
import br.unb.cic.tip.Stmt.{AssignmentStmt, *}

class PathTest extends AnyFunSuite {

  /**
   x = 1
   z = 3
   print(z)
   */
  test("path_from_stmt") {
    val s1 = AssignmentStmt("x", ConstExp(1))
    val s2 = AssignmentStmt("z", ConstExp(3))
    val s3 = OutputStmt(VariableExp("x"))

    val body = SequenceStmt(s1, SequenceStmt(s2, s3))
    val function = FunDecl("f_stmt", List(), List(), body, NullExp)
    val cfg = flow(function)

    val paths = path(cfg, function.name)

    val path1 = List(
      StartNode(function.name),
      SimpleNode(s1),
      SimpleNode(s2),
      SimpleNode(s3),
      EndNode(function.name)
    )

    assert(Set(path1) == paths)
  }

  /**
   a = 1
   if (x == 2) {
      b = 2
      c = 3
   } else {
    d = 4
   }
   e = 5
   */
  test("path_from_if_else") {
    val s1 = AssignmentStmt("a", ConstExp(1))
    val s2 = AssignmentStmt("b", ConstExp(2))
    val s3 = AssignmentStmt("c", ConstExp(3))
    val s4 = AssignmentStmt("d", ConstExp(4))
    val s5 = IfElseStmt(EqExp(VariableExp("x"),ConstExp(2)), SequenceStmt(s2,s3), Some(s4))
    val s6 = AssignmentStmt("e", ConstExp(5))

    val body = SequenceStmt(s1, SequenceStmt(s5, s6))
    val function = FunDecl("f_if_else", List(), List(), body, NullExp)
    val cfg = flow(function)

    val paths = path(cfg, function.name)

    val path1 = List(
      StartNode(function.name),
      SimpleNode(s1),
      SimpleNode(s5),
      SimpleNode(s2),
      SimpleNode(s3),
      SimpleNode(s6),
      EndNode(function.name)
    )

    val path2 = List(
      StartNode(function.name),
      SimpleNode(s1),
      SimpleNode(s5),
      SimpleNode(s4),
      SimpleNode(s6),
      EndNode(function.name)
    )

    assert(Set(path1, path2) == paths)
  }

  /**
    f = 1;
      while (n>0) {
        f = f*n;
        n = n-1;
      }
    print(f)
   */
  test("path_from_while") {
    val s1 = AssignmentStmt("f", ConstExp(1))
    val s3 = AssignmentStmt("f", MultiExp(VariableExp("f"), VariableExp("n")))
    val s4 = AssignmentStmt("n", SubExp(VariableExp("n"), ConstExp(1)))
    val s2 = WhileStmt(GTExp(VariableExp("n"), ConstExp(0)), SequenceStmt(s3,s4))
    val s5 = OutputStmt(VariableExp("f"))

    val body = SequenceStmt(s1, SequenceStmt(s2, s5))
    val function = FunDecl("f_while", List(), List(), body, NullExp)
    val cfg = flow(function)

    val paths = path(cfg, function.name)

    val path1 = List(
      StartNode(function.name),
      SimpleNode(s1),
      SimpleNode(s2),
      SimpleNode(s5),
      EndNode(function.name)
    )

    val path2 = List(
      StartNode(function.name),
      SimpleNode(s1),
      SimpleNode(s2),
      SimpleNode(s3),
      SimpleNode(s4),
      SimpleNode(s2),
      SimpleNode(s5),
      EndNode(function.name)
    )

    assert(Set(path1, path2) == paths)
  }
}