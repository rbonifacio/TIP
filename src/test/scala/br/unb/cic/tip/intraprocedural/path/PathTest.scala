package br.unb.cic.tip.intraprocedural.path

import br.unb.cic.tip.{exportDot, flow, path}
import br.unb.cic.tip.utils.Expression.*
import br.unb.cic.tip.utils.FunDecl
import br.unb.cic.tip.utils.Node.*
import br.unb.cic.tip.utils.Stmt.*
import org.scalatest.funsuite.AnyFunSuite

class PathTest extends AnyFunSuite {

  /**
   s1: x = 1
   s2: z = 3
   s3: print(z)

   * Path(1) = s1, s2, s3
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
//    println(exportDot(cfg, path1))
  }

  /**
   s1: a = 1
   s2: if (x == 2) {
   s3:   b = 2
   s4:   c = 3
   s-:  } else {
   s5:    d = 4
   s-:  }
   s6: e = 5

    * Path(1) = s1, s2, s3, s4, s6
    * Path(2) = s1, s2, s5, s6
   */
  test("path_from_if_else") {
    val s1 = AssignmentStmt("a", ConstExp(1))
    val s3 = AssignmentStmt("b", ConstExp(2))
    val s4 = AssignmentStmt("c", ConstExp(3))
    val s5 = AssignmentStmt("d", ConstExp(4))
    val s2 = IfElseStmt(EqExp(VariableExp("x"),ConstExp(2)), SequenceStmt(s3,s4), Some(s5))
    val s6 = AssignmentStmt("e", ConstExp(5))

    val body = SequenceStmt(s1, SequenceStmt(s2, s6))
    val function = FunDecl("f_if_else", List(), List(), body, NullExp)
    val cfg = flow(function)

    val paths = path(cfg, function.name)

    val path1 = List(
      StartNode(function.name),
      SimpleNode(s1),
      SimpleNode(s2),
      SimpleNode(s3),
      SimpleNode(s4),
      SimpleNode(s6),
      EndNode(function.name)
    )

    val path2 = List(
      StartNode(function.name),
      SimpleNode(s1),
      SimpleNode(s2),
      SimpleNode(s5),
      SimpleNode(s6),
      EndNode(function.name)
    )

    assert(Set(path1, path2) == paths)

//    println(exportDot(cfg, path1))
  }

  /**
    s1: f = 1;
    s2: while (n>0) {
    s3:   f = f*n;
    s4:   n = n-1;
    s-: }
    s5: print(f)

     * Path(1): s1, s2, s5
     * Path(2): s1, s2, s3, s4, s2, s5
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

    paths.foreach(x => println(exportDot(cfg, x)))
  }
}