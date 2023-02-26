package br.unb.cic.tip.cfg

import br.unb.cic.tip._
import org.scalatest.funsuite.AnyFunSuite

class cfgGeneratorTest extends AnyFunSuite {

    test("Test simple CFG") {
      val s1 = AssignmentStmt("x", ConstExp(1), 1)
      val s2 = AssignmentStmt("y", ConstExp(1), 2)
      val s3 = ReturnStmt(ConstExp(1), 3)
      val program = List(s1, s2, s3)

      val g = CFGBuilder.generate(program, 0)

//      println(g)
      g.map(n => println(n))

//      val expected = Set()
//
//      assert(expected == g)
    }

}
