package br.unb.cic.tip

import br.unb.cic.tip.*
import br.unb.cic.tip.Expression.*
import br.unb.cic.tip.Node.*
import br.unb.cic.tip.Stmt.*
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable

class LVTest extends AnyFunSuite {

  test("Linear Control FLow Graph for Live Variables"){

    /**
     x = a; Entry: {a,b} Exit: {b}
     y = b; Entry: {b}   Exit: { }
     x = 3; Entry: { }   Exit: { }
     */
    val s1 = AssignmentStmt("x", VariableExp("a"))
    val s2 = AssignmentStmt("y", VariableExp("b"))
    val s3 = AssignmentStmt("x", ConstExp(3))

    val seq = SequenceStmt(s1, SequenceStmt(s2, s3))
    val LV =  LiveVariable.run(seq)

    assert(LV(s1) == (mutable.Set(VariableExp("a"), VariableExp("b")),mutable.Set(VariableExp("b"))))
    assert(LV(s2) == (mutable.Set(VariableExp("b")),mutable.Set()))
    assert(LV(s3) == (mutable.Set(),mutable.Set()))
  }

  test("Live Variables with Output command"){
    /**
     x = a;    Entry: {a,b} Exit: {b,x}
     y = b+1;  Entry: {b,x}   Exit: {x}
     output x; Entry: {x}     Exit: { }
     */

    val s1 = AssignmentStmt("x", VariableExp("a"))
    val s2 = AssignmentStmt("y", AddExp(VariableExp("b"), ConstExp(1)))
    val s3 = OutputStmt(VariableExp("x"))

    val seq = SequenceStmt(s1, SequenceStmt(s2, s3))
    val LV =  LiveVariable.run(seq)

    assert(LV(s1) == (mutable.Set(VariableExp("a"), VariableExp("b")), mutable.Set(VariableExp("b"), VariableExp("x"))))
    assert(LV(s2) == (mutable.Set(VariableExp("b"), VariableExp("x")), mutable.Set(VariableExp("x"))))
    assert(LV(s3) == (mutable.Set(VariableExp("x")), mutable.Set()))

  }

  test("Live Variables with If statement"){
    /**
     if (x < y){  Entry: {a, x, y} Exit: {a}
      x = a;      Entry: {a}       Exit: { }
     }
     y = 1;       Entry: { }       Exit: { }

     */

    val s2 = AssignmentStmt("x", VariableExp("a"))
    val s1 = IfElseStmt(GTExp(VariableExp("x"), VariableExp("y")), s2, None)

    val s3 = AssignmentStmt("y", ConstExp(1))
    val seq = SequenceStmt(s1, s3)
    val LV = LiveVariable.run(seq)

    assert(LV(s1) == (mutable.Set(VariableExp("a"), VariableExp("x"), VariableExp("y")), mutable.Set(VariableExp("a"))))
    assert(LV(s2) == (mutable.Set(VariableExp("a")), mutable.Set()))
    assert(LV(s3) == (mutable.Set(), mutable.Set()))
  }

  test("Living Variables test from the book"){
    /**

     x = 2;       Entry: {}  Exit: {}
     y = 4;       Entry: {}  Exit: {y}
     x = 1;       Entry: {y} Exit: {x, y}
     if (y < x){  Entry: {x,y} Exit: {y}
      z = y;      Entry: {y} Exit: {z}
     }
     else{
      z = y * y;  Entry: {y} Exit: {z}
     }
     x = z;       Entry: {z} Exit: {}

     */

    val s1 = AssignmentStmt("x", ConstExp(2))
    val s2 = AssignmentStmt("y", ConstExp(4))
    val s3 = AssignmentStmt("x", ConstExp(1))

    val s5 = AssignmentStmt("z", VariableExp("y"))
    val s6 = AssignmentStmt("z", MultiExp(VariableExp("y"), VariableExp("y")))
    val s4 = IfElseStmt(GTExp(VariableExp("y"), VariableExp("x")), s5, Option(s6))

    val s7 = AssignmentStmt("x", VariableExp("z"))

    val seq = SequenceStmt(s1,
      SequenceStmt(s2,
        SequenceStmt(s3,
          SequenceStmt(s4, s7))))

    val LV = LiveVariable.run(seq)

    assert(LV(s1) == (mutable.Set(), mutable.Set()))
    assert(LV(s2) == (mutable.Set(), mutable.Set(VariableExp("y"))))
    assert(LV(s3) == (mutable.Set(VariableExp("y")), mutable.Set(VariableExp("x"), VariableExp("y"))))
    assert(LV(s4) == (mutable.Set(VariableExp("x"), VariableExp("y")), mutable.Set(VariableExp("y"))))
    assert(LV(s5) == (mutable.Set(VariableExp("y")), mutable.Set(VariableExp("z"))))
    assert(LV(s6) == (mutable.Set(VariableExp("y")), mutable.Set(VariableExp("z"))))
    assert(LV(s7) == (mutable.Set(VariableExp("z")), mutable.Set()))
  }

  test("Live Variables with While loop"){

    /**
     while(x < 10){   Entry: {x} Exit: {x}
      x = x+1;        Entry: {x} Exit: {x}
     }
     output x;        Entry: {x} Exit: { }

     */

    val s2 = AssignmentStmt("x", AddExp(VariableExp("x"), ConstExp(1)))
    val s1 = WhileStmt(GTExp(VariableExp("x"), ConstExp(10)), s2)

    val s3 = OutputStmt(VariableExp("x"))

    val seq = SequenceStmt(s1, s3)

    val LV = LiveVariable.run(seq)

    assert(LV(s1) == (mutable.Set(VariableExp("x")), mutable.Set(VariableExp("x"))))
    assert(LV(s2) == (mutable.Set(VariableExp("x")), mutable.Set(VariableExp("x"))))
    assert(LV(s3) == (mutable.Set(VariableExp("x")), mutable.Set()))
  }

}
