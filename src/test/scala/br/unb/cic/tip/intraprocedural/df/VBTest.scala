package br.unb.cic.tip.intraprocedural.df

import br.unb.cic.tip.*
import br.unb.cic.tip.utils.Expression.*
import br.unb.cic.tip.utils.Node.*
import br.unb.cic.tip.utils.Stmt.*
import br.unb.cic.tip.df.VeryBusyExpressions
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable

class VBTest extends AnyFunSuite {


  test("Linear cfg test Very Busy Expressions"){

    /**
    x = 2;       Entry: {} Exit: {x+1, y*(x+1)}
      y = y*(x+1); Entry: {y*(x+1), x+1} Exit: {x+1}
      x = x+1;     Entry: {x+1} Exit: {}

     */

    val s1 = AssignmentStmt("x", ConstExp(2))
    val s2 = AssignmentStmt("y" , MultiExp(VariableExp("y"), AddExp(VariableExp("x"), ConstExp(1))))
    val s3 = AssignmentStmt("x", AddExp(VariableExp("x"), ConstExp(1)))

    val seq = SequenceStmt(s1, SequenceStmt(s2, s3))
    val VB = VeryBusyExpressions.run(seq)

    assert(VB(s1) == (mutable.HashSet(),mutable.HashSet(AddExp(VariableExp("x"),ConstExp(1)),
      MultiExp(VariableExp("y"),AddExp(VariableExp("x"),ConstExp(1))))))

    assert(VB(s2) == (mutable.HashSet(AddExp(VariableExp("x"),ConstExp(1)), MultiExp(VariableExp("y"),
      AddExp(VariableExp("x"),ConstExp(1)))),mutable.HashSet(AddExp(VariableExp("x"),ConstExp(1)))))

    assert(VB(s3) == (mutable.HashSet(AddExp(VariableExp("x"),ConstExp(1))), mutable.HashSet()))

  }

  test("Very Busy Expressions test from the book"){

    /**

    if (a>b) {
      x = b-a;
      y = a-b;
     }
     else{
      y = b-a;
      x = a-b;
     }

     */

    val s2 = AssignmentStmt("x", SubExp(VariableExp("b"), VariableExp("a")))
    val s3 = AssignmentStmt("y", SubExp(VariableExp("a"), VariableExp("b")))
    val ifStmt = SequenceStmt(s2, s3)

    val s4 = AssignmentStmt("y", SubExp(VariableExp("b"), VariableExp("a")))
    val s5 = AssignmentStmt("x", SubExp(VariableExp("a"), VariableExp("b")))
    val elseStmt = SequenceStmt(s4, s5)

    val s1 = IfElseStmt(GTExp(VariableExp("a"), VariableExp("b")), ifStmt, Option(elseStmt))

    val VB = VeryBusyExpressions.run(s1)


    assert(VB(s1) == (mutable.Set(SubExp(VariableExp("b"),VariableExp("a")), SubExp(VariableExp("a"),VariableExp("b")))
      ,mutable.Set(SubExp(VariableExp("b"),VariableExp("a")), SubExp(VariableExp("a"),VariableExp("b")))))

    assert(VB(s2) == (mutable.Set(SubExp(VariableExp("b"),VariableExp("a")), SubExp(VariableExp("a"),VariableExp("b")))
      ,mutable.Set(SubExp(VariableExp("a"),VariableExp("b")))))

    assert(VB(s3) == (mutable.Set(SubExp(VariableExp("a"),VariableExp("b"))),mutable.Set()))

    assert(VB(s4) == (mutable.Set(SubExp(VariableExp("b"),VariableExp("a")), SubExp(VariableExp("a"),VariableExp("b")))
      ,mutable.Set(SubExp(VariableExp("a"),VariableExp("b")))))

    assert(VB(s5) == (mutable.Set(SubExp(VariableExp("a"),VariableExp("b"))),mutable.Set()))

  }

  test("While loop with non-trivial operations (Very Busy Expression)"){
    /**
    while (x/2 > y+x+1) Entry: {x/2, x+1, y+x+1} Exit: {}
       {
        y = x/2;          Entry: {x+1} Exit: {x/2, x+1, y+x+1}
        x = x+1;          Entry: {x/2, x+1} Exit: {x+1}
       }
       z = 1;             Entry: {} Exit: {}

     */

    val s2 = AssignmentStmt("y", DivExp(VariableExp("x"), ConstExp(2)))
    val s3 = AssignmentStmt("x", AddExp(VariableExp("x"), ConstExp(1)))
    val s4 = AssignmentStmt("z", ConstExp(1))
    val whileStmts = SequenceStmt(s2, s3)

    val s1 = WhileStmt(
      GTExp(DivExp(VariableExp("x"), ConstExp(2)),
        AddExp(VariableExp("y"),
          AddExp(VariableExp("x"), ConstExp(1)))),
      whileStmts)

    val s5 = AssignmentStmt("z", ConstExp(1))
    val seq = SequenceStmt(s1, s5)

    val VB = VeryBusyExpressions.run(seq)

    assert(VB(s1) == (mutable.HashSet(DivExp(VariableExp("x"),ConstExp(2)), AddExp(VariableExp("x"),ConstExp(1)),
      AddExp(VariableExp("y"),AddExp(VariableExp("x"),ConstExp(1)))),mutable.HashSet()))


    assert(VB(s2) == (mutable.HashSet(DivExp(VariableExp("x"),ConstExp(2)), AddExp(VariableExp("x"),ConstExp(1))),
      mutable.HashSet(AddExp(VariableExp("x"),ConstExp(1)))))

    assert(VB(s3) == (mutable.HashSet(AddExp(VariableExp("x"),ConstExp(1))),
      mutable.HashSet(DivExp(VariableExp("x"),ConstExp(2)), AddExp(VariableExp("x"),ConstExp(1)),
        AddExp(VariableExp("y"),AddExp(VariableExp("x"),ConstExp(1))))))

    assert(VB(s4) == (mutable.HashSet(),mutable.HashSet()))
  }


  test ("Very Busy Expressions, example from the PDF"){

    /**
    x = input;       Entry: {} Exit: {x-1, x-2}
      a = x-1;         Entry: {x-1, x-2} Exit: {x-2}
      b = x-2;         Entry: {x-2} Exit: {a*b}
      while (x>0){     Entry: {a*b} Exit: {a*b}
        output a*b-x;  Entry: {a*b, a*b-x, x-1} Exit: {a*b, x-1}
        x = x-1;       Entry: {a*b, x-1} Exit: {a*b}
      }
      output a*b;      Entry: {a*b} Exit: {}

     */

    val s1 = AssignmentStmt("x", InputExp)
    val s2 = AssignmentStmt("a", SubExp(VariableExp("x"), ConstExp(1)))
    val s3 = AssignmentStmt("b", SubExp(VariableExp("x"), ConstExp(2)))

    val s5 = OutputStmt(SubExp(MultiExp(VariableExp("a"), VariableExp("b")), VariableExp("x")))
    val s6 = AssignmentStmt("x", SubExp(VariableExp("x"), ConstExp(1)))
    val s4 = WhileStmt(GTExp(VariableExp("x"), ConstExp(0)), SequenceStmt(s5, s6))

    val s7 = OutputStmt(MultiExp(VariableExp("a"), VariableExp("b")))

    val seq = SequenceStmt(s1,
      SequenceStmt(s2,
        SequenceStmt(s3, SequenceStmt(s4, s7))))
    val VB = VeryBusyExpressions.run(seq)

    assert(VB(s1) == (mutable.HashSet(),mutable.HashSet(SubExp(VariableExp("x"),ConstExp(2)),
      SubExp(VariableExp("x"),ConstExp(1)))))

    assert(VB(s2) == (mutable.HashSet(SubExp(VariableExp("x"),ConstExp(2)),
      SubExp(VariableExp("x"),ConstExp(1))),mutable.HashSet(SubExp(VariableExp("x"),ConstExp(2)))))

    assert(VB(s3) == (mutable.HashSet(SubExp(VariableExp("x"),ConstExp(2))),
      mutable.HashSet(MultiExp(VariableExp("a"),VariableExp("b")))))

    assert(VB(s4) == (mutable.HashSet(MultiExp(VariableExp("a"),VariableExp("b"))),
      mutable.HashSet(MultiExp(VariableExp("a"),VariableExp("b")))))

    assert(VB(s5) == (mutable.HashSet(SubExp(MultiExp(VariableExp("a"),VariableExp("b")),VariableExp("x")),
      MultiExp(VariableExp("a"),VariableExp("b")), SubExp(VariableExp("x"),ConstExp(1))),
      mutable.HashSet(MultiExp(VariableExp("a"),VariableExp("b")), SubExp(VariableExp("x"),ConstExp(1)))))

    assert(VB(s6) == (mutable.HashSet(MultiExp(VariableExp("a"),VariableExp("b")), SubExp(VariableExp("x"),ConstExp(1))),
      mutable.HashSet(MultiExp(VariableExp("a"),VariableExp("b")))))

    assert(VB(s7) == (mutable.HashSet(MultiExp(VariableExp("a"),VariableExp("b"))), mutable.HashSet()))
  }
}