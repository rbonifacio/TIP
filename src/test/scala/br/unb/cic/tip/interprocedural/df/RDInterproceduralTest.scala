package br.unb.cic.tip.interprocedural.df

import br.unb.cic.tip.*
import br.unb.cic.tip.df.ReachingDefinition
import br.unb.cic.tip.utils.Expression.*
import br.unb.cic.tip.utils.FunDecl
import br.unb.cic.tip.utils.Node.*
import br.unb.cic.tip.utils.Stmt.*
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.Set

class RDInterproceduralTest extends AnyFunSuite { 


  /**
   * P(var, stmt)
   *
   * mymethod(x)
   * m1:  x = 5         entry:{(x,s1), (y,s2) U (x,m1)}    exit:{(y,s2), (x,m1)} // entry gets the values from the CALLER
   *
   * main()
   * s1:   x = 1        entry:{}                            exit:{(x,s1)}
   * s2:   y = 2        entry:{(x,s1)}                      exit:{(x,s1), (y,s2)}
   * s3:  mymethod(x)   entry:{(x,s1), (y,s2)}              exit:{(y,s2), (x,m1)} // exit get the values from the CALLEE
   * s4:  a = y + 1     entry:{(y,s2), (x, m1)}             exit:{(y,s2), (x,m1), (a,s4)}
   * s5:  b = x + 1     entry:{(y,s2), (x, m1), (a, s4)}    exit:{(y,s2), (x,m1), (a,s4), (b,s5)}
   *
   */
  test("test_rd_using_only_statements") { 

    val m1 = AssignmentStmt("x", ConstExp(5))
    val myFunction = FunDecl("myFunction", List("x"), List(), m1, NullExp) 

    val s1 = AssignmentStmt("x", ConstExp(1)) 
    val s2 = AssignmentStmt("y", ConstExp(2)) 
    val s3 = AssignmentStmt("_m1", FunctionCallExp(NameExp(myFunction.name), List(VariableExp("x")))) 
    val s4 = AssignmentStmt("a", AddExp(VariableExp("y"), ConstExp(1)))
    val s5 = AssignmentStmt("b", AddExp(VariableExp("x"), ConstExp(1)))

    //main function
    val mainBody = SequenceStmt(s1, SequenceStmt(s2, SequenceStmt(s3, SequenceStmt(s4, s5))))

    val mainFunction = FunDecl("main", List(), List("x", "y", "z"), mainBody, NullExp) 

    val program = List(myFunction, mainFunction) 

    val RD = ReachingDefinition.run(mainBody, program) 

    assert( RD(s1) == ( 
      Set(), 
      Set(s1) 
    )) 

    assert( RD(s2) == ( 
      Set(s1), 
      Set(s1, s2) 
    )) 

    assert(RD(s3) == (
      Set(s1, s2),
      Set()
    ))
//
//    assert( RD(m1) == (
//      Set(),
//      Set(AssignmentStmt("a", ConstExp(999)))
//    ))
//
//    assert( RD(s4) == (
//      Set(),
//      Set(AssignmentStmt("z", ConstExp(3)))
//    ))
  } 
}