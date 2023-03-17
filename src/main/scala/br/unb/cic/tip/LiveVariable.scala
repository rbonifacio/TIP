package br.unb.cic.tip

import br.unb.cic.tip.*
import br.unb.cic.tip.Expression.*
import br.unb.cic.tip.Node.SimpleNode
import br.unb.cic.tip.Stmt.*

import scala.collection.mutable

type DF = (mutable.Set[Expression], mutable.Set[Expression])
type Result = mutable.HashMap[Stmt, DF]

object LiveVariable {

    def run(program: Stmt): Result = {

      /** Initialisation (of W and Analysis) */

      val W: mutable.Stack[(Stmt, Stmt)] = mutable.Stack()

      val cfg = flow(program)
      val reverseFlow: mutable.HashMap[Stmt, mutable.ListBuffer[Stmt]] = mutable.HashMap()

      for (node <- cfg){
        node match {
          case (SimpleNode(stmt1), SimpleNode(stmt2)) => {

            W.append((stmt2, stmt1))

            if (reverseFlow.contains(stmt2)){
              reverseFlow(stmt2).append(stmt1)
            }

            else {
              reverseFlow.addOne(stmt2, mutable.ListBuffer(stmt1))
            }
          }
          case _ =>
        }
      }

      val analysis: mutable.HashMap[Stmt, mutable.Set[Expression]] = mutable.HashMap()

      for (stmt <- blocks(program)) {
        analysis(stmt) = mutable.Set()
      }

      /** Iteraration (updating W and analysis) */

      while (W.nonEmpty){

        val lastStmt = W.pop()
        val analysis1 = exitFunction(analysis(lastStmt._1), lastStmt._1)
        val analysis2 = analysis(lastStmt._2)

        if (!analysis1.subsetOf(analysis2)){

          analysis(lastStmt._2) = analysis2 union analysis1

          if (reverseFlow.contains(lastStmt._2)){
            for (newStmt <- reverseFlow(lastStmt._2)){
              W.append((lastStmt._2, newStmt))
            }
          }

        }

      }

      /** Presenting the result */

      val resMap: Result =  mutable.HashMap()
      for (node <- analysis){
        resMap.addOne((node._1, (exitFunction(node._2, node._1), node._2)))
      }

      resMap
    }

  def exitFunction(stmts: mutable.Set[Expression], stmt: Stmt): mutable.Set[Expression] = {
    (stmts diff kill(stmt)) union gen(stmt)
  }

  def kill(stmt: Stmt): mutable.Set[Expression] = stmt match {
    case AssignmentStmt(id, exp) => mutable.Set(VariableExp(id))
    case _ => mutable.Set()
  }

  def gen(stmt: Stmt): mutable.Set[Expression] = stmt match {
    case AssignmentStmt(id, exp) => findVariables(exp)
    case IfElseStmt(condition, s1, s2) => findVariables(condition)
    case WhileStmt(condition, stmt) => findVariables(condition)
    case OutputStmt(exp) => findVariables(exp)

    case _ => mutable.Set()
    
//    Not Sure
//      case StoreStmt(exp1: Expression, exp2: Expression) extends Stmt
//      case SequenceStmt(s1: Stmt, s2: Stmt) extends Stmt

  }
  
  def findVariables(expression: Expression, foundVariables: mutable.Set[Expression] = mutable.Set()): 
  mutable.Set[Expression] = expression match {
    case VariableExp(v) => foundVariables union mutable.Set(VariableExp(v))
    
    case AddExp(left, right) => findVariables(left, foundVariables) union findVariables(right)
    case SubExp(left, right) =>  findVariables(left, foundVariables) union findVariables(right)
    case MultiExp(left, right) => findVariables(left, foundVariables) union findVariables(right)
    case DivExp(left, right) => findVariables(left, foundVariables) union findVariables(right)
    case EqExp(left, right) => findVariables(left, foundVariables) union findVariables(right)
    case GTExp(left, right) => findVariables(left, foundVariables) union findVariables(right)

    case BracketExp(exp) => findVariables(exp, foundVariables)
    case AllocExp(exp) => findVariables(exp , foundVariables)
    case LoadExp(exp) => findVariables(exp, foundVariables)
    case FieldAccess(record, field) => findVariables(record, foundVariables)

    
    case _ => foundVariables
    
    // Not sure
    //    case VarDecl(vars) => mutable.Set(vars map {case v => VariableExp(v)} :_*)
    //    case DirectFunctionCallExp(name: Id, args: List[Expression])
    //    case IndirectFunctionCallExp(exp: Expression, args: List[Expression])
    
  }
  
}