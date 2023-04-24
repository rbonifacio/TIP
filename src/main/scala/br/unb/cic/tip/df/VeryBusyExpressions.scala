package br.unb.cic.tip.df

import scala.collection.mutable
import br.unb.cic.tip.*
import br.unb.cic.tip.utils.Expression.*
import br.unb.cic.tip.utils.Node.*
import br.unb.cic.tip.utils.Stmt.*
import br.unb.cic.tip.utils.{Expression, Stmt}


type VB = (mutable.Set[Expression], mutable.Set[Expression])
type ResultVB = mutable.HashMap[Stmt, VB]

object VeryBusyExpressions {

  def run(program: Stmt): ResultVB = {

    /** Initialisation (of W and analysis) */

    val W: mutable.Stack[(Stmt, Stmt)] = mutable.Stack()
    val analysis: mutable.HashMap[Stmt, mutable.Set[Expression]] = mutable.HashMap()

    var AExp: mutable.Set[Expression] = mutable.Set()

    for (stmt <- blocks(program)){
      analysis(stmt) = mutable.Set()
      AExp = AExp union extractExprFromStmt(stmt)
    }

    val reverseFlow: mutable.HashMap[Stmt, mutable.ListBuffer[Stmt]] = mutable.HashMap()

    for (node <- flow(program)) {
      node match {
        case (SimpleNode(stmt1), SimpleNode(stmt2)) => W.append((stmt2, stmt1))
          analysis(stmt1) = AExp

          if (reverseFlow.contains(stmt2)){
            reverseFlow(stmt2).append(stmt1)
          }
          else {
            reverseFlow.addOne(stmt2, mutable.ListBuffer(stmt1))
          }

        case (StartNode(id), SimpleNode(stmt)) => analysis(stmt) = AExp
        case _ =>
      }
    }

    /** Iteration (updating W and Analysis) */

    while (W.nonEmpty){

      val lastStmt = W.pop()
      val analysis1 = transfer(analysis(lastStmt._1), lastStmt._1, AExp)
      val analysis2 = analysis(lastStmt._2)

      if (!analysis2.subsetOf(analysis1)){

        analysis(lastStmt._2) = analysis2 intersect analysis1

        if (reverseFlow.contains(lastStmt._2)){
          for (newStmt <- reverseFlow(lastStmt._2)){
            W.append((lastStmt._2, newStmt))
          }
        }

      }
    }

    /** Presenting the result */

    val resMap: ResultVB =  mutable.HashMap()
    for (node <- analysis){
      resMap.addOne((node._1, (transfer(node._2, node._1, AExp), node._2)))
    }

    resMap

  }


  def extractExpression(expr: Expression): mutable.Set[Expression] = {
    expr match {

      case AddExp(left, right) => mutable.Set(AddExp(left,right)) union extractExpression(left) union
        extractExpression(right)
      case SubExp(left, right) => mutable.Set(SubExp(left,right)) union extractExpression(left) union
        extractExpression(right)
      case MultiExp(left, right) => mutable.Set(MultiExp(left,right)) union extractExpression(left) union
        extractExpression(right)
      case DivExp(left, right) => mutable.Set(DivExp(left,right)) union extractExpression(left) union
        extractExpression(right)
      case EqExp(left, right) => extractExpression(left) union extractExpression(right)
      case GTExp(left, right) => extractExpression(left) union extractExpression(right)

      case BracketExp(exp) => extractExpression(exp)
      case AllocExp(exp) => extractExpression(exp)
      case LoadExp(exp) => extractExpression(exp)
      //      case FieldAccess(record, field) =>

      case _ => mutable.Set()
    }
  }

  def extractExprFromStmt(stmt: Stmt): mutable.Set[Expression]  = {
    stmt match {
      case AssignmentStmt(name, exp) => extractExpression(exp)
      case StoreStmt(exp1, exp2) => extractExpression(exp1) union extractExpression(exp2)
      case OutputStmt(exp) => extractExpression(exp)
      case IfElseStmt(condition, s1, s2) => extractExpression(condition)
      case WhileStmt(condition, stmt) => extractExpression(condition)
      case _ => mutable.Set()
    }
  }

  def hasVariable(exp: Expression, variable: String): Boolean ={
    exp match {
      case VariableExp(name) => name == variable
      case AddExp(left, right) => hasVariable(left, variable) || hasVariable(right, variable)
      case SubExp(left, right) => hasVariable(left, variable) || hasVariable(right, variable)
      case MultiExp(left, right) => hasVariable(left, variable) || hasVariable(right, variable)
      case DivExp(left, right) => hasVariable(left, variable) || hasVariable(right, variable)
      case EqExp(left, right) => hasVariable(left, variable) || hasVariable(right, variable)
      case GTExp(left, right) => hasVariable(left, variable) || hasVariable(right, variable)

      case BracketExp(exp) => hasVariable(exp, variable)
      case AllocExp(exp) => hasVariable(exp, variable)
      case LoadExp(exp) => hasVariable(exp, variable)
      case FieldAccess(record, field) => hasVariable(record, variable)

      case _ => false
      // Not sure
      //    case VarDecl(vars) => mutable.Set(vars map {case v => VariableExp(v)} :_*)
      //    case DirectFunctionCallExp(name: Id, args: List[Expression])
      //    case IndirectFunctionCallExp(exp: Expression, args: List[Expression])

    }
  }

  def transfer(stmts: mutable.Set[Expression], stmt: Stmt, aexp: mutable.Set[Expression]):
  mutable.Set[Expression] = {
    (stmts diff kill(stmt, aexp)) union gen(stmt)
  }



  def kill(stmt: Stmt, aexp: mutable.Set[Expression]): mutable.Set[Expression] = {
    stmt match {
      case AssignmentStmt(name, exp) => aexp.filter(hasVariable(_, name))
      case _ => mutable.Set()
    }
  }

  def gen(stmt: Stmt): mutable.Set[Expression] = {
    extractExprFromStmt(stmt)
  }

}