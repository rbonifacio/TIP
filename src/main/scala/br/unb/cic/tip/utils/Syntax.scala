package br.unb.cic.tip.utils

import br.unb.cic.tip.utils.Expression.VariableExp

/**
 * The abstract syntax definition of the Tiny Imperative
 * Language.
 */
type Program = List[FunDecl]

type Id = String
type Field = (Id, Expression)

/** Algebraic definition of function declaration.
  *   - name: name of the function
  *   - args: formal arguments of the function
  *   - vars: local variables of the function
  *   - body: stmt corresponding to the function body
  *   - retExp: the return expression of the function
  *
 *   Concrete Syntax           |      Abstract Syntex
 *   soma(a, b) {              |
 *      return a + b;    => parser => FunDecl("soma", ["a", "b"], [], , AddExp(VariableExp("a"), VariableExp("b"))
 *   }
 */
case class FunDecl(
    name: Id,
    args: List[Id],
    vars: List[Id],
    body: Stmt,
    retExp: Expression
)

/** Algebraic data type for expressions */
enum Expression:
  case ConstExp(v: Integer) extends Expression // 0 | 1 | -1 | 2 | -2 | ...
  case VariableExp(name: Id) extends Expression // x | y | z | . . .
  case PointerExp(name: Id) extends Expression // x | y | z | . . .
  case AddExp(left: Expression, right: Expression) extends Expression // Exp + Exp
  case SubExp(left: Expression, right: Expression) extends Expression // Exp - Exp
  case MultiExp(left: Expression, right: Expression) extends Expression // Exp * Exp
  case DivExp(left: Expression, right: Expression) extends Expression // Exp / Exp
  case EqExp(left: Expression, right: Expression) extends Expression // Exp == Exp
  case GTExp(left: Expression, right: Expression) extends Expression // Exp > Exp
  case BracketExp(exp: Expression) extends Expression // (Exp)
  case NameExp(name: Id) extends Expression // (Exp)

  // function-call expression
  case FunctionCallExp(name: Expression, args: List[Any]) extends Expression

  // pointer-based expressions
  case AllocExp(exp: Expression) extends Expression // alloc Exp
  case LocationExp(pointer: Id) extends Expression // & Id
  case LoadExp(exp: Expression) extends Expression // * Exp
  case NullExp extends Expression // null

  // record-based expressions
  case RecordExp(fields: List[Field]) extends Expression // { Id : Exp , . . . , Id : Exp }
  case FieldAccess(record: Expression, field: Id) // Exp . Id
  case InputExp extends Expression // input

/** Algebraic data type for statements */

abstract class Stmt {
  val label = Stmt.getLabel()

}

object Stmt {
  var label = 0

  def getLabel(): Int = {
    label += 1
    label
  }
}

case class AssignmentStmt(name: Id, exp: Expression) extends Stmt // Id = Exp
case class AssignmentPointerStmt(name: Expression, exp: Expression) extends Stmt // Id = Exp
case class IfElseStmt(condition: Expression, s1: Stmt, s2: Option[Stmt]) extends Stmt // if ( Exp ) { Stmt } [else { Stmt }]
case class WhileStmt(condition: Expression, stmt: Stmt) extends Stmt // while ( Exp ) { Stmt }
case class SequenceStmt(s1: Stmt, s2: Stmt) extends Stmt // Stmt Stmt
case class StoreStmt(exp1: Expression, exp2: Expression) extends Stmt // *Exp = Exp
case class OutputStmt(exp: Expression) extends Stmt // output Exp
case class RecordAssignmentStmt(name: Id, field: Id, exp: Expression) extends Stmt // Id.Id = Exp;
case class RecordStoreStmt(exp1: Expression, id: Id, exp2: Expression) extends Stmt // (*Exp).Id = Exp;
case class CallStmt(stmt: AssignmentStmt) extends Stmt //
case class AfterCallStmt(stmt: AssignmentStmt) extends Stmt //
case class ReturnStmt(exp: Expression) extends Stmt //
case object NopStmt extends Stmt // nop

/** Node Types */
enum Node:
  case StartNode(function: Id) extends Node
  case EndNode(function: Id) extends Node
  case SimpleNode(stmt: Stmt) extends Node
  case SVFNode(stmt: Stmt, variable: Expression) extends Node

class LabelSensitiveStmt(val s: Stmt) {
  override def equals(x: Any): Boolean = {
    if (x.isInstanceOf[Stmt]) {
      val otherStmt = x.asInstanceOf[Stmt]
      s.label == otherStmt.label && s == otherStmt
    } else if (x.isInstanceOf[LabelSensitiveStmt]) {
      val otherStmt = x.asInstanceOf[LabelSensitiveStmt]
      s.label == otherStmt.s.label && s == otherStmt.s
    } else {
      false
    }
  }

  override def hashCode(): Int = s.hashCode()
}

object LabelSensitiveStmt {
  given stmtToLabeled: Conversion[Stmt, LabelSensitiveStmt] =
    LabelSensitiveStmt(_)
  given labeledToStmt: Conversion[LabelSensitiveStmt, Stmt] = _.s
}
