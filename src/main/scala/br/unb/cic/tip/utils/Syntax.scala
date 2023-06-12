package br.unb.cic.tip.utils

/**
 * The abstract syntax definition of the Tiny Imperative
 * Language.
 */
type Program = List[FunDecl]
type Id = String
//type Int = Integer
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
    args: List[BasicExp],
    vars: List[BasicExp],
    body: Stmt,
    retExp: Expression
)

/**
 * Type for expressions
 **/

abstract class Expression
abstract class BasicExp extends Expression
abstract class PointerLeftExp extends BasicExp


case class VariableExp(name: Id) extends BasicExp // x | y | z | . . .
case class PointerExp(name: Id) extends PointerLeftExp // p | q | . . .

case class ConstExp(v: Integer) extends Expression
case class BracketExp(exp: Expression) extends Expression // (Exp)
case class NameExp(name: Id) extends Expression // (Exp)
case object NullExp extends Expression // null

//  // Algebraic
case class AddExp(left: Expression, right: Expression) extends Expression // Exp + Exp
case class SubExp(left: Expression, right: Expression) extends Expression // Exp - Exp
case class MultiExp(left: Expression, right: Expression) extends Expression // Exp * Exp
case class DivExp(left: Expression, right: Expression) extends Expression // Exp / Exp
case class EqExp(left: Expression, right: Expression) extends Expression // Exp == Exp
case class GTExp(left: Expression, right: Expression) extends Expression // Exp > Exp
//
//  // function-call
case class FunctionCallExp(name: Expression, args: List[Any]) extends Expression
//
//  // Pointer
case class AllocExp(exp: Expression) extends Expression // alloc Exp
case class LocationExp(pointer: Id) extends Expression // & Id
case class LoadExp(pointer: PointerExp) extends PointerLeftExp // *p
//
//  // Record
case class RecordExp(fields: List[Field]) extends Expression // { Id : Exp , . . . , Id : Exp }
case class FieldAccess(record: Expression, field: Id) // Exp . Id
case object InputExp extends Expression // input

//enum Expression:
//  // Basic
//  case ConstExp(v: Integer) extends Expression // 0 | 1 | -1 | 2 | -2 | ...
//  case VariableExp(name: Id) extends Expression // x | y | z | . . .
//  case PointerExp(name: Id) extends Expression // p | q | . . .
//  case BracketExp(exp: Expression) extends Expression // (Exp)
//  case NameExp(name: Id) extends Expression // (Exp)
//  case NullExp extends Expression // null
//
//  // Algebraic
//  case AddExp(left: Expression, right: Expression) extends Expression // Exp + Exp
//  case SubExp(left: Expression, right: Expression) extends Expression // Exp - Exp
//  case MultiExp(left: Expression, right: Expression) extends Expression // Exp * Exp
//  case DivExp(left: Expression, right: Expression) extends Expression // Exp / Exp
//  case EqExp(left: Expression, right: Expression) extends Expression // Exp == Exp
//  case GTExp(left: Expression, right: Expression) extends Expression // Exp > Exp
//
//  // function-call
//  case FunctionCallExp(name: Expression, args: List[Any]) extends Expression
//
//  // Pointer
//  case AllocExp(exp: Expression) extends Expression // alloc Exp
//  case LocationExp(pointer: Id) extends Expression // & Id
//  case LoadExp(exp: Expression) extends Expression // * Exp
//
//  // Record
//  case RecordExp(fields: List[Field]) extends Expression // { Id : Exp , . . . , Id : Exp }
//  case FieldAccess(record: Expression, field: Id) // Exp . Id
//  case InputExp extends Expression // input

/** 
* Algebraic data type for statements 
*/
enum Stmt:
  //basic
  case AssignmentStmt(name: BasicExp, exp: Expression) extends Stmt // Id = Exp
  case SequenceStmt(s1: Stmt, s2: Stmt) extends Stmt // Stmt Stmt
  case NopStmt extends Stmt // nop

  //algebraic
  case IfElseStmt(condition: Expression, s1: Stmt, s2: Option[Stmt]) extends Stmt // if ( Exp ) { Stmt } [else { Stmt }]
  case WhileStmt(condition: Expression, stmt: Stmt) extends Stmt // while ( Exp ) { Stmt }

  //function
  case CallStmt(stmt: Stmt) extends Stmt //
  case AfterCallStmt(stmt: Stmt) extends Stmt //
  case ReturnStmt(exp: Expression) extends Stmt //

  //pointers
  case StoreStmt(exp1: Expression, exp2: Expression) extends Stmt // *Exp = Exp
  case OutputStmt(exp: Expression) extends Stmt // output Exp

  //records
  case RecordAssignmentStmt(name: Id, field: Id, exp: Expression) extends Stmt // Id.Id = Exp;
  case RecordStoreStmt(exp1: Expression, id: Id, exp2: Expression) extends Stmt // (*Exp).Id = Exp;

/**
 * Node Types
 */
enum Node:
  case StartNode(function: Id) extends Node
  case EndNode(function: Id) extends Node
  case SimpleNode(stmt: Stmt) extends Node
