package br.unb.cic.tip


/**
 * The abstract syntax definition of the Tiny Imperative
 * Language.
 */
type Program = List[FunDecl]

type Int = Integer
type Id  = String
type Field = (Id, Expression)
type Label = Integer

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
case class FunDecl(name: Id, args: List[Id], vars: List[Id], body: Stmt, retExp: Expression)

/** Algebraic data type for expressions */
abstract class Expression

case class ConstExp(v: Integer) extends Expression
case class VariableExp(name: Id) extends Expression
case class AddExp(left: Expression, right: Expression) extends Expression

//enum Expression:
//  case ConstExp(v: Integer) extends Expression
//  case VariableExp(name: Id) extends Expression
//  case AddExp(left: Expression, right: Expression) extends Expression
//  case SubExp(left: Expression, right: Expression) extends Expression
//  case MultiExp(left: Expression, right: Expression) extends Expression
//  case DivExp(left: Expression, right: Expression) extends Expression
//  case EqExp(left: Expression, right: Expression) extends Expression
//  case GTExp(left: Expression, right: Expression) extends Expression
//  case BracketExp(exp: Expression) extends Expression
//  // function-call expressions
//  case DirectFunctionCallExp(name: Id, args: List[Expression]) extends Expression
//  case IndirectFunctionCallExp(exp: Expression, args: List[Expression]) extends Expression
//  // pointer-based expressions
//  case AllocExp(exp: Expression) extends Expression
//  case LocationExp(pointer: Id) extends Expression
//  case LoadExp(exp: Expression) extends Expression
//  case NullExp extends Expression
//  // record-based expressions
//  case RecordExp(fields: List[Field]) extends Expression
//  case FieldAccess(record: Expression, field: Id)  // pessoa.id
//  case InputExp extends Expression

/** Algebraic data type for statements */
//enum Stmt:
//  case AssignmentStmt(name: Id, exp: Expression) extends Stmt
//  case StoreStmt(exp1: Expression, exp2: Expression) extends Stmt
//  case OutputStmt(exp: Expression) extends Stmt
//  case SequenceStmt(s1: Stmt, s2: Stmt) extends Stmt
//  case IfElseStmt(condition: Expression, s1: Stmt, s2: Option[Stmt]) extends Stmt
//  case WhileStmt(condition: Expression, stmt: Stmt) extends Stmt
//  case ReturnStmt(exp: Expression) extends Stmt
//  case DeclarationStmt(v: Expression) extends Stmt

abstract class Stmt
abstract class BasicStmt extends Stmt

case class AssignmentStmt(name: Id, exp: Expression, label: Label) extends BasicStmt
case class OutputStmt(exp: Expression, label: Label) extends BasicStmt
case class ReturnStmt(exp: Expression, label: Label) extends BasicStmt
case class SequenceStmt(s1: Stmt, s2: Stmt) extends Stmt


