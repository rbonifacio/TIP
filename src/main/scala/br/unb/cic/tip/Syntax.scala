package br.unb.cic.tip


/**
 * The abstract syntax definition of the Tiny Imperative
 * Language.
 */
type Program = List[FunDecl]

type Int = Integer
type Id  = String
type Field = (Id, Exp)
type Label = Integer

/** Algebraic definition of function declaration.
 *   - name: name of the function
 *   - args: formal arguments of the function
 *   - vars: local variables of the function
 *   - body: stmt corresponding to the function body
 *   - retExp: the return Exp of the function
 *
 *   Concrete Syntax           |      Abstract Syntex
 *   soma(a, b) {              |
 *      return a + b;    => parser => FunDecl("soma", ["a", "b"], [], , AddExp(VariableExp("a"), VariableExp("b"))
 *   }
 */
case class FunDecl(name: Id, args: List[Id], vars: List[Id], body: Stmt, retExp: Exp)

/**
 * ExpS
 */

abstract class Exp
abstract class BasicExp extends Exp
abstract class ArithmeticExp extends Exp
abstract class BoolExp extends Exp
abstract class CallExp extends Exp
abstract class PointerExp extends Exp
abstract class RecordExp extends Exp
abstract class IOExp extends Exp

//Basic Exps
case class ConstExp(v: Int) extends BasicExp
case class VariableExp(name: Id) extends BasicExp
case class BracketExp(exp: Exp) extends BasicExp

//IO Exps
case object InputExp extends IOExp
case object OutputExp extends IOExp

//Arithmetic Exps
case class AddExp(left: Exp, right: Exp) extends ArithmeticExp
case class SubExp(left: Exp, right: Exp) extends ArithmeticExp
case class MulExp(left: Exp, right: Exp) extends ArithmeticExp
case class DivExp(left: Exp, right: Exp) extends ArithmeticExp

//Boolean Exps
case class GTExp(left: Exp, right: Exp) extends Exp
case class EqExp(left: Exp, right: Exp) extends Exp

//Call functions
case class DirectFunctionCallExp(name: Id, args: List[Exp]) extends CallExp
case class IndirectFunctionCallExp(exp: Exp, args: List[Exp]) extends CallExp

//Pointers
case class AllocExp(exp: Exp) extends PointerExp
case class LocationExp(pointer: Id) extends PointerExp
case class LoadExp(exp: Exp) extends PointerExp
case object NullExp extends PointerExp

//Records
case class RecordsExp(fields: List[Field]) extends RecordExp
case class FieldAccess(record: Exp, field: Id) extends RecordExp // pessoa.id

/**
 * STATEMENTS
 */

abstract class Stmt
abstract class BasicStmt extends Stmt
abstract class ConditionStmt extends Stmt
abstract class PointerStmt extends Stmt

//Condition Statement
case class IfStmt(condition: Exp, s1: Stmt, label: Label) extends ConditionStmt
case class IfElseStmt(condition: Exp, s1: Stmt, s2: Stmt, label: Label) extends ConditionStmt
case class WhileStmt(condition: Exp, stmt: Stmt) extends ConditionStmt

//Basic Statement
case class AssignmentStmt(name: Id, exp: Exp, label: Label) extends BasicStmt
case class OutputStmt(exp: Exp, label: Label) extends BasicStmt
case class ReturnStmt(exp: Exp, label: Label) extends BasicStmt
case class DeclarationStmt(exp: Exp, label: Label) extends BasicStmt

//Pointer
case class StoreStmt(fieldAccess: FieldAccess, exp: Exp) extends PointerStmt

//Others
case class SequenceStmt(s1: Stmt, s2: Stmt) extends Stmt



