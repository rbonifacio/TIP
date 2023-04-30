package br.unb.cic.tip.utils

import br.unb.cic.tip.*
import br.unb.cic.tip.utils.Expression.*
import br.unb.cic.tip.utils._
import br.unb.cic.tip.utils.{Expression, FunDecl, Stmt}

import java.nio.file.Paths
import scala.io.Source
import scala.util.parsing.combinator.*
import scala.util.parsing.input.Positional

class BasicParser extends RegexParsers {
  def id: Parser[Id] = """[a-zA-Z]+""".r
  def int: Parser[Integer] = """-?[0-9]+""".r ^^ { (s) => Integer(s) }
}

/* Exp → Int
 *     | Id
 *     | input
 *     | null
 *     | & Id
 *     | Exp + Exp | Exp - Exp | Exp * Exp | Exp / Exp | Exp > Exp | Exp == Exp
 *     | ( Exp )
 *     | Id ( Exp,. . .,Exp )
 *     | Exp ( Exp , . . ., Exp )
 *     | alloc Exp
 *     | * Exp
 *     | { Id : Exp , . . ., Id : Exp }
 *     | Exp . Id
 */
class ExpressionParser extends BasicParser {

  def prio3Expression: Parser[Expression] = "input" ^^^ InputExp
    | "alloc" ~> expression ^^ AllocExp.apply
    | "&" ~> id ^^ LocationExp.apply
    | "*" ~> expression ^^ LoadExp.apply
    | "null" ^^^ NullExp
    | id ^^ VariableExp.apply
    | int ^^ ConstExp.apply
    | recordCreation
    | "(" ~> expression <~ ")" ^^ BracketExp.apply

  def operations: Parser[String] = """[+\-*/>]|(==)""".r

  def prio2ExpressionOperation: Parser[Expression => Expression] =
    operations ~ commit(expression)
      ^^ {
        case "+" ~ exp  => AddExp(_, exp)
        case "-" ~ exp  => SubExp(_, exp)
        case "*" ~ exp  => MultiExp(_, exp)
        case "/" ~ exp  => DivExp(_, exp)
        case ">" ~ exp  => GTExp(_, exp)
        case "==" ~ exp => EqExp(_, exp)
        case _          => throw new RuntimeException("Failure in parsing")
      }
      | "." ~> commit(id) ^^ { id => FieldAccess(_, id) }
      | arguments ^^ { case args => FunctionCallExp(_, args) }

  def prio2Expression: Parser[Expression] =
    prio3Expression ~ rep(prio2ExpressionOperation) ^^ { case exp ~ list =>
      list.foldLeft(exp)((exp, exp2) => exp2(exp))
    }

  def arguments: Parser[List[Expression]] =
    "(" ~> opt(expression) <~ ")"
      ^^ {
        case None      => Nil
        case Some(arg) => List(arg)
      }
      | "(" ~> commit(expression ~ rep("," ~> commit(expression)) <~ ")")
      ^^ mkList

  def field: Parser[Field] =
    id ~ (":" ~> expression) ^^ { case id ~ exp => (id, exp) }
  def recordCreation: Parser[RecordExp] =
    "{" ~> rep1sep(field, ",") <~ "}" ^^ { case a => RecordExp(a) }

  def expression: Parser[Expression] = prio2Expression
}

object ExpressionParser extends ExpressionParser {
  def parse(input: String): ParseResult[Expression] = {
    parse(expression, input)
  }
}

class StatementParser extends ExpressionParser {
  def condition: Parser[Expression] = "(" ~> expression <~ ")"
  def block: Parser[Stmt] = "{" ~> statement <~ "}"
  def assignment: Parser[Expression] = "=" ~> expression <~ ";"
  def elseBlock: Parser[Option[Stmt]] =
    "else" ~> commit(block) ^^ Some.apply
      | success(None)

  def singleStmt: Parser[Stmt] = failure("Helper to fix formatting")
    | "*" ~> commit(expression ~ assignment)
    ^^ { case ponterExp ~ newValueExp => StoreStmt(ponterExp, newValueExp) }
    | "(" ~> commit(
      ("*" ~> expression <~ ")") ~ ("." ~> id) ~ assignment
    )
    ^^ { case ponterExp ~ id ~ newValueExp =>
      RecordStoreStmt(ponterExp, id, newValueExp)
    }
    | "output" ~> expression <~ ";" ^^ OutputStmt.apply
    | "if" ~> commit(condition ~ block ~ elseBlock)
    ^^ { case exp1 ~ thenStmt ~ elseStmt =>
      IfElseStmt(exp1, thenStmt, elseStmt)
    }
    | "while" ~> commit(condition ~ block)
    ^^ { case exp1 ~ thenStmt => WhileStmt(exp1, thenStmt) }
    | id ~ ("." ~> commit(id ~ assignment))
    ^^ { case name ~ (field ~ exp) => RecordAssignmentStmt(name, field, exp) }
    | id ~ commit(assignment)
    ^^ { case id ~ exp => AssignmentStmt(id, exp) }

  def returnGuardedStmt =
    singleStmt - "return"

  def statement: Parser[Stmt] =
    returnGuardedStmt ~ rep(returnGuardedStmt) ^^ { case stmt ~ stmtList =>
      stmtList.reverse match {
        case Nil => stmt
        case lastStmt :: restOfStmts =>
          SequenceStmt(
            stmt,
            restOfStmts.foldLeft(lastStmt)((stmt1, stmt2) =>
              SequenceStmt(stmt2, stmt1)
            )
          )
      }
    }
      | success(NopStmt)
}

object StatementParser extends StatementParser {
  def parse(input: String): ParseResult[Stmt] = {
    parse(statement, input)
  }
}

class FunctionParser extends StatementParser {
  def functionVariables: Parser[List[Id]] =
    "var" ~> commit(rep1sep(id, ",") <~ ";") ^^ { case value =>
      value
    }
      | success(Nil)

  def function: Parser[FunDecl] = failure("Helper to fix formatting")
    | id
    ~ ("(" ~> repsep(id, ",") <~ ")")
    ~ ("{" ~> functionVariables
      ~ statement
      ~ ("return" ~> expression <~ ";") <~ "}")
    ^^ { case name ~ params ~ (vars ~ funcStmt ~ returnExp) =>
      FunDecl(name, params, vars, funcStmt, returnExp)
    }
}

object FunctionParser extends FunctionParser {
  def parse(input: String): ParseResult[FunDecl] = {
    parse(function, input)
  }
}

object TipParser extends FunctionParser {
  def program: Parser[List[FunDecl]] =
    phrase(rep(function))

  def parse(input: String): ParseResult[List[FunDecl]] = {
    parse(program, input)
  }
}

def getFileContents(fileName: String) =
  try {
    Source.fromFile(fileName).mkString
  } catch {
    case _ =>
      println(
        "Can't find file '" + fileName +
          "'. Using the first argument as source code instead"
      )
      fileName
  }

@main def main(firstString: String, others: String*) =
  val fileContents = getFileContents(firstString)
  println("============================")
  println(TipParser.parse(fileContents))
