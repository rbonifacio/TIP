package br.unb.cic.tip

import java.nio.file.Paths
import scala.io.Source

import br.unb.cic.tip.Stmt.*
import br.unb.cic.tip.Expression.*

import scala.util.parsing.combinator._
import scala.util.parsing.input.Positional

class BasicParser extends RegexParsers {
  def id: Parser[Id] = """[a-zA-Z]+""".r
  def int: Parser[Int] = """-?[0-9]+""".r ^^ { (s) => Integer(s) }
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

  def prio3Expression: Parser[Expression] = failure("Helper to fix formatting")
    | "input" ^^^ InputExp
    | "alloc" ~> expression ^^ AllocExp.apply
    | "&" ~> id ^^ LocationExp.apply
    | "*" ~> expression ^^ LoadExp.apply
    | "null" ^^^ NullExp
    | directFunctionCall
    | id ^^ VariableExp.apply
    | int ^^ ConstExp.apply
    | recordCreation
    | "(" ~> expression <~ ")" ^^ BracketExp.apply

  def operations: Parser[String] = """[+\-*/>]|(==)""".r

  def prio2ExpressionOperation: Parser[Expression => Expression] =
    failure("Helper to fix formatting")
      | operations ~ commit(expression)
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
      | arguments ^^ { case args =>
        IndirectFunctionCallExp(_, args)
      }

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
  def directFunctionCall: Parser[DirectFunctionCallExp] =
    id ~ arguments ^^ { case id ~ args => DirectFunctionCallExp(id, args) }

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
  def prio0Stmt: Parser[Stmt] = failure("Helper to fix formatting")
    | "*" ~> expression ~ ("=" ~> expression <~ ";")
    ^^ { case ponterExp ~ newValueExp => StoreStmt(ponterExp, newValueExp) }
    | ("(" ~> "*" ~> expression <~ ")")
    ~ ("." ~> id) ~ ("=" ~> expression <~ ";")
    ^^ { case ponterExp ~ id ~ newValueExp =>
      RecordStoreStmt(ponterExp, id, newValueExp)
    }
    | id ~ ("=" ~> expression <~ ";")
    ^^ { case id ~ exp => AssignmentStmt(id, exp) }
    | id ~ ("." ~> id) ~ ("=" ~> expression <~ ";")
    ^^ { case name ~ field ~ exp => RecordAssignmentStmt(name, field, exp) }
    | "output" ~> expression <~ ";" ^^ OutputStmt.apply
    | ("if" ~> "(" ~> expression <~ ")")
    ~ ("{" ~> statement <~ "}")
    ~ opt("else" ~> "{" ~> commit(statement) <~ "}")
    ^^ { case exp1 ~ thenStmt ~ elseStmt =>
      IfElseStmt(exp1, thenStmt, elseStmt)
    }
    | ("while" ~> "(" ~> expression <~ ")")
    ~ ("{" ~> statement <~ "}")
    ^^ { case exp1 ~ thenStmt => WhileStmt(exp1, thenStmt) }

  def statement: Parser[Stmt] =
    prio0Stmt ~ rep(prio0Stmt) ^^ { case stmt ~ stmtList =>
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
}

object StatementParser extends StatementParser {
  def parse(input: String): ParseResult[Stmt] = {
    parse(statement, input)
  }
}

class FunctionParser extends StatementParser {
  def functionVariables: Parser[List[Id]] =
    opt("var" ~> repsep(id, ",") <~ ";") ^^ {
      case None        => Nil
      case Some(value) => value
    }

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
