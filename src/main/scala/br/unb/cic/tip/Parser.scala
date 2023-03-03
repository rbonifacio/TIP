package br.unb.cic.tip

import java.nio.file.Paths
import scala.io.Source

import br.unb.cic.tip.Stmt.*
import br.unb.cic.tip.Expression.*

import scala.util.parsing.combinator._
import scala.util.parsing.input.Positional

class BasicParser extends RegexParsers {
  def idParser: Parser[Id] = """[a-zA-Z]+""".r
  def intParser: Parser[Int] = """-?[0-9]+""".r ^^ { (s) => Integer(s) }
}

class ExpressionParser extends BasicParser {
  def inputParser: Parser[Expression] = "input" ^^^ InputExp
  // def directFunctionCallParser: Parser[DirectFunctionCallExp] =
  //   idParser ~ ("(" ~> repsep(expressionParser, ",") <~ ")")
  //     ^^ { case id ~ args => DirectFunctionCallExp(id, args) }
  def indirectFunctionCallParser: Parser[IndirectFunctionCallExp] =
    variableParser ~ ("(" ~> repsep(expressionParser, ",") <~ ")")
      ^^ { case exp ~ args => IndirectFunctionCallExp(exp, args) }
  def variableParser: Parser[VariableExp] =
    idParser ^^ { s => VariableExp(s) }
  def constParser: Parser[ConstExp] =
    intParser ^^ { s => ConstExp(Integer(s)) }

  def fieldParser: Parser[Field] =
    idParser ~ (":" ~> expressionParser) ^^ { case id ~ exp =>
      (id, exp)
    }
  def recordCreation: Parser[RecordExp] =
    "{" ~> rep1sep(fieldParser, ",") <~ "}" ^^ { case a => RecordExp(a) }

  def opParser: Parser[String] = """[+\-*/>]|(==)""".r

  def prio2ExpParser: Parser[Expression] = failure("Helper to fix formatting")
    | inputParser
    | "alloc" ~> expressionParser ^^ AllocExp.apply
    | "&" ~> idParser ^^ LocationExp.apply
    | "*" ~> expressionParser ^^ LoadExp.apply
    | "null" ^^^ NullExp
    | indirectFunctionCallParser
    | variableParser
    | constParser
    | recordCreation
    | "(" ~> expressionParser <~ ")" ^^ BracketExp.apply

  def prio1ExpOpParser: Parser[Expression => Expression] =
    opParser ~ prio2ExpParser ^^ {
      case "+" ~ exp  => AddExp(_, exp)
      case "-" ~ exp  => SubExp(_, exp)
      case "*" ~ exp  => MultiExp(_, exp)
      case "/" ~ exp  => DivExp(_, exp)
      case ">" ~ exp  => GTExp(_, exp)
      case "==" ~ exp => EqExp(_, exp)
      case _          => throw new RuntimeException("Failure in parsing")
    }

  def prio1ExpParser: Parser[Expression] =
    prio2ExpParser ~ rep(prio1ExpOpParser) ^^ { case exp ~ list =>
      list.foldLeft(exp)((exp, exp2) => exp2(exp))
    }

  def prio0ExpOpParser: Parser[Expression => Expression] =
    "." ~> idParser ^^ { id => FieldAccess(_, id) }

  def prio0ExpParser: Parser[Expression] =
    prio1ExpParser ~ rep(prio0ExpOpParser) ^^ { case exp ~ ids =>
      ids.foldLeft(exp)((exp, it) => it(exp))
    }

  def expressionParser: Parser[Expression] = prio0ExpParser
}

class StatementParser extends ExpressionParser {
  def prio0StmtParser: Parser[Stmt] = failure("Helper to fix formatting")
    | "*" ~> expressionParser ~ ("=" ~> expressionParser <~ ";")
    ^^ { case ponterExp ~ newValueExp => StoreStmt(ponterExp, newValueExp) }
    | ("(" ~> "*" ~> expressionParser <~ ")")
    ~ ("." ~> idParser) ~ ("=" ~> expressionParser <~ ";")
    ^^ { case ponterExp ~ id ~ newValueExp =>
      RecordStoreStmt(ponterExp, id, newValueExp)
    }
    | idParser ~ ("=" ~> expressionParser <~ ";")
    ^^ { case id ~ exp => AssignmentStmt(id, exp) }
    | idParser ~ ("." ~> idParser) ~ ("=" ~> expressionParser <~ ";")
    ^^ { case name ~ field ~ exp => RecordAssignmentStmt(name, field, exp) }
    | "output" ~> expressionParser <~ ";" ^^ OutputStmt.apply
    | ("if" ~> "(" ~> expressionParser <~ ")")
    ~ ("{" ~> statementParser <~ "}")
    ~ opt("else" ~> "{" ~> commit(statementParser) <~ "}")
    ^^ { case exp1 ~ thenStmt ~ elseStmt =>
      IfElseStmt(exp1, thenStmt, elseStmt)
    }
    | ("while" ~> "(" ~> expressionParser <~ ")")
    ~ ("{" ~> statementParser <~ "}")
    ^^ { case exp1 ~ thenStmt => WhileStmt(exp1, thenStmt) }

  def statementParser: Parser[Stmt] =
    prio0StmtParser ~ rep(prio0StmtParser) ^^ { case stmt ~ stmtList =>
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

class FunctionParser extends StatementParser {
  def functionVariablesParser: Parser[List[Id]] =
    opt("var" ~> repsep(idParser, ",") <~ ";") ^^ {
      case None        => Nil
      case Some(value) => value
    }

  def functionParser: Parser[FunDecl] = failure("Helper to fix formatting")
    | idParser
    ~ ("(" ~> repsep(idParser, ",") <~ ")")
    ~ ("{" ~> functionVariablesParser
      ~ statementParser
      ~ ("return" ~> expressionParser <~ ";")
      <~ "}")
    ^^ { case name ~ params ~ (vars ~ funcStmt ~ returnExp) =>
      FunDecl(name, params, vars, funcStmt, returnExp)
    }
}

object TipParser extends FunctionParser {
  def programParser: Parser[List[FunDecl]] =
    rep1(functionParser)

  def apply(s: String) = parse(programParser, s)
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
  println(TipParser.apply(fileContents))
