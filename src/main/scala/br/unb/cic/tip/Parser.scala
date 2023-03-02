package br.unb.cic.tip

import br.unb.cic.tip.Stmt.*
import br.unb.cic.tip.Expression.*

import scala.util.parsing.combinator._

class ExpressionParser extends RegexParsers {
  def idParser: Parser[VariableExp] =
    """[a-zA-Z]+""".r ^^ { (s) => VariableExp(s) }
  def intParser: Parser[ConstExp] =
    """-?[0-9]+""".r ^^ { (s) => ConstExp(Integer(s)) }
  def inputParser: Parser[Expression] =
    "input" ^^ { _ => InputExp }

  def opParser: Parser[String] =
    """[+\-*/>]|(==)""".r ^^ { (s) => s }

  def prio1Parser: Parser[Expression] =
    idParser | intParser | "(" ~> expressionParser <~ ")"

  def prio0OperationsParser: Parser[Expression => Expression] =
    opParser ~ prio1Parser ^^ {
      case "+" ~ exp  => AddExp(_, exp)
      case "-" ~ exp  => SubExp(_, exp)
      case "*" ~ exp  => MultiExp(_, exp)
      case "/" ~ exp  => DivExp(_, exp)
      case ">" ~ exp  => GTExp(_, exp)
      case "==" ~ exp => EqExp(_, exp)
      case _          => throw new RuntimeException("Failure in parsing")
    }

  def prio0Parser: Parser[Expression] =
    prio1Parser ~ rep(prio0OperationsParser) ^^ { case exp ~ list =>
      list.foldLeft(exp)((exp, exp2) => exp2(exp))
    }

  def expressionParser: Parser[Expression] =
    prio0Parser | inputParser
}

object TipParser extends ExpressionParser {
  def mainParse(s: String) = parse(expressionParser, s)
}

@main def main(fileName: String, others: String*) =
  println(TipParser.mainParse(fileName))
