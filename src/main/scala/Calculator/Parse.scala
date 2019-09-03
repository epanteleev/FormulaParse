package Calculator

import scala.language.postfixOps
import scala.util.parsing.combinator.RegexParsers

case class CalculateError(loc: Location, msg: String){
  override def toString = s"In $loc. $msg"
}

case class Location(line: Int, column: Int) {
  override def toString = s"line: $line pos:$column"
}

class Parse(const: Map[String, Double]) extends RegexParsers {

  def number: Parser[Number] = """-?\d+(\.\d*)?""".r ^^ { n => Number(n.toDouble) }

  def id: Parser[String] = "[a-zA-Z][a-zA-Z0-9_]*".r ^^ { str => str toString }

  def variable: Parser[Constant] = id ^^ { name => Constant(name,const(name)) }

  def factor: Parser[Expression] = neg | derivative | funcl | variable | number | "(" ~> expr <~ ")"

  def neg: Parser[Expression] = "-" ~> expr ^^ {exp => Neg(exp)}

  def funcl: Parser[Expression] = id ~ ("(" ~> expr  ~ ("," ~> expr).? <~ ")" ) ^^
    {case  "sin" ~ (arg ~ None)=> Sin(arg)
    case  "cos" ~ (arg ~ None)=> Cos(arg)
    case  "tan" ~ (arg ~ None)=> Tan(arg)
    case  "pow" ~ (arg1 ~ Some(Number(b))) => Pow(arg1, b)}

  def term: Parser[Expression] = factor ~ (("*" | "/") ~ factor).* ^^ {
    case number ~ list => list.foldLeft(number) {
      case (x, "*" ~ y) => Prod(x, y)
      case (x, "/" ~ y) => Div(x, y)
    }
  }

  def derivative: Parser[Expression] = ("(" ~> expr <~ ")`") ~ id ^^ { case e ~ i => Der(e,i)}

  def expr: Parser[Expression] = term ~ ("+" ~ term | "-" ~ term).* ^^ {
    case number ~ list => list.foldLeft(number) {
      case (x, "+" ~ y) => Summ(x, y)
      case (x, "-" ~ y) => Sub(x, y)
    }
  }

  def parse(input: String) : Either[CalculateError,Expression] = parseAll(expr, input) match {
    case Success(result, string) => Right(result)
    case NoSuccess(msg, next) => Left(CalculateError(Location(next.pos.line, next.pos.column),msg))
  }
}

object Parse  extends RegexParsers{
  def apply(input: String, const: Map[String, Double]): Either[CalculateError,Expression] =
    new Parse(const) parse input
}