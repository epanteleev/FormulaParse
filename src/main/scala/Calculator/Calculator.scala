package Calculator

import scala.util.parsing.combinator.RegexParsers

sealed trait Expression

trait Math extends Expression {
  def eval: Double
}

case class Number(double: Double) extends Math{
  override def eval: Double = double
}
case class Summ (a:Math, b: Math) extends Math{
  override def eval: Double = a.eval + b.eval
}
case class Sub (a:Math, b: Math)extends Math{
  override def eval: Double = a.eval - b.eval
}
case class Prod (a:Math, b: Math)extends Math{
  override def eval: Double = a.eval * b.eval
}
case class Div(a:Math, b: Math)extends Math{
  override def eval: Double = a.eval / b.eval
}

case class id (name: String) extends Math{
  override def eval: Double = throw new Error("Calculator.id")
}

object Calculator extends RegexParsers {

  def number: Parser[Math] = """-?\d+(\.\d*)?""".r ^^ { n => Number(n.toDouble) }

  //def id: Parser[Expression] = "[a-zA-Z][a-zA-Z0-9_]*".r ^^ id

  def factor: Parser[Math] = number | "(" ~> expr <~ ")"

  def term  : Parser[Math] = factor ~ rep( ("*"  | "/") ~ factor) ^^ {
    case number ~ list => list.foldLeft(number) {
      case (x, "*" ~ y) => Prod(x,y)
      case (x, "/" ~ y) => Div(x,y)
    }
  }

  def expr  : Parser[Math] = term ~ rep("+" ~ term | "-" ~ term) ^^ {
    case number ~ list => list.foldLeft(number) {
      case (x, "+" ~ y) => Summ(x, y)
      case (x, "-" ~ y) => Sub(x, y)
    }
  }

  def apply(input: String): Double = parseAll(expr, input) match {
    case Success(result, i) =>
      println(result)
      // println(i)
      result.eval
    case failure : NoSuccess => scala.sys.error(failure.msg)
  }
}