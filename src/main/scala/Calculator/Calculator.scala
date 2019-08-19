package Calculator

import scala.util.parsing.combinator.RegexParsers

sealed trait Expression

case class Number(double: Double) extends Expression
case class Summ (a:Expression, b: Expression) extends Expression
case class Sub (a:Expression, b: Expression)extends Expression
case class Prod (a:Expression, b: Expression)extends Expression
case class Div(a:Expression, b: Expression)extends Expression
case class Id (name: String) extends Expression
case class Function(id: Id, expr: Expression) extends Expression

object Calculator extends RegexParsers {
  lazy val default_const = Map("PI" -> math.Pi)
  lazy val default_function = Map("sin" -> ((x:Double) => math.sin(x)),
    "cos" -> ((x:Double) => math.cos(x)))

  def number: Parser[Number] = """-?\d+(\.\d*)?""".r ^^ { n => Number(n.toDouble) }

  def id: Parser[Id] = "[a-zA-Z][a-zA-Z0-9_]*".r ^^ { str => Id(str) }

  def factor: Parser[Expression] = funcl | id | number | "(" ~> expr <~ ")"

  def funcl: Parser[Function] = id ~ ("(" ~> expr <~ ")") ^^ (pair => Function(pair._1, pair._2))

  def term: Parser[Expression] = factor ~ rep(("*" | "/") ~ factor) ^^ {
    case number ~ list => list.foldLeft(number) {
      case (x, "*" ~ y) => Prod(x, y)
      case (x, "/" ~ y) => Div(x, y)
    }
  }

  def expr: Parser[Expression] = term ~ rep("+" ~ term | "-" ~ term) ^^ {
    case number ~ list => list.foldLeft(number) {
      case (x, "+" ~ y) => Summ(x, y)
      case (x, "-" ~ y) => Sub(x, y)
    }
  }


  def apply(input: String,const: Map[String, Double] = default_const,
            funcl: Map[String, Double => Double] = default_function): Double = {
    def eval (expr: Expression): Double = {
      expr match {
        case Number(x) => x
        case Id(name) => const(name)
        case Summ(a,b) => eval(a) + eval(b)
        case Sub(a,b) => eval(a) - eval(b)
        case Prod(a,b) => eval(a) * eval(b)
        case Div(a,b) => eval(a) / eval(b)
        case Function(id, expr) => funcl(id.name)(eval(expr))
      }
    }
    parseAll(expr, input) match {
      case Success(result, string) =>
        println(result)
        eval(result)

      case failure: NoSuccess => scala.sys.error(failure.msg)
    }
  }
}