package Calculator

import scala.util.parsing.combinator.RegexParsers

sealed trait Expression

case class Number(double: Double) extends Expression
case class Summ (a:Expression, b: Expression) extends Expression
case class Sub (a:Expression, b: Expression) extends Expression
case class Prod (a:Expression, b: Expression) extends Expression
case class Div(a:Expression, b: Expression) extends Expression
case class Id (name: String) extends Expression
case class Pow(expr: Expression, degree: Double ) extends Expression

case class Function1(name: Id, expr: Expression) extends Expression
case class Der(exp: Expression) extends Expression

class AST extends RegexParsers {

  def number: Parser[Number] = """-?\d+(\.\d*)?""".r ^^ { n => Number(n.toDouble) }

  def id: Parser[Id] = "[a-zA-Z][a-zA-Z0-9_]*".r ^^ { str => Id(str) }

  def const: Parser[Pow] = id ^^ { named => Pow(named,1) }

  def factor: Parser[Expression] = derivative | funcl | const | number | "(" ~> expr <~ ")"

  def funcl: Parser[Expression] = id ~ ("(" ~> expr  ~ ("," ~> expr).? <~ ")" ) ^^
    {case  i ~ (arg ~ None)=> Function1(i, arg)
    case  Id("pow") ~ (arg1 ~ Some(Number(b))) => Pow(arg1, b)}

  def term: Parser[Expression] = factor ~ (("*" | "/") ~ factor).* ^^ {
    case number ~ list => list.foldLeft(number) {
      case (x, "*" ~ y) => Prod(x, y)
      case (x, "/" ~ y) => Div(x, y)
    }
  }
  def derivative: Parser[Expression] = "(" ~> expr <~ ")`" ^^ { e => Der(e)}

  def expr: Parser[Expression] = term ~ ("+" ~ term | "-" ~ term).* ^^ {
    case number ~ list => list.foldLeft(number) {
      case (x, "+" ~ y) => Summ(x, y)
      case (x, "-" ~ y) => Sub(x, y)
    }
  }

  def parse(input: String) : Expression = parseAll(expr, input) match {
    case Success(result, string) =>
      //println(result)
      result
    case failure: NoSuccess => scala.sys.error(failure.msg)
  }

}

object AST  extends RegexParsers{
  def apply(input: String): Expression =  new AST parse input
}

object Calculate{
  lazy val defaultConst: Map[String, Double] = Map("PI" -> math.Pi, "e" -> math.E)
  lazy val defaultFunction1: Map[String, Double => Double] = Map(
    "sin" -> ((x:Double) => math.sin(x)),
    "sin`" -> ((x:Double) => math.cos(x)),
    "cos" -> ((x:Double) => math.cos(x)),
    "cos`" -> ((x:Double) => -math.sin(x)),
    "tan" -> ((x:Double) => math.tan(x)),
    "atan" -> ((x: Double) => math.atan(x)),
    "tan`" -> ((x:Double) => 1.0 / (math.cos(x) * math.cos(x))))

  def apply(str: String, const: Map[String, Double] = defaultConst) : Double = {
    def eval (expr: Expression): Double = {
      expr match {
        case Number(x) => x
        case Id(name) => const(name)
        case Summ(a, b) => eval(a) + eval(b)
        case Sub(a, b) => eval(a) - eval(b)
        case Prod(a, b) => eval(a) * eval(b)
        case Div(a, b) => eval(a) / eval(b)
        case Function1(id, expr) => defaultFunction1(id.name)(eval(expr))
        case Der(exp) => eval(der(exp))
        case Pow(x,d) => math.pow(eval(x),d)
      }
    }
    def der(expr: Expression): Expression ={
      expr match {
        case Prod(a, b) => Summ(Prod(der(a),b),Prod(a,der(b)))
        case Div(a,b) => Div(Sub(Prod(der(a),b),Prod(a,der(b))),Prod(b,b))
        case Number(x) => Number(0)
        case Summ(a,b) => Summ(der(a),der(b))
        case Sub(a,b) => Sub(der(a),der(b))
        case Id(x) => Number(0)
        case Function1(name,arg) => {
          arg match {
            case Id(x) => Function1(name,arg)
            case _ => Prod(Function1(Id(name.name + "`"),arg),der(arg))
          }
        }
        case Pow(e, d) =>  e match {
          case Number(e) => Number(0)
          case Id(e) => Prod(Number(d), Pow(Id(e), d - 1.0))
          case e  => Prod(Prod(Number(d), Pow(e, d - 1.0)),der(e))
        }
        case Der(exp) =>  throw new Error("Calculate.apply.der")
      }
    }
    eval(AST(str))
  }
}