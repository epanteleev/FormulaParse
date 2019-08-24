package Calculator

import Calculator.AST.parseAll

import scala.util.parsing.combinator.RegexParsers

sealed trait Expression

case class Number(double: Double) extends Expression
case class Summ (a:Expression, b: Expression) extends Expression
case class Sub (a:Expression, b: Expression) extends Expression
case class Prod (a:Expression, b: Expression) extends Expression
case class Div(a:Expression, b: Expression) extends Expression
case class Id (name: String) extends Expression
case class Function(id: Id, expr: Expression) extends Expression
case class Der(exp: Expression) extends Expression

class AST(const: Map[String, Double] ) extends RegexParsers {

  def number: Parser[Number] = """-?\d+(\.\d*)?""".r ^^ { n => Number(n.toDouble) }

  def id: Parser[Id] = "[a-zA-Z][a-zA-Z0-9_]*".r ^^ { str => Id(str) }

  def factor: Parser[Expression] = derivative | funcl | id | number | "(" ~> expr <~ ")"

  def funcl: Parser[Function] = id ~ ("(" ~> expr  ~ ("," ~ expr <~ ")").? <~ ")" ) ^^ (pair => Function(pair._1, pair._2._1))

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
  def apply(input: String, const: Map[String, Double]): Expression =  new AST(const) parse input
}

object Calculate{
  lazy val defaultConst: Map[String, Double] = Map("PI" -> math.Pi)
  lazy val defaultFunction: Map[String, Double => Double] = Map(
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
        case Function(id, expr) => defaultFunction(id.name)(eval(expr))
        case Der(exp) => {
          //println(exp)
          val newAst = der(exp)
          println(newAst)
          eval(newAst)
        }
      }
    }
    def der(expr: Expression): Expression ={
      expr match {
        case Prod(a, b) => Summ(Prod(der(a),b),Prod(a,der(b)))
        case Div(a,b) => Div(Sub(Prod(der(a),b),Prod(a,der(b))),Prod(b,b))
        case Number(x) => Number(0)
        case Summ(a,b) => Summ(der(a),der(b))
        case Sub(a,b) => Sub(der(a),der(b))
        case Id(x) => Id(x)
        case Function(name,arg) => {
          arg match {
            case Id(x) => Function(Id(name.name + "`"),arg)
            case _ => Prod(Function(Id(name.name + "`"),arg),der(arg))
          }
        }
      }
    }
    eval(AST(str,const))
  }
}