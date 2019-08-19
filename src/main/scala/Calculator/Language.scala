package Calculator
import scala.util.parsing.combinator.RegexParsers

sealed trait Expr
case class Identifier(name: String) extends Expr
case class Func(id: Identifier, e: Identifier) extends Expr
case class Num(id: Double) extends Expr
case class RegFunc(name: String, arg: Double) extends Expr

object Language extends RegexParsers {

  def number: Parser[Num] = """-?\d+(\.\d*)?""".r ^^ { n => Num(n.toDouble) }

  def id: Parser[Identifier] = "[a-zA-Z][a-zA-Z0-9_]*".r ^^ { str => Identifier(str)}

  def expr  : Parser[Func] = "def" ~ id ~ ("{" ~> id <~ "}") ^^
    { pair =>  Func(pair._1._2, pair._2)}

  def apply(input: String): Unit = parseAll(expr, input) match {
    case Success(result, i) => result match {
      case Func(name, body) => println("|- " + name + "->" + body)
      case RegFunc(name, arg: Double) => val l = name match {
        case "sin" => math.sin(arg)
        case "print" => println(arg)
      }

    }
      //println(result)
      // println(i)
     // result.eval
    case failure : NoSuccess => scala.sys.error(failure.msg)
  }
}