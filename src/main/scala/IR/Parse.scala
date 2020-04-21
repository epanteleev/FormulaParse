package IR

import IR.MIR.CmpOp

import scala.util.parsing.combinator.RegexParsers

case class CalculateError(loc: Location, msg: String) {
  override def toString = s"In $loc. $msg"
}

case class Location(line: Int, column: Int) {
  override def toString = s"line: $line pos:$column"
}

class Parse extends RegexParsers {
  def integer: Parser[Number] = """-?\d+""".r ^^ { n => Number(tInt(n.toInt)) }

  def double: Parser[Number] = """-?\d+\.(\d*)?""".r ^^ { n => Number(tDouble(n.toDouble)) }

  def number: Parser[Number] = double | integer

  def id: Parser[String] = "[a-zA-Z][a-zA-Z0-9_]*".r

  def variable: Parser[Variable] = id ^^ { name => Variable(name) }

  def bool: Parser[Condition] = expr ~ ("==" | "!=" | "<" | ">") ~ expr ^^ { case lh ~ op ~ rh => Condition(lh, CmpOp(op), rh) }

  def ret: Parser[Return] = "return".? ~> expr ^^ { n => Return(n) }

  def block: Parser[List[Expression]] = "{" ~> start <~ "}"

  def loop: Parser[Loop] = "while" ~> "(" ~> bool ~ ")" ~ block ^^ { case con ~ _ ~ bl => Loop(con, bl) }

  def condition: Parser[Expression] = "if" ~> "(" ~> bool ~ ")" ~ block ~ ("else" ~ block).? ^^ {
    case cond ~ _ ~ bl ~ None => IfThen(cond, bl, Nil)
    case cond ~ _ ~ bl ~ Some(_ ~ elseBl) => IfThen(cond, bl, elseBl)
  }

  def varOrNum: Parser[Expression] = variable | number

  def functionCall: Parser[Expression] = id ~ ("(" ~> (expr).? ~ ("," ~> expr).* <~ ")") ^^ {
    case name ~ (Some(arg1) ~ list) => CallFunction(name, List(arg1) ++ list)
    case name ~ (None ~ Nil) => CallFunction(name, Nil)
  }

  def factor: Parser[Expression] = functionCall | varOrNum | "(" ~> expr <~ ")"

  def term: Parser[Expression] = factor ~ (("*" | "/") ~ factor).* ^^ {
    case number ~ list => list.foldLeft(number) {
      case (x, "*" ~ y) => Prod(x, y)
      case (x, "/" ~ y) => Div(x, y)
    }
  }

  def expr: Parser[Expression] = term ~ ("+" ~ term | "-" ~ term).* ^^ {
    case number ~ list => list.foldLeft(number) {
      case (x, "+" ~ y) => Sum(x, y)
      case (x, "-" ~ y) => Sub(x, y)
    }
  }

  def assignment: Parser[Expression] = "val" ~> id ~ "=" ~ expr ^^ { case name ~ e ~ exp => Assignment(name, exp) }

  def start: Parser[List[Expression]] = rep(loop | condition | assignment | ret | expr | factor)

  def parse(input: String) : List[Expression] = {
      parseAll(start, input) match {
      case Success(result, string) => result
      case NoSuccess(msg, next) => scala.sys.error(
        CalculateError(Location(next.pos.line, next.pos.column), msg).toString)
    }
  }

}

object Parse  extends RegexParsers{
  def apply(input: String): List[Expression] = new Parse parse input
}