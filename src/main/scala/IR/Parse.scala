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

  def number: Parser[Number] = """-?\d+(\.\d*)?""".r ^^ { n => Number(n.toDouble, tDouble()) }

  def id: Parser[String] = "[a-zA-Z][a-zA-Z0-9_]*".r ^^ { str => str toString }

  def variable: Parser[Variable] = id ^^ { name => Variable(name) }

  def varNum: Parser[Expression] = variable | number

  def factor: Parser[Expression] = funcl | varNum | "(" ~> expr <~ ")"

  def bool: Parser[Condition] = expr ~ ("==" | "!=" | "<" | ">") ~ expr ^^ { case lh ~ op ~ rh => Condition(lh, CmpOp(op), rh) }

  def ret: Parser[Return] = "return" ~> expr ^^ {n => Return(n)}

  def block: Parser[List[Expression]] = "{" ~> start <~ "}"

  def loop: Parser[Loop] = "while" ~> "(" ~> bool ~ ")" ~ block ^^ { case con ~ _ ~ bl => Loop(con,bl)}

  def condition: Parser[Expression] = "if" ~>"(" ~> bool ~")" ~ block ~ ("else" ~ block).? ^^ {
    case cond ~ _ ~ bl ~ None => IfThen(cond,bl, Nil)
    case cond ~ _ ~ bl ~ Some(_ ~ elseBl) =>  IfThen(cond,bl, elseBl)
  }

  def funcl: Parser[Expression] = id ~ ("(" ~> (expr).?  ~ ("," ~> expr).* <~ ")" ) ^^ {
    case  name ~ (Some(arg1) ~ list) => CallFunction(name, List(arg1) ++ list)
    case name ~(None ~ Nil) => CallFunction(name, Nil)
  }

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

  def assigment: Parser[Expression] = id ~ "=" ~ expr ^^ { case name ~ e ~ exp => Assignment(name, exp) }

  def start: Parser[List[Expression]] = rep(loop | condition | ret | assigment | expr | factor)

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