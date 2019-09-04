package Calculator

import scala.util.parsing.combinator.RegexParsers

case class CalculateError(loc: Location, msg: String){
  override def toString = s"In $loc. $msg"
}

case class Location(line: Int, column: Int) {
  override def toString = s"line: $line pos:$column"
}

class Parse extends RegexParsers {

  lazy val eqvl: Map[String, String] = Map("==" -> "EQ", "!=" -> "NOTEQ")
  def number: Parser[Number] = """-?\d+(\.\d*)?""".r ^^ { n => Number(n.toDouble) }

  def id: Parser[String] = "[a-zA-Z][a-zA-Z0-9_]*".r ^^ { str => str toString }

  def variable: Parser[Variable] = id ^^ { name => Variable(name) }

  def varNum: Parser[Expression] = variable | number
  def factor: Parser[Expression] = funcl | varNum | "(" ~> expr <~ ")"

  def bool: Parser[Condition] = varNum ~ ("==" | "!=") ~ varNum ^^ { case lh ~ op ~ rh  => Condition(lh, eqvl(op), rh)}

  def ret: Parser[Return] = "return" ~> expr ^^ {n => Return(n)}

  def block: Parser[List[Expression]] = "{" ~> start <~ "}"

  def loop: Parser[Loop] = "while" ~> "(" ~> bool ~ ")" ~ block ^^ { case con ~ _ ~ bl => Loop(con,bl)}

  def condition: Parser[Expression] = "if" ~>"(" ~> bool ~")" ~ block ^^ { case cond ~ _ ~ bl => IfThen(cond,bl)  }

  def funcl: Parser[Expression] = id ~ ("(" ~> expr  ~ ("," ~> expr).? <~ ")" ) ^^ {
    case  "pow" ~ (arg1 ~ Some(Number(b))) => Pow(arg1, b)}

  def term: Parser[Expression] = factor ~ (("*" | "/") ~ factor).* ^^ {
    case number ~ list => list.foldLeft(number) {
      case (x, "*" ~ y) => Prod(x, y)
      case (x, "/" ~ y) => Div(x, y)
    }
  }

  def expr: Parser[Expression] = term ~ ("+" ~ term | "-" ~ term).* ^^ {
    case number ~ list => list.foldLeft(number) {
      case (x, "+" ~ y) => Summ(x, y)
      case (x, "-" ~ y) => Sub(x, y)
    }
  }

  def eq: Parser[Expression] =  id ~ "=" ~ expr ^^ { case name ~ e ~ exp => Equality(name,exp)}

  def start: Parser[List[Expression]] = rep(loop | condition | ret | eq | factor)

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

object MakeByteCode{

  def apply(str: String) : List[CodeOp] = {
    val res = Parse(str)

    @scala.annotation.tailrec
    def comp(result: List[Expression], list: List[CodeOp]):List[CodeOp] = {
      result match {
        case head :: l => comp(result.tail, list ++ head.toByteCode)
        case Nil => list
      }

    }
    comp(res,List())
  }
}