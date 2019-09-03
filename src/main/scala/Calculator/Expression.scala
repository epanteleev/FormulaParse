package Calculator

import scala.language.postfixOps

sealed trait Expression{
  def eval: Double
  def derivative(variable: String): Expression
}

case class Number(double: Double) extends Expression{
  override def eval: Double = double

  override def derivative(variable: String): Expression = Number(0)

  override def toString: String = double toString
}

case class Summ(a:Expression, b: Expression) extends Expression{
  override def eval: Double = a.eval + b.eval

  override def derivative(variable: String): Expression =
    Summ(a derivative variable, b derivative variable)

  override def toString: String = s"($a + $b)"
}

case class Sub (a:Expression, b: Expression) extends Expression{
  override def eval: Double = a.eval - b.eval

  override def derivative(variable: String): Expression =
    Sub(a derivative variable, b derivative variable )

  override def toString: String = s"($a - $b)"
}

case class Prod (a:Expression, b: Expression) extends Expression{
  override def eval: Double = a.eval * b.eval

  override def derivative(variable: String): Expression =
    Summ(Prod(a derivative variable, b), Prod (a, b derivative variable))

  override def toString: String = s"$a * $b"
}

case class Div (a:Expression, b: Expression) extends Expression{
  override def eval: Double = a.eval / b.eval

  override def derivative(variable: String): Expression =
    Div(Sub(Prod(a derivative variable, b),Prod(a, b derivative variable)),Prod(b, b))

  override def toString: String = s"$a / $b"
}

case class Constant(name: String, data: Double) extends Expression{
  override def eval: Double = data

  override def derivative(variable: String): Expression =
    if (name == variable) Number(1.0) else Number(0)

  override def toString: String = s"<$name=$data>"
}

case class Pow(expr: Expression, degree: Double) extends Expression{
  override def eval: Double = math.pow(expr eval,degree)

  override def derivative(variable: String): Expression =
    Prod(Prod(Number(degree), Pow(expr, degree - 1.0)), expr derivative variable)


  override def toString: String = s"($expr ^ $degree)"
}

case class Neg(expr: Expression) extends Expression{
  override def eval: Double = -expr.eval

  override def derivative(variable: String): Expression = Neg(expr derivative variable)

  override def toString: String = s"-($expr)"
}

case class Sin(expr: Expression) extends Expression{
  override def eval: Double = math.sin(expr eval)

  override def derivative(variable: String): Expression = Prod(Cos(expr),expr derivative variable)

  override def toString: String = s"sin($expr)"
}

case class Cos(expr: Expression) extends Expression{
  override def eval: Double = math.cos(expr eval)

  override def toString: String = s"cos($expr)"

  override def derivative(variable: String): Expression =
    Prod(Neg(Sin(expr)),expr derivative variable)
}

case class Tan(expr: Expression) extends Expression{
  override def eval: Double = math.tan(expr.eval)

  override def derivative(variable: String): Expression =
    Prod(Div(Number(1.0),Pow(Cos(expr),2)), expr derivative variable)

  override def toString: String = s"tan($expr)"
}

case class Der(exp: Expression,variable: String) extends Expression{
  override def eval: Double = (exp derivative variable) eval

  override def derivative(v: String): Expression = (exp derivative variable) derivative v

  override def toString: String = s"($exp)`$variable"
}

object Creator{
  val map: Map[String, Expression => Expression] = Map(
    "tan" -> ((e: Expression) => Tan(e)),
    "cos" -> ((e: Expression) => Cos(e)),
    "sin" ->((e: Expression) => Sin(e))
  )
}