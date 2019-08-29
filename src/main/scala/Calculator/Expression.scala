package Calculator

sealed trait Expression{
  def eval: Double
  def deriv(variable: String): Expression
}

case class Number(double: Double) extends Expression{
  override def eval: Double = double

  override def deriv(variable: String): Expression = Number(0)

  override def toString: String = double toString
}
case class Summ(a:Expression, b: Expression) extends Expression{
  override def eval: Double = a.eval + b.eval

  override def deriv(variable: String): Expression = {
    (a,b) match {
      case (arg1,Number(data)) => arg1.deriv(variable)
      case others => Summ(a.deriv(variable), b.deriv(variable))
    }
  }

  override def toString: String = s"($a + $b)"
}
case class Sub (a:Expression, b: Expression) extends Expression{
  override def eval: Double = a.eval - b.eval

  override def deriv(variable: String): Expression = (a,b) match {
    case (arg1,Number(data)) => arg1.deriv(variable)
    case others => Sub(a.deriv(variable), b.deriv(variable))
  }

  override def toString: String = s"($a - $b)"
}

case class Prod (a:Expression, b: Expression) extends Expression{
  override def eval: Double = a.eval * b.eval

  override def deriv(variable: String): Expression = {
    (a, b) match {
      case (exp,Number(0.0)) => Number(0.0)
      case (exp, Number(1.0)) => exp deriv variable
      case others => Summ (Prod (a deriv  variable , b), Prod (a, b deriv  variable ) )
    }
  }

  override def toString: String = s"$a * $b"
}

case class Div (a:Expression, b: Expression) extends Expression{
  override def eval: Double = a.eval / b.eval

  override def deriv(variable: String): Expression =
    Div(Sub(Prod(a.deriv(variable),b),Prod(a,b.deriv(variable))),Prod(b,b))

  override def toString: String = s"$a / $b"
}

case class Constant(name: String, data: Double) extends Expression{
  override def eval: Double = data

  override def deriv(variable: String): Expression =
    if (name == variable)
      Number(1.0)
    else
      Number(0)

  override def toString: String = s"<$name=$data>"
}
case class Pow(expr: Expression, degree: Double ) extends Expression{
  override def eval: Double =   math.pow(expr.eval,degree)

  override def deriv(variable: String): Expression =
    Prod(Prod(Number(degree), Pow(expr, degree - 1.0)), expr deriv variable)


  override def toString: String = s"($expr ^ $degree)"
}

case class Not(expr: Expression) extends Expression{
  override def eval: Double = -expr.eval

  override def deriv(variable: String): Expression = Not(expr.deriv(variable))

  override def toString: String = s"-$expr"
}
case class Sin(expr: Expression) extends Expression{
  override def eval: Double = math.sin(expr eval)

  override def deriv(variable: String): Expression = Prod(Cos(expr),expr deriv variable)

  override def toString: String = s"sin( $expr )"
}
case class Cos(expr: Expression) extends Expression{
  override def eval: Double = math.cos(expr eval)

  override def toString: String = s"cos( $expr )"

  override def deriv(variable: String): Expression =
    Prod(Not(Sin(expr)),expr deriv variable)
}
case class Der(exp: Expression,variable: String) extends Expression{
  override def eval: Double = (exp deriv variable) eval

  override def deriv(v: String): Expression = (exp deriv variable) deriv v

  override def toString: String = s"( $exp )`$variable"
}

case class Tan(expr: Expression) extends Expression{
  override def eval: Double = math.tan(expr.eval)

  override def deriv(variable: String): Expression =
    Prod(Div(Number(1.0),Pow(Cos(expr),2)),expr deriv variable)

  override def toString: String = s"tan( $expr )"
}

object Creator{
  val map: Map[String, Expression => Expression] = Map(
    "tan" -> ((e: Expression) => Tan(e)),
    "cos" -> ((e: Expression) => Cos(e)),
    "sin" ->((e: Expression) => Sin(e))
  )
}