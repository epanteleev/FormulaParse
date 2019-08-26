package Calculator

sealed trait Expression{
  def eval: Double
  def deriv(variable: String): Expression
}

case class Number (double: Double) extends Expression{
  override def eval: Double = double

  override def deriv(variable: String): Expression = Number(0)
}
case class Summ (a:Expression, b: Expression) extends Expression{
  override def eval: Double = a.eval + b.eval

  override def deriv(variable: String): Expression = {
    (a,b) match {
      case (arg1,Number(data)) => arg1.deriv(variable)
      case _ => Summ(a.deriv(variable), b.deriv(variable))
    }

  }
}
case class Sub (a:Expression, b: Expression) extends Expression{
  override def eval: Double = a.eval - b.eval

  override def deriv(variable: String): Expression = Sub(a.deriv(variable), b.deriv(variable))
}

case class Prod (a:Expression, b: Expression) extends Expression{
  override def eval: Double = a.eval * b.eval

  override def deriv(variable: String): Expression = {
    (a, b) match {
      case (exp,Number(0.0)) => Number(0.0)
      //case (exp, Number(1.0)) => exp deriv variable
      case (_,_) => Summ (Prod (a deriv  variable , b), Prod (a, b deriv  variable ) )
    }
  }
}

case class Div (a:Expression, b: Expression) extends Expression{
  override def eval: Double = a.eval / b.eval

  override def deriv(variable: String): Expression =
    Div(Sub(Prod(a.deriv(variable),b),Prod(a,b.deriv(variable))),Prod(b,b))
}

case class Id (name: String, data: Double) extends Expression{
  override def eval: Double = data

  override def deriv(variable: String): Expression =
    if (name == variable)
      Id(name,data)
    else
      Number(0)
}
case class Pow (expr: Expression, degree: Double ) extends Expression{
  override def eval: Double = math.pow(expr.eval,degree)

  override def deriv(variable: String): Expression = {
    (expr,degree) match {
      case (Number(e),_) => Number(0)
      case (Id(e, data), 1.0) =>  Number (1.0)
      //case (e, 2.0) => Prod(e,e)
      case (Id(e, data), d) => {
        if (e == variable)
          Prod(Number(degree), Pow(Id(variable, data), degree - 1.0))
        else
          Number(0)
      }
      case _ => Prod(Prod(Number(degree), Pow(expr, degree - 1.0)), expr deriv variable)
    }
  }
}

case class Not(expr: Expression) extends Expression{
  override def eval: Double = -expr.eval

  override def deriv(variable: String): Expression = Not(expr.deriv(variable))
}
case class Sin (expr: Expression) extends Expression{
  override def eval: Double = math.sin(expr eval)

  override def deriv(variable: String): Expression = Prod(Cos(expr),expr deriv variable)
}
case class Cos (expr: Expression) extends Expression{
  override def eval: Double = math.cos(expr eval)

  override def deriv(variable: String): Expression = Prod(Not(Sin(expr)),expr deriv variable)
}
case class Der(exp: Expression,variable: String) extends Expression{
  override def eval: Double = {
    val newAst = exp deriv variable
   // println(newAst)
    newAst.eval
  }

  override def deriv(v: String): Expression = (exp deriv v) deriv variable
}

case class Tan(expr: Expression) extends Expression{
  override def eval: Double = math.tan(expr.eval)

  override def deriv(variable: String): Expression =
    Prod(Div(Number(1.0),Pow(Cos(expr),2)),expr deriv variable)
}

object Creator{
  val map: Map[String, Expression => Expression] = Map(
    "tan" -> ((e: Expression) => Tan(e)),
    "cos" -> ((e: Expression) => Cos(e)),
    "sin" ->((e: Expression) => Sin(e))
  )
}