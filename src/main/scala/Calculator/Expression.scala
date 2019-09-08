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

class BinaryOperator (a:Expression, b: Expression) extends Expression {
  override def eval: Double = throw new Error("rr")

  override def derivative(variable: String): Expression = throw new Error("rr")

  protected def swapSubTree: Option[(Expression, Expression)] = {
    a match {
      case ZeroDivision(_) => Some((b, a))
      case Number(_) => Some((b, a))
      case _ => Some((a, b))
    }
  }
}

class Summ private (val a:Expression, val b: Expression) extends BinaryOperator(a, b) {
  override def eval: Double = a.eval + b.eval

  override def derivative(variable: String): Expression =
      Summ(a derivative variable, b derivative variable)

  override def toString: String = s"($a + $b)"
}

object Summ {
  def apply(a: Expression, b: Expression): Summ = new Summ(a, b)
  def unapply(arg: Summ): Option[(Expression, Expression)] = arg.swapSubTree
}

class Sub private (val a:Expression, val b: Expression) extends BinaryOperator(a, b) {
  override def eval: Double = a.eval - b.eval

  override def derivative(variable: String): Expression =
     Sub(a derivative variable, b derivative variable )

  override def toString: String = s"($a - $b)"
}

object Sub {
  def apply(a: Expression, b: Expression): Sub = new Sub(a, b)
  def unapply(arg: Sub): Option[(Expression, Expression)] = arg.swapSubTree
}

class Prod private (val a:Expression, val b: Expression) extends BinaryOperator(a, b) {
  override def eval: Double = a.eval * b.eval

  override def derivative(variable: String): Expression =
     Summ(Prod(a derivative variable, b), Prod (a, b derivative variable))

  override def toString: String = s"$a * $b"
}

object Prod {
  def apply(a: Expression, b: Expression): Prod = new Prod(a, b)
  def unapply(arg: Prod): Option[(Expression, Expression)] = arg.swapSubTree
}

class Div private (val a:Expression, val b: Expression) extends BinaryOperator(a, b) {
  override def eval: Double = a.eval / b.eval

  override def derivative(variable: String): Expression =
    Div(Sub(Prod(a derivative variable, b),Prod(a, b derivative variable)),Prod(b, b))

  override def toString: String = s"$a / $b"
}

object Div {
  def apply(a: Expression, b: Expression): Div = new Div(a, b)
  def unapply(arg: Div): Option[(Expression, Expression)] = Some((arg.a, arg.b))
}

class Constant private (val name: String, val data: Double) extends Expression {
  override def eval: Double = data

  override def derivative(variable: String): Expression =
    if (name == variable) Number(1.0) else Number(0)

  override def toString: String = s"<$name=$data>"
}

object Constant{
  def apply(name: String, data: Double): Constant = new Constant(name, data)
  def unapply(arg: Constant): Option[(String, Double)] = Some((arg.name,arg.data))
}

class Pow private (val expr: Expression, val degree: Double) extends Expression {
  override def eval: Double = math.pow(expr eval,degree)

  override def derivative(variable: String): Expression =
    Prod(Prod(Number(degree), Pow(expr, degree - 1.0)), expr derivative variable)


  override def toString: String = s"($expr ^ $degree)"
}

object Pow {
  def apply(expr: Expression, degree: Double): Pow = new Pow(expr, degree)
  def unapply(arg: Pow): Option[(Expression, Double)] = Some((arg.expr, arg.degree))
}

class Neg private (val expr: Expression) extends Expression{
  override def eval: Double = -expr.eval

  override def derivative(variable: String): Expression = Neg(expr derivative variable)

  override def toString: String = s"-($expr)"
}

object Neg {
  def apply(expr: Expression): Neg = new Neg(expr)
  def unapply(arg: Neg): Option[Expression] = Some(arg.expr)
}

class Sin private (val expr: Expression) extends Expression{
  override def eval: Double = math.sin(expr eval)

  override def derivative(variable: String): Expression = Prod(Cos(expr),expr derivative variable)

  override def toString: String = s"sin($expr)"
}

object Sin {
  def apply(expr: Expression): Sin = new Sin(expr)
  def unapply(arg: Sin): Option[Expression] = Some(arg.expr)
}

case class Cos(expr: Expression) extends Expression{
  override def eval: Double = math.cos(expr eval)

  override def toString: String = s"cos($expr)"

  override def derivative(variable: String): Expression =
    Prod(Neg(Sin(expr)),expr derivative variable)
}

case class Tan (expr: Expression) extends Expression{
  override def eval: Double = math.tan(expr.eval)

  override def derivative(variable: String): Expression =
    Prod(Div(Number(1.0),Pow(Cos(expr),2)), expr derivative variable)

  override def toString: String = s"tan($expr)"
}

class Der private (val exp: Expression, val variable: String) extends Expression{
  override def eval: Double = (exp derivative variable) eval

  override def derivative(v: String): Expression = (exp derivative variable) derivative v

  override def toString: String = s"($exp)`$variable"
}

object Der{
  def apply(exp: Expression, variable: String): Der = new Der(exp, variable)

  def unapply(arg: Der): Option[(Expression, String)] = Some((arg.exp, arg.variable))
}

case class ZeroDivision(expr: Expression) extends Expression {
  override def eval: Double = throw new ArithmeticException("zero division")

  override def derivative(variable: String): Expression = throw new ArithmeticException("zero division")

  override def toString: String = s"{$expr / 0.0}"
}