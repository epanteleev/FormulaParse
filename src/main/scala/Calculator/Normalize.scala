package Calculator

import org.graalvm.compiler.lir.Variable

private object isZero{
  private val eps: Double = 5.96e-08
  def apply(d: Double): Boolean = math.abs(d - 0.0D) < eps
}

object Normalize {
  def apply(expression: Expression):Expression = norm(expression)

  private def norm (expression: Expression): Expression = {
    expression match {
      case Summ(right, left) => normSumm(right, left)
      case Sub(left, right) => normSub(left, right)
      case Prod(left, right) => normProd(left, right)
      case Div(left, right) => normDiv(left, right)
      case Der(expr, v) => normDer(expr, v)
      case Neg(e) => norm(e)
      case others => others
    }
  }

  private def normDer (exp: Expression, variable: String): Expression = {
    val n = norm(exp)
    n match{
      case ZeroDivision(_) => n
      case _ => Der(exp, variable)
    }
  }

  private def normProd (left: Expression, right: Expression): Expression = {
    val r = norm(right)
    val l = norm(left)
    (l, r) match {
      case (ZeroDivision(_), _) => l
      case (_, ZeroDivision(_)) => r
      case (_, Number(0.0)) => normNotUsed(l, r)
      case (Number(0.0), _) => normNotUsed(r, l)
      case _ => Prod(l, r)
    }
  }

  def normNotUsed(l:Expression, r: Expression): Expression ={

    def internalNorm (builder: (Expression, Expression) => Expression,
                      left: Expression,
                      right: Expression): Option[Expression] = {
      val l = iterNormNotUsed(left)
      val r = iterNormNotUsed(right)
      (l, r) match {
        case (Some(res1), Some(res2)) => Some(builder(res1, res2))
        case (_, Some(_)) => r
        case (Some(_), _) => l
        case (None, None) => r
      }
    }
    def iterNormNotUsed(expr: Expression): Option[Expression] = {
      expr match {
        case Summ(left, right) => internalNorm(Summ.apply, left, right)
        case Sub(left, right) => internalNorm(Sub.apply, left, right)
        case Prod(left, right) => internalNorm(Prod.apply, left, right)
        case Div(_) => Some(expr)
        case Der(e, v) => {
          val l = iterNormNotUsed(e)
          l match {
            case Some(_) => l
            case None => l
          }
        }
        case Neg(e) => {
          val l = iterNormNotUsed(e)
          l match {
            case Some(_) => l
            case None => l
          }
        }
        case _ => None
      }
    }

    val n = iterNormNotUsed(l)
    n match {
      case Some(e) => Prod(e, r)
      case None => Number(0.0)
    }
  }

  private def normSumm (left: Expression, right: Expression): Expression = {
    val r = norm(right)
    val l = norm(left)
    (l, r) match {
      case (ZeroDivision(_), _) => l
      case (_, ZeroDivision(_)) => r
      case (_, Number(0.0)) => l
      case (Number(0.0), _) => r
      case (Number(a), Number(b)) => Number(a + b)
      case  _ => Summ(l, r)
    }
  }

  private def normSub(left: Expression, right: Expression): Expression = {
    val r = norm(right)
    val l = norm(left)
    (l, r) match {
      case (ZeroDivision(_), _) => l
      case (_, ZeroDivision(_)) => r
      case (_, Number(0.0)) => l
      case (Number(0.0), _) => r
      case (Number(a), Number(b)) => Number(a - b)
      case  _ => Sub(l, r)
    }
  }

  private def normDiv(left: Expression, right: Expression): Expression = {
    val r = norm(right)
    val l = norm(left)
    (l, r) match {
      case (ZeroDivision(_), _) => l
      case (_, ZeroDivision(_)) => r
      case (a, Number(0.0)) => ZeroDivision(a)
      case (Number(0.0), Number(a)) =>
        if(isZero(a))
          ZeroDivision(Div(l, r))
        else Number(0.0)
      case _ => Div(left, right)
    }
  }
}
