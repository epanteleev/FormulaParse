package Calculator

object Normalize {
  def apply(expression: Expression):Expression = norm(expression)

  @scala.annotation.tailrec
  private def norm(expression: Expression): Expression = {
    expression match {
      case Summ(left, right) => normSubSumm(left, right)
      case Sub(left, right) => normSubSumm(left, right)
      case Prod(_, Number(0.0)) => Number(0.0)
      case Prod(Number(0.0), _) => Number(0.0)
      case Prod(left, Number(1.0)) => norm(left)
      case Prod(Number(1.0), right) => norm(right)
      case others => others // ?
    }
  }

  private def normSubSumm(left: Expression, right: Expression): Expression ={
    (left, right) match {
      case (Number(0.0), right) => norm(right)
      case (left, Number(0.0)) => norm(left)
      case  _ => Summ(left, right)
    }
  }
}
