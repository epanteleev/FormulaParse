package Calculator

object Normalize {
  def apply(expression: Expression):Expression = norm(expression)

  @scala.annotation.tailrec
  private def norm(expression: Expression): Expression = {
    expression match {
      case Summ(left, Number(0.0)) => norm(left)
      case Summ(Number(0.0), right) => norm(right)
      case Sub(left, Number(0.0)) => norm(left)
      case Sub(Number(0.0), right) => norm(right)
      case Prod(_, Number(0.0)) => Number(0.0)
      case Prod(Number(0.0), _) => Number(0.0)
      case Prod(left, Number(1.0)) => norm(left)
      case Prod(Number(1.0), right) => norm(right)
      case others => others // ?
    }
  }

}
