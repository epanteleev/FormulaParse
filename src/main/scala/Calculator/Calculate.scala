package Calculator

object Calculate{
  private lazy val defaultConst: Map[String, Double] = Map("PI" -> math.Pi, "e" -> math.E)

  def apply(str: String): Double = apply(str, defaultConst)

  def apply(str: String, const: Map[String, Double] ) : Double = Parse(str,const) match {
    case Right(result) => Normalize(result) eval
    case Left(error) => throw new Error(error toString)
  }

  def apply(exp: Expression): Double = exp.eval
}
