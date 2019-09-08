import Calculator._

object Main extends App {
  val map: Map[String, Double] = Map("x" -> 3.0, "y" -> 0.0, "z" -> math.Pi)
  val function = Map("sin" -> ((x:Double) => math.sin(x)),
    "cos" -> ((x:Double) => math.cos(x)))
  //val d = 3 / 0
  //println(CalculateDeriv("(x*x + 0) + 1", "x", Map("x" -> 4.0)))
  val ast = Parse("((x/3.0 + 6)`x * 0) + 5", Map("x" -> 7.0))
  val Ast: Expression = ast match {
    case Right(el) => el
    case Left(_) => throw new Error("expected Right")
  }
  val normalAst = Normalize(Ast)

  println(normalAst)

}
