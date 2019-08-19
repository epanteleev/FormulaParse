package Calculator

object Main extends App {
  val const = Map("x" -> 4.0, "angle" -> 90.0, "PI" -> math.Pi)
  val function = Map("sin" -> ((x:Double) => math.sin(x)),
    "cos" -> ((x:Double) => math.cos(x)))

  println(Calculator("(1 + 4)*2"))
  println(Calculator("(1 + 4)*2 - 4.0"))
  println(Calculator("( 1 + x ) * 2 - 4.0", const,function))
  println(Calculator("(-1 + 4) * 2 - 4.0"))
  println(Calculator("-1 + sin(PI/2) * 2 - 4.0"))
  println(Calculator("cos(PI + PI)"))
}