package Calculator

object Main extends App {
  val const = Map("x" -> 0.0, "angle" -> 90.0, "PI" -> math.Pi)
  val function = Map("sin" -> ((x:Double) => math.sin(x)),
    "cos" -> ((x:Double) => math.cos(x)))

  println(Calculate("(1 + 4)*2"))
  println(Calculate("(1 + 4)*2 - 4.0"))
  //println(Calculate("( 1 + x ) * 2 - 4.0", const,function))
  println(Calculate("(-1 + 4) * 2 - 4.0"))
  println(Calculate("-1 + sin(PI/2) * 2 - 4.0"))
  println(Calculate("cos(PI + PI)"))
 // println(AST(" 6 + (x + 4)`"))
  println(Calculate("(tan(x) + 4)` + 6",const))
  println(Calculate("(cos(x))` + (sin(x))`",const))

}