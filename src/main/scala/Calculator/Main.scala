package Calculator

object Main extends App {
  val map: Map[String, Double] = Map("x" -> 3.0, "y" -> 0.0, "z" -> math.Pi)
  val function = Map("sin" -> ((x:Double) => math.sin(x)),
    "cos" -> ((x:Double) => math.cos(x)))

//  println(Calculate("(1 + 4)*2"))
//  println(Calculate("(1 + 4)*2 - 4.0"))
//  //println(Calculate("( 1 + x ) * 2 - 4.0", const,function))
//  println(Calculate("(-1 + 4) * 2 - 4.0"))
//  println(Calculate("-1 + sin(PI/2) * 2 - 4.0"))
//  println(Calculate("cos(PI + PI)"))
// // println(AST(" 6 + (x + 4)`"))
//  //println(Calculate("(tan(x) + 4)` + 6",const))
//  println(Calculate("(cos(x))`x + (sin(x))`x",const))
//  //println(Calculate("log(2, 4)",const))
//  println("\n")
//  println(Calculate("(cos(x))`x",const))
//  println("\n")
//  println(Calculate("((cos(x))`x)`x",const))
 // println("\n")
  //println(Calculate("((cos(y))`y)`x", map))

  println(Calculate("((x)`x + 1)`x",map))

}