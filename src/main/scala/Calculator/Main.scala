package Calculator

object Main extends App {
  println(Calculator("(1 + 4)*2"))
  println(Calculator("(1 + 4)*2 - 4.0"))
  println(Calculator("( 1 + 4 ) * 2 - 4.0"))
  println(Calculator("(-1 + 4 ) * 2 - 4.0"))
  val map = Map( "sin" -> ((x : Double) => math.sin(x)))
}