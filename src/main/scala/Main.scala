import Calculator.{MakeByteCode, Parse}
import Interpretator.Execute
import Compiler._
object Main extends App {

  val s = StackFrame()
  s.push("x")
  s.push("0")
  s.push("yyy")
  s.push("5")
  s.push("tyu")
  s.newFrame
  s.push("y")
  s.push("1")

  s.getPos("x")

  val programm: String =
    """
      |y = 10 * 1
      |y = 8
      |x = 4
      |if( 1 + x == 4 + 1){
      |  while ( x != 100){
      |   x = x + 1
      |  }
      |}
      |t = 8
      |if(t == 8){
      |  s = t + y
      |}else
      |{
      |  x = 44
      |}
      |return x
      |""".stripMargin
  println(MakeByteCode(programm))
  println(Compilate(programm))
//  val programm =
//"""
//  |printInt (2 + 4)
//  |""".stripMargin
//  println(MakeByteCode(programm))
}
