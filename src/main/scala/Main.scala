import Calculator.{MakeByteCode, Parse}
import Interpretator.Execute
import Compiler._
object Main extends App {

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
  val programm2 =
"""
  |i = readInt()
  |printInt (i + 4)
  |return 0
  |""".stripMargin
  println(MakeByteCode(programm2))
  println(Compilate(programm2))
}
