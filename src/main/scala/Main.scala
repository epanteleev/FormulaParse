import Calculator.{MakeByteCode, Parse}
import Interpretator.Execute

object Main extends App {
  ///(Compilator.Compilate("x = 2 + 4\n x"))
  val loop =
    """
      |x = 0
      |while ( x != 3){
      | x = x + 1
      |}
      |x
      |""".stripMargin
  MakeByteCode(loop).foreach( x => println(x))
  println(Execute(loop))
  val programm =
    """
      |x = 3 + 4
      |x = x + 6*0
      |if ( x == 7){
      | y = 6
      | if( y != 6){
      |   x = x + y
      | }
      |}
      |return x
      |""".stripMargin

  println(Execute(programm))  // res: 7
}
