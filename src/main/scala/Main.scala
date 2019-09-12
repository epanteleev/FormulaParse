import Calculator.{MakeByteCode, Parse}
import Interpretator.Execute
import Compilator._
object Main extends App {


//  val programm: String =
//    """
//      |x = 0
//      |while( x < 8){
//      | if(x == 0){
//      |  x = 1
//      | }else{
//      |  x = x + 5
//      | }
//      |}
//      |return x
//      |""".stripMargin

  val programm: String =
    """
      |y = 10 * 1
      |y = 8
      |x = 4
      |if( x == 4){
      |  while ( x != 100){
      |   x = x + 1
      |  }
      |}
      |t = 8
      |if(t == 9){
      | x = t + y
      |}else
      |{
      | x = 44
      |}
      |return x
      |""".stripMargin
  println(MakeByteCode(programm))
  println(Compilate(programm))
}
