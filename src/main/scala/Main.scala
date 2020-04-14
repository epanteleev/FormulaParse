import IR.{ConvertToMIR, Parse}
import interpreter.Execute

object Main extends App {

//    val programm: String =
//      """
//        |y = 10 * 1
//        |y = 8
//        |x = 4
//        |if( 1 + x == 4 + 1){
//        |  while ( x != 100){
//        |   x = x + 1
//        |  }
//        |}
//        |t = 8
//        |if(t == 8){
//        |  s = t + y
//        |}else
//        |{
//        |  x = 44
//        |}
//        |return x
//        |""".stripMargin
    //println(MakeByteCode(Parse(programm)))
    //println(Compilate(programm))
    val programm2 =
    """
      |x = 4
      |if(x != 0) {
      | x = 3
      |}
      |return x
      |
    """.stripMargin
    //val a = ConvertToMIR(Parse(programm2))
    //println(a)
    println(Execute(programm2))
    //println(Compilate(programm2))
    //println(Execute("2 + 3"))

}