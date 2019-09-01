import Calculator.Parse

object Main extends App {

  //println(Compilator.Compilate("x = 2+3\n return x"))
  val r = Parse("x = 2+3 if ( x == 0) {\n return x \n} return 4 \n")
  println(r)

//  MakeByteCode("x = 2+3\n x") foreach (x => println(x))
//  println(Execute("x = 2\n (x + 3)"))
//  MakeByteCode("(1 + 4)*2 - 9").foreach( x => println(s"$x"))
//  println(Interpret(MakeByteCode("(1 + 4)*2")))
//  println(Interpret(MakeByteCode("(1 + 4)*2 - 9")))
//  println(Interpret(MakeByteCode("9/3")))

}
