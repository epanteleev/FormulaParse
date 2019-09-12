import Interpretator.Execute
import org.scalatest.FunSuite

class TestLoop extends FunSuite {

  import Calculator._

  test("i = 0 \n while( i != 9){ i = i + 3} return i") {
    assert(Execute("i = 0 \n while( i != 9){ i = i + 3} return i") === 9)
  }

  trait loop1{
    val programm: String =
      """
        |x = 0
        |y = 0
        |while ( x == 0){
        | y = y + 1
        | if( y != 6){
        |   x = 7
        | }
        |}
        |return x
        |""".stripMargin
    val res: Double = 7.0
  }

  new loop1 {
    test("Execute: InternalCond") {
      assert(Execute(programm) === res)
    }
  }

  trait loop2{
    val programm: String =
      """
        |x = 0
        |while ( x != 10000){
        | x = x + 1
        |}
        |return x
        |""".stripMargin
    val res: Double = 10000
  }

  new loop2 {
    test("Execute: loop2") {
      assert(Execute(programm) === res)
    }
  }

  trait loop3{
    val programm: String =
      """
        |x = 0
        |while ( x < 10000){
        | if( x == 10 ){
        |  x = x * 10
        | }
        | x = x + 1
        |}
        |return x
        |""".stripMargin
    val res: Double = 10000
  }

  new loop3 {
    test("Execute: loop3") {
      assert(Execute(programm) === res)
    }
  }
}