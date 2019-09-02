import Interpretator.Execute
import org.scalatest.FunSuite

class TestIfThen extends FunSuite {

  import Calculator._

  test("Execute: x = 2+3\n if ( x == 0) {\n y = x + 5 return x \n}\n  y = 7\n return (4 + y) \n") {
    assert(Execute("x = 2+3 if ( x == 0) {\n y = x + 5 return x \n}\n  y = 7\n return (4 + y) \n") === 11.0)
  }

  test("Execute: x = 2+3\n if ( x == 0) {\n y = x + 5 return x \n}\n  y = 7 + x\n return (4 + y) \n") {
    assert(Execute("x = 2+3\n if ( x == 0) {\n y = x + 5 return x \n}\n  y = 7 + x\n return (4 + y)\n") === 16.0)
  }

  test("Execute: x = 2 if(x == 2) { x = x + x} return x") {
    assert(Execute("x = 2\n if(x == 2)\n { x = x+ x}\n return x") === 4.0)
  }


  trait InternCond1{
    val programm: String =
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
    val res: Double = 7.0
  }

  new InternCond1 {
    test("Execute: InternalCond") {
      assert(Execute(programm) === res)
    }
  }
}