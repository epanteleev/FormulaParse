import interpreter.Execute
import org.scalatest.FunSuite

class TestCmpThen extends FunSuite {

  test("Execute: x = 2+3\n if ( x == 0) {\n y = x + 5 return x \n}\n  y = 7\n return (4 + y) \n") {
    assert(Execute("x = 2+3 if ( x == 0) {\n y = x + 5 return x \n}\n  y = 7\n return (4 + y) \n") === 11.0)
  }

  test("Execute: x = 2+3\n if ( x == 0) {\n y = x + 5 return x \n}\n  y = 7 + x\n return (4 + y) \n") {
    assert(Execute("x = 2+3\n if ( x == 0) {\n y = x + 5 return x \n}\n  y = 7 + x\n return (4 + y)\n") === 16.0)
  }

  test("Execute: x = 2 if(x == 2) { x = x + x} return x") {
    assert(Execute("x = 2\n if(x == 2)\n { x = x+ x}\n return x") === 4.0)
  }

  test("Execute: x = 2 if(x == 2) { return 6 } return x") {
    assert(Execute("x = 2 if(x == 2) { return 6 } return x") === 6.0)
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

  trait InternCond2 {
    val programm: String =
      """
        |x = 3 + 4
        |x = x + 6*0
        |if ( x == 7){
        | y = 6
        | if( y != 6){
        |   x = x + y
        | }
        | else{
        |   y = 5
        |   r = 8
        |   t = 0
        | }
        |}
        |return y
        |""".stripMargin
    val res: Double = 5.0
  }

  new InternCond2 {
    test("Execute: InternalCond2") {
      assert(Execute(programm) === res)
    }
  }

  trait InternCond3 {
    val programm: String =
      """
        |x = 3 + 4
        |x = x + 6*0
        |if ( 7 == x){
        | y = 6
        | if( 6 != y){
        |   x = x + y
        | }
        | else{
        |   y = 5
        |   r = 8
        |   t = 0
        | }
        |}
        |return y
        |""".stripMargin
    val res: Double = 5.0
  }

  new InternCond3 {
    test("Execute: InternalCond3") {
      assert(Execute(programm) === res)
    }
  }
}