import interpreter.Interpret
import org.scalatest.FunSuite

class TestExpression extends FunSuite {

  test("Execute: val x = 5\n val y=5+x\n y ") {
    assert(Interpret("val x = 5\n val y=5+x\n y") === 10)
  }

  test("Execute: val x = 5\n val y = 6 \n val z=y+x\n z ") {
    assert(Interpret("val x = 5\n val y = 6 \n val z=y+x\n z") === 11)
  }
  test("Execute: val x = 2 + 2\n x ") {
    assert(Interpret("val x = 2 + 2\n x") === 4)
  }

  test("Execute: val x = 4 / 2\n x") {
    assert(Interpret("val x = 4 / 2\n x") === 2.0)
  }

  test("Execute: val x = pow(4, 2)\nx") {
    assert(Interpret("val x = pow(4, 2) \nx") === 16.0)
  }

  test("Execute: val x = pow(4, 2) + 3*6.0\n x") {
    assert(Interpret("val x = pow(4, 2) + 3*6.0\n x") === 34.0)
  }

  test("Execute: x = pow(4, 2) + (6+3)-3*(6.0+5)\n z=x+2\n z") {
    assert(Interpret("val x = pow(4, 2) + (6+3)-3*(6.0+5)\n val z=x+2\n z") === -6.0)
  }
  test("Execute: val x = pow(4, 2) + (6+3)-3*(6.0+5)\n val x=x+2\n x") {
    assert(Interpret("val x = pow(4, 2) + (6+3)-3*(6.0+5)\n val x=x+2\n x") === -6.0)
  }

  test("Execute: val y = 0\n val x = pow(4, 2) + (6+3)-3*(6.0+5)\n\n x") {
    assert(Interpret(" val y = 0\n val x = pow(4, 2) + (6+3)-3*(6.0+5)\n\n x") === -8.0)
  }
}