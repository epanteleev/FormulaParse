import Interpretator.Execute
import org.scalatest.FunSuite

class TestInterpret extends FunSuite {

  test("Execute: x = 5\n y=5+x\n y ") {
    assert(Execute("x = 5\n y=5+x\n y") === 10)
  }

  test("Execute: x = 5\n y = 6 \n z=y+x\n z ") {
    assert(Execute("x = 5\n y = 6 \n z=y+x\n z") === 11)
  }
  test("Execute: x = 2 + 2\n x ") {
    assert(Execute("x = 2 + 2\n x") === 4)
  }

  test("Execute: x = 4 / 2\n x") {
    assert(Execute("x = 4 / 2\n x") === 2.0)
  }

  test("Execute: x = pow(4, 2)\nx") {
    assert(Execute("x = pow(4, 2) \nx") === 16.0)
  }

  test("Execute: x = pow(4, 2) + 3*6.0\n x") {
    assert(Execute("x = pow(4, 2) + 3*6.0\n x") === 34.0)
  }

  test("Execute: x = pow(4, 2) + (6+3)-3*(6.0+5)\n z=x+2\n z") {
    assert(Execute("x = pow(4, 2) + (6+3)-3*(6.0+5)\n z=x+2\n z") === -6.0)
  }
  test("Execute: x = pow(4, 2) + (6+3)-3*(6.0+5)\n x=x+2\n x") {
    assert(Execute("x = pow(4, 2) + (6+3)-3*(6.0+5)\n x=x+2\n x") === -6.0)
  }

  test("Execute: x = 0\n x = pow(4, 2) + (6+3)-3*(6.0+5)\n x=x+x\n x") {
    assert(Execute(" x = 0\n x = pow(4, 2) + (6+3)-3*(6.0+5)\n x=x+x\n x") === -16.0)
  }
}