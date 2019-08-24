import org.scalatest.FunSuite

class TestCalculate extends FunSuite {

  import Calculator._
  test("Calculate: 2 + 2 = 4") {
    assert(Calculate("2 + 2") === 4)
  }

  test("Calculate: (2 + 2)*4 = 16") {
    assert(Calculate("(2 + 2)*4") === 16)
  }

  test("Calculate: sin(PI/2) + 1 = 2") {
    assert(Calculate("sin(PI/2) + 1") === 2)
  }

  test("Calculate: cos(PI/2) + 1 = 1") {
    assert(Calculate("cos(PI/2) + 1") === 1)
  }

  test("Calculate: tan(PI/4) + 1 = 2") {
    assert(Calculate("tan(PI/4) + 1") === 2)
  }

  test("Calculate: pow(3,4) = 81") {
    assert(Calculate("pow(3.0 , 4)") === 81)
  }

  trait TestCase{
    val map: Map[String, Double] = Map("x" -> 3.0, "y" -> 0.0, "z" -> math.Pi)
  }

  new TestCase {
    test("Calculate:  x = 3;\n (pow(x,4))` = 101") {
      assert(Calculate("(pow(x,4))`",map) === 108)
    }

    test("Calculate: (x)` = 1.0") {
      assert(Calculate("(x)`",map) === 1.0)
    }

    test("Calculate: ((x)`)` -> Exception ") {
      intercept[Error] {
        Calculate("((x)`)`",map)
      }
    }
  }
}