import org.scalatest.FunSuite

class TestCalculate extends FunSuite {

  import Calculator._
  test("Calculate: 2 + 2 = 4") {
    assert(Calculate("2 + 2") === 4)
  }

  test("Calculate: 2 - 2 + 4 = 4") {
    assert(Calculate("2 - 2 + 4") === 4)
  }

  test("Calculate: (2 + 2)*4 = 16") {
    assert(Calculate("(2 + 2) * 4") === 16)
  }

  test("Calculate: sin(PI/2) + 1 = 2") {
    assert(Calculate("sin(PI / 2) + 1") === 2)
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

    test("Calculate: cos(-z) = -1 ") {
      assert(Calculate("cos(-z)",map) ===  -1)
    }

    test("Calculate: cos(-(x - 3.0000 )) ") {
      assert(Calculate("cos(-(x - 3.0000 ))",map) ===  1)
    }

    test("Calculate: ((cos(y))`y)`x = 0 ") {
      assert(Calculate("((cos(y))`x)`y",map) ===  0)
    }
    test("Calculate:  x = 3;\n x * 1 = 3") {
      assert(Calculate("x * 1",map) === 3)
    }

    test("Calculate:  x = 3;\n (pow(x,4))`x = 101") {
      assert(Calculate("(pow(x,4))`x",map) === 108)
    }

    test("Calculate: (x)`x = 1.0") {
      assert(Calculate("(x)`x",map) === 1.0)
    }

    test("Calculate: ((x)`x + 1)`x = 0 ") {
        assert(Calculate("((x)`x + 1)`x",map) === 0)
    }
    test("Calculate: ((x)`x)`x = 0 ") {
      assert(Calculate("((x)`x)`x", map) === 0)
    }
    test("Calculate: ((cos(y))`y)`y = -1 ") {
            assert(Calculate("((cos(y))`y)`y",map) ===  -1)
    }

    test("Calculate: (cos(y))`y + (sin(z))`z = -1 ") {
      assert(Calculate(" (cos(y))`y + (sin(z))`z",map) ===  -1)
    }

    test("Calculate: (tan(y) + y)`y + 1 ") {
      assert(Calculate("(tan(y) * y)`y + 1",map) ===  1)
    }
    test("Calculate: (((pow(x,2))`x)`x)`x ") {
      assert(Calculate("(((pow(x,2))`x)`x)`x",map) ===  0)
    }

    test("Calculate: (cos(y))`x = 0 ") {
      assert(Calculate("(cos(y))`x",map) ===  0)
    }

    test("Calculate: ((cos(y))`y)'z + ((sin(z))`z)`y = 0 ") {
      assert(Calculate(" ((cos(y))`y)`z - ((sin(z))`z)`y",map) ===  0)
    }

  }
}