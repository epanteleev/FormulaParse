import org.scalatest.FunSuite

class TestFail extends FunSuite {

  import Calculator._

  test("Fail: 3++4") {
    intercept[Error] {
      Calculate("3++4")
    }
  }

  test("Fail: (3+4") {
    intercept[Error] {
      Calculate("(3+4(")
    }
  }

  test("Fail: (3+4(") {
    intercept[Error] {
      Calculate("(3+4(")
    }
  }

  test("Fail: pow(9, )") {
    intercept[NoSuchElementException] {
      Calculate("pow(9, )")
    }
  }

  test("Fail: sin()") {
    intercept[NoSuchElementException] {
      Calculate("sin()")
    }
  }

  test("Fail: sin") {
    intercept[NoSuchElementException] {
      Calculate("sin")
    }
  }

  test("Fail: (sin(2))`") {
    intercept[Error] {
      Calculate("(sin(2))`")
    }
  }
}
