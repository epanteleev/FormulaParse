import org.scalatest.FunSuite

// fix it
class TestFrame extends FunSuite {

  import Compiler.StackFrame

  trait Test1 {
    val s = StackFrame()
    s.push("x")
    s.push("0")
    s.push("yyy")
    s.push("5")
    s.push("tyu")
  }
  new Test1 {
    test("StackFrame: Test1") {
      assert(s.contains("tui") === false)
      assert(s.align === 20)
      assert(s.contains("x"))
      assert(s.contains("0"))
      assert(s.getPos("x") === 4)
      assert(s.getPos("tyu") === 20)
    }
  }

  trait Test2 {
    val s = StackFrame()
    s.push("x")
    s.push("0")
    s.push("yyy")
    s.push("5")
    s.push("tyu")
    s.newFrame
    s.push("y")
    s.push("1")
  }

  new Test2 {
    test("StackFrame: Test2") {
      assert(s.getPos("x") === 4)
      assert(s.getPos("y") === 24)
      assert(s.align === 8)

    }
  }

  trait Test3 {
    val s = StackFrame()
    s.push("x")
    s.push("0")
    s.push("yyy")
    s.push("5")
    s.push("tyu")
    s.newFrame
    s.push("y")
    s.push("1")
    s.pop
    s.push("z")
    s.push("30")
    s.push("qqq")
  }
  new Test3 {
    test("StackFrame: Test3") {
      assert(s.getPos("x") === 4)
      assert(s.getPos("z") === 24)
      assert(s.align === 32)

    }
  }
}