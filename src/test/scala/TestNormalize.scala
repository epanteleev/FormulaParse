import org.scalatest.FunSuite

class TestNormalize extends FunSuite {

  import Calculator._

  trait NormalizeTest1 {
    val ast = Parse("(x + 4) + 0", Map("x" -> 4.0))
    val Ast: Expression = ast match {
      case Right(el) => el
      case Left(_) => throw new Error("expected Right")
    }
    val normalAst = Normalize(Ast)
  }

  new NormalizeTest1 {
    test("Ast1 toString") {
      assert(normalAst.toString === "(<x=4.0> + 4.0)")
    }
  }

  trait NormalizeTest2 {
    val ast = Parse("(x + 0) + 0", Map("x" -> 4.0))
    val Ast: Expression = ast match {
      case Right(el) => el
      case Left(_) => throw new Error("expected Right")
    }
    val normalAst = Normalize(Ast)
  }

  new NormalizeTest2 {
    test("Ast2 toString") {
      assert(normalAst.toString === "<x=4.0>")
    }
  }

  trait NormalizeTest3 {
    val ast = Parse("(x + 0) *0", Map("x" -> 4.0))
    val Ast: Expression = ast match {
      case Right(el) => el
      case Left(_) => throw new Error("expected Right")
    }
    val normalAst = Normalize(Ast)
  }

  new NormalizeTest3 {
    test("Ast3 toString") {
      assert(normalAst.toString === "0.0")
    }
  }

}