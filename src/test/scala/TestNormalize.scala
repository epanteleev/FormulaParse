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
    val ast = Parse("(x + 0) * 0", Map("x" -> 4.0))
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


  trait NormalizeTest4 {
    val ast = CalculateDeriv("(x + 0)", "x", Map("x" -> 4.0))
  }

  new NormalizeTest4 {
    test("Ast4 toString") {
      assert(ast.toString === "1.0")
    }
  }


  trait NormalizeTestZeroDiv5 {
    val ast = Parse("(x/0 + a)", Map("x" -> 4.0, "a" -> 3.0))
    val Ast: Expression = ast match {
      case Right(el) => el
      case Left(_) => throw new Error("expected Right")
    }
    val normalAst = Normalize(Ast)
  }

  new NormalizeTestZeroDiv5 {
    test("Ast5 toString") {
      assert(normalAst.toString === "{<x=4.0> / 0.0}")
    }
  }

  trait NormalizeTestZeroDiv6 {
    val ast = Parse("(x/0 + a) * 0", Map("x" -> 4.0, "a" -> 3.0))
    val Ast: Expression = ast match {
      case Right(el) => el
      case Left(_) => throw new Error("expected Right")
    }
    val normalAst = Normalize(Ast)
  }

  new NormalizeTestZeroDiv6 {
    test("Ast6 toString") {
      assert(normalAst.toString === "{<x=4.0> / 0.0}")
    }
  }

  trait NormalizeTestZeroDiv7 {
    val ast = Parse("(x/0 - a) + 5/7 * 56", Map("x" -> 4.0, "a" -> 3.0))
    val Ast: Expression = ast match {
      case Right(el) => el
      case Left(_) => throw new Error("expected Right")
    }
    val normalAst = Normalize(Ast)
  }

  new NormalizeTestZeroDiv7 {
    test("Ast7 toString") {
      assert(normalAst.toString === "{<x=4.0> / 0.0}")
    }
  }

  trait NormalizeTestZeroDiv8 {
    val ast = Parse("(x/0 - a)`x + 5/7 * 56", Map("x" -> 4.0, "a" -> 3.0))
    val Ast: Expression = ast match {
      case Right(el) => el
      case Left(_) => throw new Error("expected Right")
    }
    val normalAst = Normalize(Ast)
  }

  new NormalizeTestZeroDiv8 {
    test("Ast8 toString") {
      assert(normalAst.toString === "{<x=4.0> / 0.0}")
    }
  }

  trait NormalizeTestZeroDiv9 {
    val ast = Parse("(x/0/5)`x + 5/7 * 56", Map("x" -> 4.0, "a" -> 3.0))
    val Ast: Expression = ast match {
      case Right(el) => el
      case Left(_) => throw new Error("expected Right")
    }
    val normalAst = Normalize(Ast)
  }

  new NormalizeTestZeroDiv9 {
    test("Ast9 toString") {
      assert(normalAst.toString === "{<x=4.0> / 0.0}")
    }
  }

  trait NormalizeTestZeroDiv10 {
    val ast = Parse("(x/0/5)`x + 5/7 * 56 + a/0", Map("x" -> 4.0, "a" -> 3.0))
    val Ast: Expression = ast match {
      case Right(el) => el
      case Left(_) => throw new Error("expected Right")
    }
    val normalAst = Normalize(Ast)
  }

  new NormalizeTestZeroDiv10 {
    test("Ast10 toString") {
      assert(normalAst.toString === "{<x=4.0> / 0.0}")
    }
  }

  trait NormalizeTestZeroDiv11 {
    val ast = Parse("0/0")
    val Ast: Expression = ast match {
      case Right(el) => el
      case Left(_) => throw new Error("expected Right")
    }
    val normalAst = Normalize(Ast)
  }

  new NormalizeTestZeroDiv11 {
    test("Normalize: 0/0 is ZeroDiv") {
      assert(normalAst.toString === "{0.0 / 0.0}")
    }
  }

  trait NormalizeTest12 {
    val ast = Parse("0/3.0")
    val Ast: Expression = ast match {
      case Right(el) => el
      case Left(_) => throw new Error("expected Right")
    }
    val normalAst = Normalize(Ast)
  }

  new NormalizeTest12 {
    test("Normalize: 0/3.0 is 0.0") {
      assert(normalAst.toString === "0.0")
    }
  }

  trait NormalizeTestNotUsed13 {
    val ast = Parse("(x/3.0) * 0.0", Map("x" -> 7.0))
    val Ast: Expression = ast match {
      case Right(el) => el
      case Left(_) => throw new Error("expected Right")
    }
    val normalAst = Normalize(Ast)
  }

  new NormalizeTestNotUsed13 {
    test("Normalize: (x/3.0) * 0.0") {
      assert(normalAst.toString === "<x=7.0> / 3.0 * 0.0")
    }
  }


  trait NormalizeTestNotUsed14 {
    val ast = Parse("(x/3.0 + 6) * 0", Map("x" -> 7.0))
    val Ast: Expression = ast match {
      case Right(el) => el
      case Left(_) => throw new Error("expected Right")
    }
    val normalAst = Normalize(Ast)
  }

  new NormalizeTestNotUsed14 {
    test("Normalize: 0 * (x/3.0 + 6)") {
      assert(normalAst.toString === "<x=7.0> / 3.0 * 0.0")
    }
  }

  trait NormalizeTestNotUsed15 {
    val ast = Parse("(x/3.0 + 6 - (6*7)) * 0", Map("x" -> 7.0))
    val Ast: Expression = ast match {
      case Right(el) => el
      case Left(_) => throw new Error("expected Right")
    }
    val normalAst = Normalize(Ast)
  }

  new NormalizeTestNotUsed15 {
    test("Normalize: (x/3.0 + 6 - (6*7)) * 0") {
      assert(normalAst.toString === "<x=7.0> / 3.0 * 0.0")
    }
  }

  trait NormalizeTestNotUsed16 {
    val ast = Parse("(x/3.0 + 6 - (6/ x)) * 0", Map("x" -> 7.0))
    val Ast: Expression = ast match {
      case Right(el) => el
      case Left(_) => throw new Error("expected Right")
    }
    val normalAst = Normalize(Ast)
  }

  new NormalizeTestNotUsed16 {
    test("Normalize: (x/3.0 + 6 - (6/ x)) * 0") {
      assert(normalAst.toString === "(<x=7.0> / 3.0 - 6.0 / <x=7.0>) * 0.0") // fail
    }
  }

  trait NormalizeTestNotUsed17 {
    val ast = Parse("(x/3.0 + 6 - (6/0)) * 0", Map("x" -> 7.0))
    val Ast: Expression = ast match {
      case Right(el) => el
      case Left(_) => throw new Error("expected Right")
    }
    val normalAst = Normalize(Ast)
  }

  new NormalizeTestNotUsed17 {
    test("Normalize: (<x=7.0> / 3.0 + 6.0 / <x=7.0>) * 0.0") {
      assert(normalAst.toString === "{6.0 / 0.0}") // fail
    }
  }

  trait NormalizeTestNotUsed18 {
    val ast = Parse("(x/3.0 + 6 - (6/0))`x * 0", Map("x" -> 7.0))
    val Ast: Expression = ast match {
      case Right(el) => el
      case Left(_) => throw new Error("expected Right")
    }
    val normalAst = Normalize(Ast)
  }

  new NormalizeTestNotUsed18 {
    test("Normalize: (x/3.0 + 6 - (6/0))`x * 0") {
      assert(normalAst.toString === "{6.0 / 0.0}") // fail
    }
  }


  trait NormalizeTestNotUsed19 {
    val ast = Parse("((-x/3.0 + 6)`x * 0) + (-x + 5) * 0", Map("x" -> 7.0))
    val Ast: Expression = ast match {
      case Right(el) => el
      case Left(_) => throw new Error("expected Right")
    }
    val normalAst = Normalize(Ast)
  }

  new NormalizeTestNotUsed19 {
    test("Normalize: ((-x/3.0 + 6)`x * 0) + (-x + 5) * 0") {
      assert(normalAst.toString === "<x=7.0> / 3.0 * 0.0") // fail
    }
  }

}