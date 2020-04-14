package IR

import IR.MIR._

sealed trait Expression {
  def foreach(f: Expression => Unit ): Unit = f(this)
}

case class Number private(double: Double, typeid: TypeKind) extends Expression {
  override def toString: String = double toString
}

abstract class BinaryNode(val a: Expression, val b: Expression) extends Expression {
  override def foreach(f: Expression => Unit): Unit = {
    a.foreach(f)
    b.foreach(f)
    f(this)
  }
}

case class Sum(override val a: Expression, override val b: Expression) extends BinaryNode(a, b) {
  override def toString: String = s"($a + $b)"
}

case class Sub(override val a: Expression, override val b: Expression) extends BinaryNode(a, b) {
  override def toString: String = s"($a - $b)"
}

case class Prod(override val a: Expression, override val b: Expression) extends BinaryNode(a, b) {
  override def toString: String = s"$a * $b"
}

case class Div(override val a:Expression, override val b: Expression) extends BinaryNode(a, b) {
  override def toString: String = s"$a / $b"
}

case class Variable(name: String) extends Expression {
  override def toString: String = s"<$name>"
}

case class Not(expr: Expression) extends Expression {

  override def foreach(f: Expression => Unit): Unit = {
    f(expr)
    f(this)
  }

  override def toString: String = s"-$expr"
}

case class Assignment(name: String, expr: Expression) extends Expression {

  override def toString: String = s"$name = $expr"

  override def foreach(f: Expression => Unit): Unit = {
    expr.foreach(f)
    f(this)
  }
}

case class Condition(left: Expression,op: CmpOp, right: Expression) extends Expression {
  override def toString: String = s"$left $op $right"
}

case class Return(exp: Expression) extends Expression {
  override def toString: String = s"return $exp"

  override def foreach(f: Expression => Unit): Unit = {
    exp.foreach(f)
    f(this)
  }
}


abstract class Flow(val condition: Condition) extends Expression

case class IfThen(override val condition: Condition, ifBlock: List[Expression], elseBlock: List[Expression]) extends Flow(condition) {
  override def toString: String = s"if ( $condition ) {\n $ifBlock\n} else {\n$elseBlock\n}"
}

case class Loop(override val condition: Condition, expr: List[Expression]) extends Flow(condition) {

  override def toString: String = s"while( $condition) {\n$expr\n}\n"

  //override def toByteCode(cfg: Graph): Graph = ???

  //{
  //    val Block = gen(expr)
  //    val c = condition.toByteCode
  //    val res = c.tail
  //    c.head match {
  //      case Cmp(op, l1) =>  {
  //        val l0 = NameGen()
  //        List(Goto(l0), Label(l1), Begin()) ++ Block ++ List(End(), Label(l0)) ++ res ++ List(Cmp(op, l1))
  //      }
  //      case others => throw new Error(s"expected If, found $others")
  //    }
  //}
}

case class CallFunction(name: String, opList: List[Expression]) extends Expression {
  override def toString: String = s"$name($opList)"

  override def foreach(f: Expression => Unit): Unit = {
    for (x <- opList) {
      x.foreach(f)
    }
    f(this)
  }

  //override def toByteCode(cfg: Graph): Graph = ???

  //  {
  //    val args = gen(opList)
  //    args ++ List(Call(name, baseBlock))
  //  }
}

case class Function(name: String, argList: List[Expression], body: Expression) extends Expression {
  override def toString: String = s"""def $name($argList) {\n$body\n}"""
}