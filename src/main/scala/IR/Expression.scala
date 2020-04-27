package IR

import IR.MIR._
import IR.Type.TypeId

sealed trait Expression {
  def foreach(f: Expression => Unit): Unit = f(this)

  def toStringHelper(expList: List[Expression]): String = {
    expList.foldLeft(new String())((s: String, exp: Expression) => s"$s ${exp.toString}")
  }
}

case class Number private(typeId: TypeId) extends Expression {
  override def toString: String = typeId.toString
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

abstract class Assignment(name: String, expr: Expression) extends Expression {

  override def foreach(f: Expression => Unit): Unit = {
    expr.foreach(f)
    f(this)
  }
}

case class ReDef(name: String, expr: Expression) extends Assignment(name, expr){
  override def toString: String = s"$name <- $expr"
}

case class Let(name: String, expr: Expression) extends Assignment(name, expr){
  override def toString: String = s"val $name = $expr"
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
  override def toString: String = {
    val string = if (elseBlock.nonEmpty) {
      s"else {\n${toStringHelper(elseBlock)}\n}"
    } else ""
    s"if ( $condition ) {\n ${toStringHelper(ifBlock)}\n} $string"
  }
}

case class Loop(override val condition: Condition, expr: List[Expression]) extends Flow(condition) {

  override def toString: String = s"while( $condition) {\n${toStringHelper(expr)}\n}\n"
}

case class CallFunction(name: String, opList: List[Expression]) extends Expression {
  override def toString: String = s"$name($opList)"

  override def foreach(f: Expression => Unit): Unit = {
    for (x <- opList) {
      x.foreach(f)
    }
    f(this)
  }
}

case class Function(name: String, argList: List[Expression], body: Expression) extends Expression {
  override def toString: String = s"""def $name($argList) {\n$body\n}"""
}