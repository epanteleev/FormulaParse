package Calculator

sealed trait Expression{
  def toByteCode: List[CodeOp]


  def gen(result: List[Expression], list: List[CodeOp] = List()):List[CodeOp] = {
    result match {
      case head :: l => gen(result.tail, list ++ head.toByteCode)
      case Nil => list
    }
  }
}


case class Number(double: Double) extends Expression{

  override def toByteCode: List[CodeOp] = List(Push(double))


  override def toString: String = double toString
}


case class Summ(a:Expression, b: Expression) extends Expression{

  override def toByteCode: List[CodeOp] = a.toByteCode ++ b.toByteCode ++ List(BinaryOp("SUM"))

  override def toString: String = s"($a + $b)"
}


case class Sub (a:Expression, b: Expression) extends Expression{

  override def toByteCode: List[CodeOp] = a.toByteCode ++ b.toByteCode ++ List(BinaryOp("SUB"))

  override def toString: String = s"($a - $b)"
}

case class Prod (a:Expression, b: Expression) extends Expression{

  override def toByteCode: List[CodeOp] = a.toByteCode ++ b.toByteCode ++ List(BinaryOp("PROD"))

  override def toString: String = s"$a * $b"
}

case class Div (a:Expression, b: Expression) extends Expression{

  override def toByteCode: List[CodeOp] = a.toByteCode ++ b.toByteCode ++ List(BinaryOp("DIV"))

  override def toString: String = s"$a / $b"
}


case class Variable(name: String) extends Expression{

  override def toByteCode: List[CodeOp] = List(Load(name))

  override def toString: String = s"<$name>"
}


case class Pow(expr: Expression, degree: Double ) extends Expression{

  override def toByteCode: List[CodeOp] = expr.toByteCode ++ List(Push(degree), BinaryOp("POW"))

  override def toString: String = s"($expr ^ $degree)"
}

case class Not(expr: Expression) extends Expression{

  override def toByteCode: List[CodeOp] = expr.toByteCode ++  List( UnaryOp("NOT"))

  override def toString: String = s"-$expr"
}

case class Equality (name: String, exp: Expression) extends  Expression {

  override def toString: String = s"$name = $exp"

  override def toByteCode: List[CodeOp] = exp.toByteCode ++ List(Store(name))
}

case class Condition(left: Expression,op: String, right: Expression) extends Expression {

  override def toString: String = s"$left $op $right"

  override def toByteCode: List[CodeOp] = {
    List(If(op, Int.MaxValue)) ++ left.toByteCode ++ right.toByteCode
  }
}

case class Return(exp: Expression) extends Expression{

  override def toString: String = s"return $exp"

  override def toByteCode: List[CodeOp] = exp.toByteCode ++ List(Ret())
}

case class IfThen(cond: Condition, body: List[Expression]) extends Expression{
  override def toString: String = s"if ( $cond ){\n $body\n}"

  override def toByteCode: List[CodeOp] = {

    val b = gen(body)
    val c = cond.toByteCode
    val res = c.tail
    c.head match{
      case If(op, adr) =>  res ++ List(If(op,  b.length )) ++ b
      case others => throw new Error(s"expected If, found $others")
    }

  }
}

case class Loop(condition: Condition, expr: List[Expression]) extends Expression{

  override def toString: String = s"while( $condition){\n$expr\n}\n"

  override def toByteCode: List[CodeOp] ={
    val b = gen(expr)
    val c = condition.toByteCode
    val res = c.tail
    c.head match{
      case If(op, adr) =>  res ++ List(If(op,  b.length + 1)) ++ b ++ List(Goto(-(b.length + c.length)))
      case others => throw new Error(s"expected If, found $others")
    }
  }
}