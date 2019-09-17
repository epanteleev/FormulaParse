package Calculator

sealed trait Expression {
  def toByteCode: List[CodeOp]

  def gen(result: List[Expression], list: List[CodeOp] = List()):List[CodeOp] = {
    result match {
      case head :: l => gen(result.tail, list ++ head.toByteCode)
      case Nil => list
    }
  }
}

case class Number(double: Double) extends Expression {

  override def toByteCode: List[CodeOp] = List(Push(double))

  override def toString: String = double toString
}

case class Summ(a:Expression, b: Expression) extends Expression {

  override def toByteCode: List[CodeOp] = a.toByteCode ++ b.toByteCode ++ List(BinaryOp("SUM"))

  override def toString: String = s"($a + $b)"
}

case class Sub(a:Expression, b: Expression) extends Expression {

  override def toByteCode: List[CodeOp] = a.toByteCode ++ b.toByteCode ++ List(BinaryOp("SUB"))

  override def toString: String = s"($a - $b)"
}

case class Prod(a:Expression, b: Expression) extends Expression{

  override def toByteCode: List[CodeOp] = a.toByteCode ++ b.toByteCode ++ List(BinaryOp("PROD"))

  override def toString: String = s"$a * $b"
}

case class Div(a:Expression, b: Expression) extends Expression {

  override def toByteCode: List[CodeOp] = a.toByteCode ++ b.toByteCode ++ List(BinaryOp("DIV"))

  override def toString: String = s"$a / $b"
}

case class Variable(name: String) extends Expression {

  override def toByteCode: List[CodeOp] = List(Load(name))

  override def toString: String = s"<$name>"
}

case class Pow(expr: Expression, degree: Double ) extends Expression {

  override def toByteCode: List[CodeOp] = expr.toByteCode ++ List(Push(degree), BinaryOp("POW"))

  override def toString: String = s"($expr ^ $degree)"
}

case class Not(expr: Expression) extends Expression {

  override def toByteCode: List[CodeOp] = expr.toByteCode ++  List( UnaryOp("NOT"))

  override def toString: String = s"-$expr"
}

case class Equality(name: String, exp: Expression) extends Expression {

  override def toString: String = s"$name = $exp"

  override def toByteCode: List[CodeOp] = exp.toByteCode ++ List(Store(name))
}

case class Condition(left: Expression,op: String, right: Expression) extends Expression {

  override def toString: String = s"$left $op $right"

  override def toByteCode: List[CodeOp] = {
    List(If(op, NameGen())) ++ left.toByteCode ++ right.toByteCode
  }
}

case class Return(exp: Expression) extends Expression{

  override def toString: String = s"return $exp"

  override def toByteCode: List[CodeOp] = exp.toByteCode ++ List(Ret())
}

case class IfThen(condition: Condition, body: List[Expression], elseBlock: List[Expression]) extends Expression{
  override def toString: String = s"if ( $condition ) {\n $body\n} else {\n$elseBlock\n}"

  override def toByteCode: List[CodeOp] = {
    /*
      GOTO lO
      LABEL l1
       <<ifBl>>
      GOTO l2
       <prepare args>
      IF <cmp> L1
       <elseBlock>
      LABEL l2
     */
    val (ifBl, elseBl) = (gen(body), gen(elseBlock))
    val cond = condition.toByteCode
    val res = cond.tail
    cond.head match{
      case If(op, l1) => {
        val l0 = NameGen()
        val l2 = NameGen()
        List(Goto(l0), Label(l1), Begin()) ++ ifBl ++ List(End(), Goto(l2), Label(l0)) ++ res ++ List(If(op, l1),Begin()) ++ elseBl ++ List(End(), Label(l2))
      }
      case others => throw new Error(s"expected If, found $others")
    }

  }
}

case class Loop(condition: Condition, expr: List[Expression]) extends Expression {

  override def toString: String = s"while( $condition) {\n$expr\n}\n"

  override def toByteCode: List[CodeOp] = {
    val Block = gen(expr)
    val c = condition.toByteCode
    val res = c.tail
    c.head match {
      case If(op, l1) =>  {
        val l0 = NameGen()
        List(Goto(l0), Label(l1), Begin()) ++ Block ++ List(End(), Label(l0)) ++ res ++ List(If(op, l1))
      }
      case others => throw new Error(s"expected If, found $others")
    }
  }
}

case class CallFunction(name: String, opList: List[Expression]) extends Expression {
  override def toString: String = s"$name($opList)"

  override def toByteCode: List[CodeOp] = {
    val args = gen(opList)
    args ++ List(Call(name))
  }
}