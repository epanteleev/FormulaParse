package Compiler

sealed trait x86Instruction

case class x86Push(a: String) extends x86Instruction {
  override def toString: String = s"push $a"
}

case class x86Popl(des: String) extends x86Instruction{
  override def toString: String = s"pop\t$des"
}

case class x86Movl(des: String, src: String) extends x86Instruction {
  override def toString: String = s"mov\t$des,$src"
}

case class x86BinOp(op: String, a: String, b: String) extends x86Instruction {
  override def toString: String = s"$op\t$a, $b"
}

object x86Add {
  def apply(a: String, b: String): x86BinOp = x86BinOp("add", a, b)
}

object x86Sub {
  def apply(a: String, b: String): x86BinOp = x86BinOp("sub", a, b)
}

object x86Prod {
  def apply(a: String, b: String): x86BinOp = x86BinOp("mul", a, b)
}

object x86Div {
  def apply(a: String, b: String): x86BinOp = x86BinOp("div", a, b)
}

case class x86Call(label: String) extends x86Instruction {
  override def toString: String = s"call\t$label"
}

case class x86Ret() extends x86Instruction {
  override def toString: String = s"ret"
}

case class x86Label(name: String) extends x86Instruction {
  override def toString: String = s"$name:"
}

case class x86Jump(labelName: String, t: String) extends x86Instruction {
  override def toString: String = s"$t\t$labelName"
}

case class x86Cmp(arg1: String, arg2: String) extends x86Instruction {
  override def toString: String = s"cmp $arg1, $arg2"
}