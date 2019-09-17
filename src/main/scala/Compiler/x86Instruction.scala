package Compiler

sealed trait x86Instruction

case class x86Push(a: String) extends x86Instruction {
  override def toString: String = s"\tpushl\t$a"
}

case class x86Popl(des: String) extends x86Instruction{
  override def toString: String = s"\tpopl\t$des"
}

case class x86Movl(src: String, des: String) extends x86Instruction{
  override def toString: String = s"\tmovl\t$src, $des"
}

case class x86BinOp(op: String, a: String, b: String) extends x86Instruction{
  override def toString: String = s"\t$op\t$a, $b"
}

case class x86Call(label: String) extends x86Instruction{
  override def toString: String = s"\tcall\t$label"
}

case class x86Ret() extends x86Instruction {
  override def toString: String = s"\tret"
}

case class x86Label(name: String) extends x86Instruction {
  override def toString: String = s"$name:"
}

case class x86Jump(labelName: String, t: String) extends x86Instruction {
  override def toString: String = s"\t$t\t$labelName"
}

case class x86Cmp(arg1: String, arg2: String) extends x86Instruction {
  override def toString: String = s"\tcmpl $arg2, $arg1"
}