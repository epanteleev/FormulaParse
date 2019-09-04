package Compilator

sealed trait x86Instruction

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
  override def toString: String = s"\tcall $label"
}

case class x86Ret() extends x86Instruction{
  override def toString: String = s"\tret"
}