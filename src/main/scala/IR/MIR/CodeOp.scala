package IR.MIR

sealed trait CodeOp

case class Push(num: Double) extends CodeOp {
  override def toString: String = s"PUSH $num"
}

case class BinaryOp(op: String) extends CodeOp {
  override def toString: String = op
}

case class UnaryOp(op: String) extends CodeOp {
  override def toString: String = op
}

case class Store(name: String) extends CodeOp {
  override def toString: String = s"STORE $name "
}

case class Load(name: String) extends CodeOp {
  override def toString: String = s"LOAD $name "
}

case class IO(command: String) extends CodeOp {
  override def toString: String = command
}

case class Ret() extends CodeOp {
  override def toString: String = "RETURN"
}

case class Cmp(cmp: CmpOp) extends CodeOp {
  override def toString: String = s"CMP $cmp"
}

case class Goto(labelName: Int) extends CodeOp {
  override def toString: String = s"GOTO L$labelName"
}

case class Call(name: String) extends CodeOp {
  override def toString: String = s"CALL $name"
}

case class If(labelIf: Int, labelElse: Int) extends CodeOp {
  override def toString: String = s"IF L$labelIf L$labelElse"
}