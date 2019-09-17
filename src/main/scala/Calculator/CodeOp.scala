package Calculator

sealed trait CodeOp


case class Push(num: Double) extends CodeOp{
  override def toString: String = s"PUSH $num"
}

case class BinaryOp(op: String) extends CodeOp{
  override def toString: String = op
}

case class UnaryOp(op: String) extends CodeOp{
  override def toString: String = op
}

case class Store(name: String) extends CodeOp{
  override def toString: String = s"STORE $name "
}


case class Load(name: String) extends CodeOp{
  override def toString: String = s"LOAD $name "
}

case class IO (command: String) extends CodeOp{
  override def toString: String = command
}

case class Ret() extends CodeOp{
  override def toString: String = "RETURN"
}

case class If(cmp: String, labelName: String) extends CodeOp {
  override def toString: String = s"IF $cmp $labelName"
}

case class Goto(labelName: String) extends CodeOp {
  override def toString: String = s"GOTO $labelName"
}

case class Label(name: String) extends CodeOp {
  override def toString: String = s"LABEL $name"
}

case class End() extends CodeOp {
  override def toString: String = "END"
}

case class Begin() extends CodeOp {
  override def toString: String = "BEGIN"
}

case class Call(name: String) extends CodeOp {
  override def toString: String = s"CALL $name"
}