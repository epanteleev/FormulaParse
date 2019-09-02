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

case class If(cmp: String, label: Int) extends CodeOp{
  override def toString: String = s"IF $cmp $label"
}

case class Goto(adr: Int) extends CodeOp{
  override def toString: String = s"GOTO $adr"
}
