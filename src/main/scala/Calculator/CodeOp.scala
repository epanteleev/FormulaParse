package Calculator

sealed trait Operations


case class Push(num: Double) extends Operations{
  override def toString: String = s"PUSH $num"
}


case class BinaryOp(op: String) extends Operations{
  override def toString: String = op
}


case class UnaryOp(op: String) extends Operations{
  override def toString: String = op
}

case class Store(name: String) extends Operations{
  override def toString: String = s"STORE $name "
}


case class Load(name: String) extends Operations{
  override def toString: String = s"LOAD $name "
}

case class IO (command: String) extends Operations{
  override def toString: String = command
}

case class Ret() extends Operations{
  override def toString: String = "RETURN"
}