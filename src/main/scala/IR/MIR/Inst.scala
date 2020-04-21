package IR.MIR

import IR.TypeId

sealed trait Inst

case class Push(num: TypeId) extends Inst {
  override def toString: String = s"PUSH_$num"
}

case class BinaryOp(op: String) extends Inst {
  override def toString: String = op
}

case class UnaryOp(op: String) extends Inst {
  override def toString: String = op
}

case class Store(name: String) extends Inst {
  override def toString: String = s"STORE $name "
}

case class Load(name: String) extends Inst {
  override def toString: String = s"LOAD $name "
}

case class IO(command: String) extends Inst {
  override def toString: String = command
}

case class Ret() extends Inst {
  override def toString: String = "RETURN"
}

case class Cmp(cmp: CmpOp) extends Inst {
  override def toString: String = s"CMP $cmp"
}

case class Goto(labelName: Int) extends Inst {
  override def toString: String = s"GOTO L$labelName"
}

case class Call(name: String) extends Inst {
  override def toString: String = s"CALL $name"
}

case class If(labelIf: Int, labelElse: Int) extends Inst {
  override def toString: String = s"IF L$labelIf L$labelElse"
}