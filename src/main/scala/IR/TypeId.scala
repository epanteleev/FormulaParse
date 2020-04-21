package IR

sealed trait TypeId extends Any

case class tInt(number: Int) extends AnyVal with TypeId {
  override def toString: String = s"I32 $number"
}

case class tFloat(number: Float) extends AnyVal with TypeId {
  override def toString: String = s"F32 $number"
}

case class tDouble(number: Double) extends AnyVal with TypeId {
  override def toString: String = s"F64 $number"
}

case class tBool(number: Boolean) extends AnyVal with TypeId {
  override def toString: String = s"bl  $number"
}

case class tByte(number: Byte) extends AnyVal with TypeId {
  override def toString: String = s"U8  $number"
}

case class tRef(number: Int) extends AnyVal with TypeId {
  override def toString: String = s"REF  $number"
}