package IR

sealed trait TypeKind {}
case class tInt() extends TypeKind {
  override def toString: String = "int"
}
case class tFloat() extends TypeKind {
  override def toString: String = "float"
}
case class tDouble() extends TypeKind {
  override def toString: String = "double"
}
case class tByte() extends TypeKind {
  override def toString: String = "byte"
}