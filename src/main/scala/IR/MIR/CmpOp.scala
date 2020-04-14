package IR.MIR

import IR.MIR

import scala.collection.mutable

trait CmpOp {
  def not: CmpOp = {
    this match {
      case _: Less => Greater()
      case _: Greater => Less()
      case _: Eq => NotEq()
      case _: NotEq => Eq()
      case _ => throw new RuntimeException("Match error")
    }
  }
}

object CmpOp {
  private lazy val cmp: Map[String, CmpOp] = Map(
    "==" -> (Eq()),
    "!=" -> (NotEq()),
    "<" -> (Less()),
    ">" -> (Greater())
  )
  def apply(op: String): CmpOp = CmpOp.cmp(op)
}

case class Eq() extends CmpOp {
  override def toString: String = "EQ"
}
case class NotEq() extends CmpOp {
  override def toString: String = "NE"
}
case class Less() extends CmpOp {
  override def toString: String = "LS"
}
case class Greater() extends CmpOp {
  override def toString: String = "GT"
}

