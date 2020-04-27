package IR.MIR

import scala.collection.mutable

class Block private(var inst: List[Inst], val id: Int) {
  var defInfo: mutable.Set[String] = mutable.Set[String]()

  var flowInst: Inst = null

  def pushInst(i: Inst): Unit = inst = inst ++ List(i)

  def !=(bl: Block): Boolean = id != bl.id
  def ==(bl: Block): Boolean = id == bl.id

  def addDef(name: String):Boolean = defInfo add name

  def isDef(name: String):Boolean = defInfo contains name

  def notDef(name: String):Boolean = !isDef(name)

  override def toString: String = {
    val instStr = inst.foldLeft(new mutable.StringBuilder())((x: mutable.StringBuilder, y: Inst) => x ++= s"\n\t$y")
    val defStr = defInfo.foldLeft(new mutable.StringBuilder("["))((x: mutable.StringBuilder, y: String) => x ++= s" $y ") + "]"
    s"L$id: " + defStr + instStr  + s"\n\t$flowInst"
  }
}

object Block {
  def apply(codeOp: List[Inst]): Block = new Block(codeOp, BlockIdGen())

  def apply(): Block = new Block(List(), BlockIdGen())

  def unapply(arg: Block): Option[(List[Inst])] = Some(arg.inst)
}

object BlockIdGen {
  private var id: Int = -1

  def reset(): Unit = {
    id = -1
  }

  def apply(): Int = {
    id = id + 1
    id
  }
}