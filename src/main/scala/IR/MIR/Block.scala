package IR.MIR

import IR.Flow

trait Node{
  def id(): Int
}

class Block private(var inst: List[CodeOp], val id: Int) extends Node {
  var ifInst: If = _
  def instruction: List[CodeOp] = inst

  def pushInst(i: CodeOp): Unit = inst = inst ++ List(i)

  def !=(bl: Block): Boolean = id != bl.id
  def ==(bl: Block): Boolean = id == bl.id

  override def toString: String = s"L$id:" +
    inst.foldLeft(new String())((x: String, y: CodeOp) => s"$x\n\t$y")
}

object Block {
  def apply(codeOp: List[CodeOp]): Block = new Block(codeOp, BlockIdGen())
  def apply(): Block = new Block(List(), BlockIdGen())
  def unapply(arg: Block): Option[(List[CodeOp])] = Some(arg.inst)
}

class End extends Node {
  override def id(): Int = -1
}

object End {
  private var _end: End = null

  def apply(): End = {
    if (_end == null) {
      _end = new End()
    }
    _end
  }
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