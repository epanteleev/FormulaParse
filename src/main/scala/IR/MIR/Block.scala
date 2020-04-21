package IR.MIR

trait Node{
  def id(): Int
}

class Block private(var inst: List[Inst], val id: Int) extends Node {

  var flowInst: Inst = null

  @deprecated
  def instruction: List[Inst] = inst

  def pushInst(i: Inst): Unit = inst = inst ++ List(i)

  def !=(bl: Block): Boolean = id != bl.id

  def ==(bl: Block): Boolean = id == bl.id

  override def toString: String = s"L$id:" +
    inst.foldLeft(new String())((x: String, y: Inst) => s"$x\n\t$y") + s"\n\t$flowInst"
}

object Block {
  def apply(codeOp: List[Inst]): Block = new Block(codeOp, BlockIdGen())

  def apply(): Block = new Block(List(), BlockIdGen())

  def unapply(arg: Block): Option[(List[Inst])] = Some(arg.inst)
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