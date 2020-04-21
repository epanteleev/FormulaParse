package IR.MIR

class Graph private(var block: List[Block]) {

  def addBlock(bl: Block): Unit = {
    block = block ++ List(bl)
  }

  override def toString: String = {
    block.foldLeft(new String())((x: String, y: Block) => {
      s"$x\n$y"
    })
  }

  def findBlock(id: Int): Option[Block] = {
    block.find((p: Block) => (p.id == id))
  }

  def nextBlock(bl: Block): Option[Block]  = {
    findBlock(bl.id + 1)
  }

  def lastBlock: Block = block.last

  def insert(bl: Block): Graph = {
    block = block ++ List(bl)
    this
  }
}


object Graph {
  def apply(): Graph = {
    BlockIdGen.reset()
    new Graph(List(Block(List())))
  }

  def unapply(arg: Graph): Option[List[Block]] = Some(arg.block)
}
