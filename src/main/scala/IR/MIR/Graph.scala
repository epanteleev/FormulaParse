package IR.MIR

import IR.MIR.Edges.Edges

class Graph private (private var edges: Edges, private var block: List[Block]) {

  def addBlock(bl: Block): Unit = {
    block = block ++ List(bl)
  }

  override def toString: String = {
    block.foldLeft(new String())( (x: String , y: Block) => { s"$x\n$y" } )
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
  def apply(): Graph = Graph(Edges(), List(Block(List())))
  def apply(edges: Edges, blocks: List[Block]): Graph = {
    //BlockIdGen.reset()
    new Graph(edges, blocks)
  }
  def unapply(arg: Graph): Option[(Edges, List[Block])] = Some(arg.edges, arg.block)
}
