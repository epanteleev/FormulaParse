package IR.MIR


//case class Edges (edges:List[(Int, Int)]) {
//
//  def ++ (rth: Edges): Edges = Edges(edges ++ rth.edges)
//}

object Edges {
  type Edges = List[(Int, Int)]

  def apply(): Edges = List()
}