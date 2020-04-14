package IR

abstract class NodeWalker[+T] {
  def walk(node: Sum): T

  def walk(node: Sub): T

  def walk(node: Prod): T

  def walk(node: Div): T

}
