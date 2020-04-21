package Compiler

import IR.MIR.Graph
import IR.{Convert, Expression, Parse}

object Compile {
  def apply(input: String): String = {
    val p = Parse(input)
    println(p.foldLeft(new String)((s: String, e: Expression) => s"$s\n${e.toString}"))
    val t = Convert(p)
    println(t)
    new x86Generate(t).interpretGraph.toString
  }

  def apply(graph: Graph): String = {
    new x86Generate(graph).interpretGraph.toString
  }
}
