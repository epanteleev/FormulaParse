package Compilator

import Calculator._

object Compilate {
  def apply(programm: String): String = {
    NameGen.reset()
    val code = MakeByteCode(programm)
    x86Generator.compilate(code)
  }
}
