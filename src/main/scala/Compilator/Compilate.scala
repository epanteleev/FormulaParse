package Compilator

import Calculator._

object Compilate {
  def apply(programm: String): String = {
    val code = MakeByteCode(programm)
    x86Generator.compilate(code)
  }
}
