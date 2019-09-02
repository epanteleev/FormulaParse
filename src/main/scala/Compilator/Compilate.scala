package Compilator

import Calculator._
import scala.collection.mutable
import scala.collection.mutable.{HashMap, Stack}
import Calculator.{BinaryOp, IO, Load, CodeOp, Push, Store}

class Compilate(gen: x86Generator) {
  var env: mutable.HashMap[String, Double] = mutable.HashMap[String, Double]()
  var stack: mutable.Stack[Double] =  mutable.Stack[Double]()


  def exec(code: List[CodeOp]): String = {
    code match {
      case Nil => gen.get
      case Push(num) :: list => {
        gen.PUSH(num.toInt)
        exec(code.tail)
      }
      case BinaryOp("SUM") :: list => {
        gen.SUM()
        exec(code.tail)
      }
      case Store(name) :: list => {
        gen.STORE(name)
        exec(code.tail)
      }
      case Load(name) :: list => {
        gen.LOAD(name)
        exec(code.tail)
      }
      case Ret() :: list =>
        gen.RET
        exec(code.tail)
//      }
    }
  }
}

object Compilate{
  def apply(in: String): String = {
    new Compilate(new x86Generator) exec (MakeByteCode(in))
  }
}
