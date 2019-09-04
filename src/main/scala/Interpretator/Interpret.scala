package Interpretator

import Calculator._
import scala.collection.mutable
import scala.collection.mutable.{HashMap, Stack}

object Execute{
  def apply(input: String): Double = Interpret(MakeByteCode(input))
}

private class Interpret (allcode: List[CodeOp]) {
  var env: mutable.HashMap[String, Double] = mutable.HashMap[String, Double]()
  var stack: mutable.Stack[Double] =  mutable.Stack[Double]()
  var map: Map[String, (Double, Double) => Boolean] = Map(
    "EQ" -> ((a: Double, b : Double) => a==b),
    "NOTEQ" -> ((a: Double, b : Double) => a != b))
  var ret: Double = Double.MaxValue



  def exec(code: List[CodeOp]): Double = {
    val arr: Array[CodeOp] = code toArray
    var address: Int = 0

    while(address < code.length){
      address = doneCommand(address, arr(address))
    }
    if(stack.nonEmpty) stack pop else ret
  }

  private def doneCommand(adr: Int, code: CodeOp): Int ={
    code match {
      case Push(num) => {
        stack.push(num)
        adr + 1
      }
      case BinaryOp("SUM") => {
        stack.push(stack.pop + stack.pop)
        adr + 1
      }
      case BinaryOp("SUB") => {
        stack.push(-(stack.pop - stack.pop))
        adr + 1
      }
      case BinaryOp("PROD") => {
        stack.push(stack.pop * stack.pop)
        adr + 1
      }
      case BinaryOp("DIV") => {
        val a = stack.pop
        val b = stack.pop
        stack.push(b / a)
        adr + 1
      }
      case BinaryOp("POW") => {
        val a = stack.pop
        val b = stack.pop
        stack.push(math.pow(b, a))
        adr + 1
      }

      case Store(name) => {
        val saved = stack.pop()
        env.put(name, saved)
        adr + 1
      }
      case Load(name) => {
        val loaded = {
          env.get(name) match {
            case Some(double) => double
            case None => throw new Error("xoxo")
          }
        }
        stack.push(loaded)
        adr + 1
      }
      case IO("PRINT") => {
        println(stack.pop)
        adr + 1
      }
      case If(cmp, b) => {
        val bl = map(cmp)(stack.pop(), stack.pop)
        if (bl) adr + 1
        else adr + b + 1
      }
      case Goto(pos) => {
        adr + pos
      }
      case Ret() => {
        ret = stack.pop
        Int.MaxValue
      }
    }
  }

}

object Interpret {
  def apply(code: List[CodeOp]): Double = new Interpret(code) exec code
}