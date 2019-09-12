package Interpretator

import Calculator._
import scala.collection.mutable
import scala.collection.mutable.{HashMap, Stack}

object Execute{
  def apply(input: String): Double = Interpret(MakeByteCode(input))
}

private class Interpret (code: List[CodeOp]) {
  val VarInfo: mutable.HashMap[String, Double] = mutable.HashMap[String, Double]()
  val stack: mutable.Stack[Double] =  mutable.Stack[Double]()
  val labelAdr: mutable.HashMap[String, Int] = mutable.HashMap[String, Int]()

  val cmpFunc: Map[String, (Double, Double) => Boolean] = Map(
    "EQ" -> ((a: Double, b : Double) => a == b),
    "NOTEQ" -> ((a: Double, b : Double) => a != b),
    "LESS" -> ((a: Double, b : Double) => a < b),
    "GREATER" -> ((a: Double, b : Double) => a > b))

  val arr: Array[CodeOp] = code toArray

  def exec: Double = {

    for (it <- arr.indices) {
      arr(it) match {
        case Label(name) => labelAdr.put(name, it)
        case _ => ()
      }
    }
    doneCommand(0)
  }

  @scala.annotation.tailrec
  private def doneCommand(adr: Int): Int = {
    if (adr >= code.length) stack.pop toInt
    else code(adr) match {
      case Push(num) => {
        stack.push(num)
        doneCommand(adr + 1)
      }
      case BinaryOp("SUM") => {
        stack.push(stack.pop + stack.pop)
        doneCommand(adr + 1)
      }
      case BinaryOp("SUB") => {
        stack.push(-(stack.pop - stack.pop))
        doneCommand(adr + 1)
      }
      case BinaryOp("PROD") => {
        stack.push(stack.pop * stack.pop)
        doneCommand(adr + 1)
      }
      case BinaryOp("DIV") => {
        val a = stack.pop
        val b = stack.pop
        stack.push(b / a)
        doneCommand(adr + 1)
      }
      case BinaryOp("POW") => {
        val a = stack.pop
        val b = stack.pop
        stack.push(math.pow(b, a))
        doneCommand(adr + 1)
      }

      case Store(name) => {
        val saved = stack.pop()
        VarInfo.put(name, saved)
        doneCommand(adr + 1)
      }
      case Load(name) => {
        val loaded = {
          VarInfo.get(name) match {
            case Some(double) => double
            case None => throw new Error("xoxo")
          }
        }
        stack.push(loaded)
        doneCommand(adr + 1)
      }
      case IO("PRINT") => {
        println(stack.pop)
        doneCommand(adr + 1)
      }
      case If(cmp, b) => {
        val u = stack.pop()
        val bl = cmpFunc(cmp)(stack.pop, u)
        if (bl) doneCommand(1 + labelAdr (b))
        else  doneCommand(adr + 1)
      }
      case Goto(pos) => {
         doneCommand(labelAdr(pos))
      }
      case Label(name) => {
        labelAdr.put(name, adr)
        doneCommand(adr + 1)
      }
      case Ret() => {
        stack.pop toInt
      }
      case otherwise => error(s"command skipped: $otherwise")
    }
  }
}

object Interpret {
  def apply(code: List[CodeOp]): Double = new Interpret(code) exec
}