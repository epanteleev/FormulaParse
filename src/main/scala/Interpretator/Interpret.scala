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
    "notEQ" -> ((a: Double, b : Double) => a != b))

  private def end: Double = if (stack.nonEmpty) stack pop else throw new Error("stack is empty")

  def exec(code: List[CodeOp]): Double = {
    code match {

      case Push(num) :: list => {
        stack.push(num)
        exec(code.tail)
      }
      case BinaryOp("SUM") :: list => {
        stack.push(stack.pop + stack.pop)
        exec(code.tail)
      }
      case BinaryOp("SUB") :: list => {
        stack.push(-(stack.pop - stack.pop))
        exec(code.tail)
      }
      case BinaryOp("PROD") :: list => {
        stack.push(stack.pop * stack.pop)
        exec(code.tail)
      }
      case BinaryOp("DIV") :: list => {
        val a = stack.pop
        val b = stack.pop
        stack.push(b / a)
        exec(code.tail)
      }
      case BinaryOp("POW") :: list => {
        val a = stack.pop
        val b = stack.pop
        stack.push(math.pow(b, a))
        exec(code.tail)
      }

      case Store(name) :: list => {
        val saved = stack.pop()
        env.put(name,saved)
        exec(code.tail)
      }
      case Load(name) :: list => {
        val loaded = {env.get(name) match {
          case Some(double) => double
          case None => throw new Error("xoxo")
        }}
        stack.push(loaded)
        exec(code.tail)
      }
      case IO("PRINT") :: list => {
        println(stack.pop)
        exec(code.tail)
      }
      case If(cmp,b) :: list => {
        val bl = map(cmp)(stack.pop(),stack.pop)
        if(bl) exec(code.tail)
        else exec(code.drop(b + 1))
      }
      case Goto(adr) :: list => {
        if(adr < 0){
          val newList = allcode.drop(allcode.length - (code.length - adr))
          exec(newList)
        } else{
          exec( code.drop(adr + 1))
        }
      }
      case Ret() :: list => end
      case Nil => end
    }
  }


}

object Interpret {
  def apply(code: List[CodeOp]): Double = new Interpret(code) exec (code)
}