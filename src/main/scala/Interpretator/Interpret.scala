package Calculator

import scala.collection.mutable
import scala.collection.mutable.Stack
import scala.collection.mutable.HashMap

object Execute{
  def apply(input: String): Double = Interpret(MakeByteCode(input))
}

private object Interpret {
  val env: HashMap[String, Double] = mutable.HashMap[String, Double]()
  val stack: Stack[Double] =  Stack[Double]()

  @scala.annotation.tailrec
  def exec(code: List[Operations]): Double = {
    code match {
      case Nil => stack.pop()
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
    }
  }

  def apply(code: List[Operations]): Double = exec(code)
}
