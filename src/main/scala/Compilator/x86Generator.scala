package Compilator
import Calculator._

import scala.collection.mutable
import scala.collection.mutable.{HashMap, Stack}

//class RegAlloc{
//  private var stack: mutable.Stack[String] = mutable.Stack[String]()
//  def pop:String = stack.pop
//  def alloc =
//}
object x86Generator {
  val x86regs = Array("%ebx", "%ecx", "%esi", "%edi", "%eax", "%edx")
  var stack: mutable.Stack[String] = mutable.Stack[String]()
  var vars: mutable.Set[String] = mutable.Set[String]()

  def allocateReg: String = {
    val len = stack.length
    val arrLen = x86regs.length
    if(stack.isEmpty) "%eax"
    else  x86regs((len + 1) % arrLen)
  }

  def gen(code: List[CodeOp]): List[x86Instruction] ={

    code match {
      case Nil => List()
      case Load(name) :: list => {
        val s = allocateReg
        stack.push(s)
        List(x86Movl(name,s)) ++ gen(code.tail)
      }
      case Push(num) :: list => {
        val s = allocateReg
        stack.push(s)
        List(x86Movl("$" + num.toInt.toString , s)) ++ gen(code.tail)
      }
      case Store(name) :: list =>{
        vars = vars + name
        val s = stack.pop
        List(x86Movl(s, name)) ++ gen(code.tail)
      }
      case Ret() :: list => {
        List(x86Ret()) ++ gen(code.tail)
      }
      case BinaryOp(op) :: list => {
        val x = stack.pop
        val y = stack.pop
        val ret = List(x86Movl(y, "%eax"),
          x86BinOp(OptoInst(BinaryOp(op)), x, "%eax"))
        stack.push(y)
        ret ++ gen(code.tail)
      }

    }

  }

  def createDataSegment: String = "\t.data\n" + vars.map(x => s"$x:\t.int\n").mkString

  def compilate(op: List[CodeOp]): String = {
    val code =  "\t.global main\n\t.text\nmain:\n" + gen(op).map(x => x.toString).mkString("\n")
    createDataSegment + code
  }

  def OptoInst(op: CodeOp): String ={
    op match {
      case BinaryOp("SUM") => "addl"
      case BinaryOp("SUB") => "subl"
      case BinaryOp("PROD") => "imull"
    }
  }
}
