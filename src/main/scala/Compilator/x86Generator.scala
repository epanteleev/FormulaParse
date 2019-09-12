package Compilator
import Calculator._

import scala.collection.mutable
import scala.collection.mutable.{HashMap, Stack}

class RegAlloc {
  private val x86regs = Array("%ebx", "%ecx", "%esi", "%edi", "%edx","%eax")
  private val stack: mutable.Stack[String] = mutable.Stack[String]()

  def allocate: String = {
    val len = stack.length
    val arrLen = x86regs.length
    val ret = if(stack.isEmpty) x86regs(0)
                else  x86regs((len) % arrLen)
    add(ret)
    ret
  }
  def stackPointer: String = "%esp"
  def basePointer: String = "ebp"
  def add(reg: String): Unit = stack push reg
  def getReg: String = stack pop
}

object RegAlloc {
  def apply(): RegAlloc = new RegAlloc()
}

class LocalSpace {
  private var stack: List[String] =  List[String]()
  def push(a: String): Unit = {
    stack = a :: stack
  }
  def size: Int = stack.size * 4

  def getPos(nameVar: String): Int = {
    @scala.annotation.tailrec
    def iter(s: List[String], it: Int): Int = {
      s match {
        case a::list  => if(a == nameVar) it else iter(s.tail, it + 1)
        case Nil => 0
      }
    }
    iter(stack, 0) * 4
  }

  def contains(nameVar: String): Boolean = stack.contains(nameVar)
}

object LocalSpace {
  def apply(): LocalSpace = new LocalSpace()
}

object x86Generator {
  var vars: mutable.Set[String] = mutable.Set[String]()
  val RegBuff: RegAlloc = RegAlloc()
  val localSpace = LocalSpace()

  @scala.annotation.tailrec
  def gen(acc: List[x86Instruction] = List(), code: List[CodeOp]): List[x86Instruction] = {

    code match {
      case Nil => acc
      case Load(nameVar) :: Push(n) :: If(cmp, labelName) :: list => {
        val s = localSpace.getPos(nameVar)
        gen(acc ++ List(x86Cmp( s + "(" + RegBuff.stackPointer + ")", "$" + (n toInt) toString),
          CmpToInst(cmp, labelName)), code.drop(3))
      }
      case Load(name) :: _ => {
        val s = localSpace.getPos(name)
        val r = RegBuff.allocate
        gen(acc ++ List(x86Movl( s + "(" + RegBuff.stackPointer + ")", r)), code.tail)
      }
      case Push(num) :: _ => {
        val s = RegBuff.allocate
        gen(acc ++ List(x86Movl("$" + num.toInt.toString , s)), code.tail)
      }
      case Store(name) :: _ => {
        if(!localSpace.contains(name)){
          val s = RegBuff.getReg
          localSpace.push( name)
          gen(acc ++ List(x86Push(s)), code.tail)
        }else{
          val s = localSpace.getPos(name)
          val r = RegBuff.getReg
          gen(acc ++ List(x86Movl(r, s + "(" + RegBuff.stackPointer + ")")), code.tail)
        }
      }
      case Ret() :: _ => {
        gen(acc ++ List( x86Movl(RegBuff.getReg, "%eax"),freeStack, x86Ret()), code.tail)
      }
      case Goto(name) :: _ => {
        gen(acc ++ List(x86Jump(name, "jmp")) , code.tail)
      }
      case Label(name) :: _ => {
        gen(acc ++ List(x86Label(name)), code.tail)
      }
      case BinaryOp(op) :: _ => {
        val x = RegBuff.getReg
        val y = RegBuff.getReg
        val ret = List(x86Movl(y, "%eax"), x86BinOp(OpToInst(op), x, y))
        RegBuff.add(y)
        gen(acc ++ ret, code.tail)
      }
   }
  }

  def freeStack = x86BinOp(OpToInst("SUM"), "$" + localSpace.size, RegBuff.stackPointer)

  def createDataSegment: String = {
    if(vars.nonEmpty)
      "\t.data\n" + vars.map(x => s"$x:\t.long\n").mkString
    else ""
  }

  def createCodeSegment(op: List[CodeOp]): String = gen(List(), op).map(x => x.toString).mkString("\n")

  def compilate(op: List[CodeOp]): String = {
    val code =  "\t.global main\n\t.text\nmain:\n" + createCodeSegment(op)
    createDataSegment + code
  }

  def OpToInst(op: String): String = {
    op match {
      case "SUM" => "addl"
      case "SUB" => "subl"
      case "PROD" => "imull"
    }
  }
  def CmpToInst(cmpType: String, label: String): x86Jump = {
    val t = cmpType match {
      case "EQ" => "je"
      case "NOTEQ" => "jne"
      case "LESS" => "jl"
      case "GREATER" => "jg"
    }
    x86Jump(label, t)
  }
}
