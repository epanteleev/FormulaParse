package Compiler

import scala.collection.mutable

class RegAllocator {
  private val x86regs = Array("rbx", "rcx", "rsi", "rdi", "rdx","rax")
  private val stack: mutable.Stack[String] = mutable.Stack[String]()

  def allocate: String = {
    val len = stack.length
    val arrLen = x86regs.length
    val ret = if(stack.isEmpty) x86regs(0)
    else  x86regs((len) % arrLen)
    push(ret)
    ret
  }
  def map[A](f :String => A): mutable.Stack[A] = {
    val ret = stack.map(f)
    stack.clear()
    ret
  }

  def push(reg: String): Unit = stack push reg

  def pop: String = stack pop
}

object RegAllocator {
  val stackPointer: String = "rsp"
  val basePointer: String = "rbp"
  def apply(): RegAllocator = new RegAllocator()
}
