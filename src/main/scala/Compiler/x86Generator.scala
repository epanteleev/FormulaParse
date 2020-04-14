//package Compiler
//import IR.MIR.{Begin, BinaryOp, Call, CodeOp, End, Goto, If, Label, Load, Push, Ret, Store}
//import IR._
//
//import scala.collection.mutable
//import scala.collection.mutable.{HashMap, Stack}
//
//class RegAlloc {
//  private val x86regs = Array("%ebx", "%ecx", "%esi", "%edi", "%edx","%eax")
//  private val stack: mutable.Stack[String] = mutable.Stack[String]()
//
//  def allocate: String = {
//    val len = stack.length
//    val arrLen = x86regs.length
//    val ret = if(stack.isEmpty) x86regs(0)
//              else  x86regs((len) % arrLen)
//    add(ret)
//    ret
//  }
//  def map[A](f :String => A): mutable.Stack[A] = {
//    val ret = stack.map(f)
//    stack.clear()
//    ret
//  }
//
//  def stackPointer: String = "%esp"
//  def basePointer: String = "%ebp"
//  def add(reg: String): Unit = stack push reg
//  def getReg: String = stack pop
//}
//
//object RegAlloc {
//  def apply(): RegAlloc = new RegAlloc()
//}
//
//class x86Generator {
//  var vars: mutable.Set[String] = mutable.Set[String]()
//  val RegBuff: RegAlloc = RegAlloc()
//  val localSpace = StackFrame()
//
//  @scala.annotation.tailrec
//  final def gen(acc: List[x86Instruction] = List(), code: List[CodeOp]): List[x86Instruction] = {
//
//    code match {
//      case Nil => acc
//      case Load(nameVar) :: Push(n) :: If(cmp, labelName) :: list => {
//        val s = localSpace.getPos(nameVar)
//        gen(acc ++ List(x86Cmp("-" + s + "(" + RegBuff.basePointer + ")", "$" + (n toInt) toString),
//          CmpToInst(cmp, labelName)), list)
//      }
//      case Push(n) :: Load(nameVar) :: If(cmp, labelName) :: list => {
//        val s = localSpace.getPos(nameVar)
//        gen(acc ++ List(x86Cmp("-" + s + "(" + RegBuff.basePointer + ")", "$" + (n toInt) toString),
//          CmpToInst(cmp, labelName)), list)
//      }
//      case If(cmp, labelName) :: list => {
//        val a = RegBuff.getReg
//        val b = RegBuff.getReg
//        gen(acc ++ List(x86Cmp(a, b), CmpToInst(cmp, labelName)), list)
//      }
//      case Load(name) :: list => {
//        val s = localSpace.getPos(name)
//        val r = RegBuff.allocate
//        gen(acc ++ List(x86Movl("-" + s + "(" + RegBuff.basePointer + ")", r)), list)
//      }
//      case Push(num) :: _ => {
//        val s = RegBuff.allocate
//        gen(acc ++ List(x86Movl("$" + num.toInt.toString , s)), code.tail)
//      }
//      case Store(name) :: list => {
//        if(!localSpace.contains(name)){
//          val s = RegBuff.getReg
//          localSpace.push( name)
//          gen(acc ++ List(x86Push(s)), list)
//        }else{
//          val s = localSpace.getPos(name)
//          val r = RegBuff.getReg
//          gen(acc ++ List(x86Movl(r, "-" + s + "(" + RegBuff.basePointer + ")")), code.tail)
//        }
//      }
//      case Ret() :: _ => {
//        gen(acc ++ List( x86Movl(RegBuff.getReg, "%eax"),freeStack, x86Ret()), code.tail)
//      }
//      case Goto(name) :: list => {
//        gen(acc ++ List(x86Jump(name, "jmp")), list)
//      }
//      case End() :: list => {
//        gen(acc ++ List(freeStack), list)
//      }
//      case Begin() :: list => {
//        localSpace.newFrame
//        gen(acc, list)
//      }
//      case Label(name) :: _ => {
//        gen(acc ++ List(x86Label(name)), code.tail)
//      }
//      case BinaryOp(op) :: list => {
//        val x = RegBuff.getReg
//        val y = RegBuff.getReg
//        val ret = List(x86BinOp(BinaryOpToInst(op), x, y))
//        RegBuff.add(y)
//        gen(acc ++ ret, list)
//      }
//      case Call(name) :: list => {
//        gen(acc ++ RegBuff.map(x => {localSpace.push(x); x86Push(x)}) ++ List({RegBuff.add("%eax"); x86Call(name)}), list)
//      }
//   }
//  }
//
//  def freeStack: x86BinOp = {
//    val frame = localSpace.pop
//    x86BinOp(BinaryOpToInst("SUM"), "$" + frame.size, RegBuff.stackPointer)
//  }
//
//  def createDataSegment: String = {
//    if(vars.nonEmpty)
//      "\t.data\n" + vars.map(x => s"$x:\t.long\n").mkString
//    else ""
//  }
//
//  def createCodeSegment(op: List[CodeOp]): String =
//    gen(List(x86Movl(RegBuff.stackPointer, RegBuff.basePointer)), op).map(x => x.toString).mkString("\n")
//
//  def compilate(op: List[CodeOp]): String = {
//    val code =  "\t.global main\n\t.text\nmain:\n" + createCodeSegment(op)
//    createDataSegment + code
//  }
//
//  def BinaryOpToInst(op: String): String = {
//    op match {
//      case "SUM" => "addl"
//      case "SUB" => "subl"
//      case "PROD" => "imull"
//      case "DIV" => "divl"
//    }
//  }
//  def CmpToInst(cmpType: String, label: String): x86Jump = {
//    val t = cmpType match {
//      case "EQ" => "je"
//      case "NOTEQ" => "jne"
//      case "LESS" => "jl"
//      case "GREATER" => "jg"
//    }
//    x86Jump(label, t)
//  }
//}
//
//object x86Generator{
//  def apply(): x86Generator = new x86Generator()
//}
