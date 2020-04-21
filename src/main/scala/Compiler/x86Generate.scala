package Compiler

import IR.MIR._
import IR._

import scala.collection.mutable

class x86Generate(cfg: Graph) {
  val RegBuff: RegAllocator = RegAllocator()
  val localSpace: StackFrame = StackFrame()
  var vars: mutable.Set[String] = mutable.Set[String]()

  def interpretGraph: x86Context = {
    val ctx = x86Context()

    @scala.annotation.tailrec
    def iter(bl: Block): Block = {
      interpretBlock(bl, ctx)
      interpretFlow(bl.flowInst, ctx)
      val next = cfg.nextBlock(bl)
      next match {
        case Some(x) => iter(x)
        case None => bl
      }
    }

    iter(cfg.block.head)
    ctx
  }

  private def interpretBlock(block: Block, ctx: x86Context): Unit = {
    @scala.annotation.tailrec
    def executeCodeOp(list: List[Inst]): Unit = {
      if (list.nonEmpty) {
        list.head match {
          case op: Push => fit(op, ctx)
          case op: BinaryOp => fit(op, ctx)
          case op: Store => fit(op, ctx)
          case op: Load => fit(op, ctx)
          case op: Cmp => fit(op, ctx)
          case op: Call => fit(op, ctx)
          case otherwise => error(s"command skipped: $otherwise")
        }
        executeCodeOp(list.tail)
      }
    }

    executeCodeOp(block.inst)
  }

  def fit(arg: Push, ctx: x86Context): Unit = {
    arg.num match {
      case tInt(op) => ctx.pushInst(x86Movl(RegBuff.allocate, op.toString))
      case otherwise => throw new RuntimeException(s"$otherwise: Type unsupported yet")
    }

  }

  def fit(arg: Cmp, ctx: x86Context): Unit = {
  }

  def fit(op: Call, ctx: x86Context): Unit = {

  }

  def fit(arg: BinaryOp, ctx: x86Context): Unit = {
    val x = RegBuff.pop
    val y = RegBuff.pop
    val inst = arg.op match {
      case "SUM" => x86Add(x, y)
      case "SUB" => x86Sub(x, y)
      case "PROD" => x86Prod(x, y)
      case "DIV" => x86Div(x, y)
      case otherwise => throw new RuntimeException(s"$otherwise: Undefine binary operator")
    }
    RegBuff.push(y)
    ctx.pushInst(inst)
  }

  def fit(arg: Store, ctx: x86Context): Unit = {
    if (!localSpace.contains(arg.name)) {
      val s = RegBuff.pop
      localSpace.push(arg.name)
      ctx.pushInst(x86Push(s))
    } else {
      val s = localSpace.getPos(arg.name)
      val r = RegBuff.pop
      ctx.pushInst(x86Movl("[" + RegAllocator.basePointer + "-" + s + "]", r))
    }
  }

  def fit(arg: Load, ctx: x86Context): Unit = {
    val s = localSpace.getPos(arg.name)
    val r = RegBuff.allocate
    ctx.pushInst(x86Movl(r, "[" + RegAllocator.basePointer + "-" + s + "]")) // --???---
  }

  def interpretFlow(arg: Inst, ctx: x86Context): Unit = {
    arg match {
      case _: Ret =>
        ctx.pushInst(x86Movl("rax", RegBuff.pop))
        ctx.pushInst(x86Popl("rbp"))
        freeStack(ctx)
        ctx.pushInst(x86Ret())
      case _ => ???
    }
  }

  def freeStack(ctx: x86Context): Unit = {
    val frame = localSpace.pop
    ctx.pushInst(x86Add(RegAllocator.stackPointer, frame.size.toString))
  }

}
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
