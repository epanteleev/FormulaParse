package Compiler

class x86Context(var asmInst: List[x86Instruction]) {
  def pushInst(inst: x86Instruction):Unit = {
    asmInst = asmInst ++ List(inst)
  }
  val main = "main"

  override def toString: String = {
    s"\tSECTION .text\n\tglobal $main\n$main:\n" +
      asmInst.foldLeft(new String())((str: String, inst: x86Instruction) => s"$str\t${inst.toString}\n" )
  }
  //  def compilate(op: List[CodeOp]): String = {
  //    val code =  "\t.global main\n\t.text\nmain:\n" + createCodeSegment(op)
  //    createDataSegment + code
  //  }
}

object x86Context {
  def apply(): x86Context = new x86Context(List(x86Push(RegAllocator.basePointer), x86Movl(RegAllocator.basePointer, RegAllocator.stackPointer)))
}