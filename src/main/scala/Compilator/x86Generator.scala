package Compilator


class x86Generator extends Generator{
  val x86regs = List("%ebx", "%ecx", "%esi", "%edi", "%eax", "%edx")

  var codeSegment: String = "\t.global main\n\t.text\nmain:\n"
  var dataSegment: String = "\t.data\n"
  var env: List[String] = List[String]()

  def SUM(): Unit = codeSegment = codeSegment ++ "\tpopl %ebx \n\tpopl %ecx \n\taddl %ebx, %ecx \n\tpushl %ecx\n"
  def PUSH(i: Int): Unit = codeSegment = codeSegment ++ s"\tpushl $$$i\n"

  def STORE(name: String): Unit = {
    if(! env.contains(name)){
      dataSegment = dataSegment + s"$name:\t.int\n"
    }
    codeSegment = codeSegment ++ s"\tpopl %ebx\n\tmovl %ebx, $name\n"
    env = env ++ List(name)
  }

  def LOAD(name: String): Unit = codeSegment = codeSegment ++ s"\tmovl $name, %ebx \n\tpushl %ebx\n"

  def RET: Unit = codeSegment = codeSegment ++ s"\tpopl %eax\n"

  def get: String = dataSegment + "\n" + codeSegment
  //def Move(des: String): Unit = codeSegment = codeSegment ++ s"pushl $des\n"
}

