package Compiler

class LocalSpace {
  private var stack: List[String] =  List[String]()
  def push(a: String): Unit = {
    stack =  stack ++ List(a)
  }

  def size: Int = stack.size * 8

  def getPos(nameVar: String): Int = {
    @scala.annotation.tailrec
    def iter(s: List[String], it: Int): Int = {
      s match {
        case a::list  => if(a == nameVar)  it else iter(list, it + 4)
        case Nil => -1
      }
    }

    iter(stack, 8)
  }

  def foreach(f:String => Unit):Unit = stack.foreach(f)

  def map[A](f:String => A):List[A] = stack.map(f)

  def contains(nameVar: String): Boolean = stack.contains(nameVar)
}

object LocalSpace {
  def apply(): LocalSpace = new LocalSpace()
}

class StackFrame {
  private var stackLocalSpaces: List[LocalSpace] = List[LocalSpace](LocalSpace())

  def newFrame(): Unit = stackLocalSpaces = LocalSpace() :: stackLocalSpaces

  def pop: LocalSpace = {
    if (stackLocalSpaces.isEmpty) throw new Error("StackFrame is empty")
    val ret = stackLocalSpaces.head
    stackLocalSpaces = stackLocalSpaces.tail
    ret
  }

  def push(a: String): Unit = stackLocalSpaces.head.push(a)

  def getPos(nameVar: String): Int = {

    @scala.annotation.tailrec
    def iter(s: List[LocalSpace], it: Int): Int = {
      s match {
        case e::list => {
          val ret = e.getPos(nameVar)
          if (ret != -1) ret + list.map(x => x.size).sum
          else iter(list, it - e.size)
        }
        case Nil => -1
      }
    }
    iter(stackLocalSpaces, allSize)
  }

  def contains(nameVar: String): Boolean = {
    @scala.annotation.tailrec
    def iter(s: List[LocalSpace]): Boolean = {
      s match {
        case el :: list =>
          if (el.contains(nameVar)) true
          else iter(list)
        case Nil => false
      }
    }
    iter(stackLocalSpaces)
  }

  def allSize:Int = {
    def iter(it: Int, list: List[LocalSpace]): Int = {
      list match {
        case a::l => iter(a.size + it, l)
        case Nil => it
      }
    }
    iter(0, stackLocalSpaces)
  }
  def align: Int = stackLocalSpaces.head.size
}

object StackFrame {
  def apply(): StackFrame = new StackFrame()
}
