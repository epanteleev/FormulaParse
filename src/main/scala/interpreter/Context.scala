package interpreter
import scala.collection.mutable

class Context {
  private val stack: mutable.Stack[Double] = mutable.Stack[Double]()
  private val flags: mutable.Stack[Boolean] = mutable.Stack[Boolean]()
  private val vars: mutable.HashMap[String, Double] = mutable.HashMap[String, Double]()

  def pop: Double = stack.pop()

  def popFlags: Boolean = flags.pop()
  def push(double: Double):Unit = stack.push(double)
  def pushFlags(flag: Boolean): Unit = flags.push(flag)

  def registerVariable(name: String, double: Double): Option[Double] = vars.put(name, double)

  def loadVariable(id: String): Double = {
    vars.get(id) match {
      case Some(double) => double
      case None => throw new Error("variable wasn't load")
    }
  }
}

object Context {
  def apply(): Context = new Context()
}

