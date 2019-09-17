package Calculator

object NameGen {
  var id:Int = -1
  def reset(): Unit = {id = -1}
  def apply(tok: String = "L"): String = {
    id  = id + 1
    tok + (id toString)
  }
}
