package interpreter

import IR.MIR._
import IR._

import scala.collection.mutable

object Execute {
  def apply(input: String): Double = {
    val t = ConvertToMIR(Parse(input))
    println(t)
    new Interpret(t) execute
  }
}

private class Interpret(cfg: Graph) {

  lazy val cmpFunc: Map[String, (Double, Double) => Boolean] = Map(
    "EQ" -> ((a: Double, b: Double) => a == b),
    "EN" -> ((a: Double, b: Double) => a != b),
    "LS" -> ((a: Double, b: Double) => a < b),
    "GT" -> ((a: Double, b: Double) => a > b)
  )


  def execute: Double = {
    val Graph(_, list) = cfg;
    val currentBlock = list.head
    val ctx = Context()
    @scala.annotation.tailrec
    def iter(bl: Block):Block = {
      executeBlock(bl, ctx) match {
        case Some(x) if(x.id != bl.id) => iter(x)
        case None => bl
      }
    }
    iter(currentBlock)
    ctx.pop
  }

  private def executeBlock(block: Block, ctx: Context): Option[Block] = {
    var bl:Option[Block] = cfg.nextBlock(block)
    @scala.annotation.tailrec
    def executeCodeOp(list: List[CodeOp]): Unit = {
      if (list.nonEmpty) {
        list.head match {
          case op: Push => fit(op, ctx)
          case op: BinaryOp => fit(op, ctx)
          case op: Store => fit(op, ctx)
          case op: Load => fit(op, ctx)
          case op: Ret => fit(op, ctx)
          case op: Cmp => fit(op, ctx)
          case op: If => bl = fit(op, ctx, block)
          case op: Goto => bl = fit(op, ctx, block)
          case otherwise => error(s"command skipped: $otherwise")
        }
        executeCodeOp(list.tail)
      }
    }
    executeCodeOp(block.instruction)
    bl
  }

  def fit(arg: If, ctx: Context, block: Block): Option[Block] = {
    val label = if(ctx.popFlags) {
      arg.labelIf
    } else {
      arg.labelElse
    }
    cfg.findBlock(label)
  }

  def fit(arg: Goto, ctx: Context, block: Block): Option[Block] = {
    cfg.findBlock(arg.labelName)
  }

  def fit(arg: Push, ctx: Context): Unit = {
    ctx.push(arg.num)
  }

  def fit(arg: Cmp, ctx: Context): Unit = {
    val a = ctx.pop
    val b = ctx.pop
    ctx.pushFlags(cmpFunc(arg.cmp.toString)(a, b))
  }

  def fit(arg: BinaryOp, ctx: Context): Unit = {
    arg.op match {
      case "SUM" => ctx.push(ctx.pop + ctx.pop)
      case "SUB" => ctx.push(-(ctx.pop - ctx.pop))
      case "PROD" => ctx.push(ctx.pop * ctx.pop)
      case "DIV" => {
        val a = ctx.pop
        val b = ctx.pop
        ctx.push(b / a)
      }
      case "POW" => {
        val a = ctx.pop
        val b = ctx.pop
        ctx.push(math.pow(b, a))
      }
    }
  }

  def fit(arg: Store, ctx: Context): Unit = ctx.registerVariable(arg.name, ctx.pop)

  def fit(arg: Load, ctx: Context): Unit = ctx.push(ctx.loadVariable(arg.name))

  def fit(arg: Ret, ctx: Context): Unit = () //ctx.push(ctx.loadVariable(arg.name))
}

object Interpret {

}