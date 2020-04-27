package interpreter

import IR.MIR._
import IR.Type.{tDouble, tInt}
import IR._

object Interpret {
  def apply(input: String): Double = {
    val p = Parse(input)
    println(p.foldLeft(new String)((s: String, e: Expression) => s"$s\n${e.toString}"))
    val t = Convert(p)
    println(t)
    new Interpret(t).interpretGraph.pop
  }
}

private class Interpret(cfg: Graph) {

  lazy val cmpFunc: Map[String, (Double, Double) => Boolean] = Map(
    "EQ" -> ((a: Double, b: Double) => a == b),
    "NE" -> ((a: Double, b: Double) => a != b),
    "LS" -> ((a: Double, b: Double) => a < b),
    "GT" -> ((a: Double, b: Double) => a > b)
  )


  def interpretGraph: Context = {
    val list = cfg.block;
    val currentBlock = list.head
    val ctx = Context()

    @scala.annotation.tailrec
    def iter(bl: Block): Block = {
      interpretBlock(bl, ctx)
      val next = switchBlock(bl.flowInst, ctx, bl)
      next match {
        case Some(x) => iter(x)
        case None => bl
      }
    }

    iter(currentBlock)
    ctx
  }

  def switchBlock(flow: Inst, ctx: Context, block: Block): Option[Block] = {
    flow match {
      case op: If => fit(op, ctx, block) match {
        case Some(x) => Some(x)
        case None => throw new RuntimeException("Block wasn't found")
      }
      case op: Goto => fit(op, ctx, block) match {
        case Some(x) => Some(x)
        case None => throw new RuntimeException("Block wasn't found")
      }
      case op: Ret => None
      case _ => cfg.nextBlock(block) match {
        case Some(op) => Some(op)
        case None => throw new RuntimeException("Block wasn't found")
      }
    }
  }

  def fit(arg: If, ctx: Context, block: Block): Option[Block] = {
    val label = if (ctx.popFlags) {
      arg.labelIf
    } else {
      arg.labelElse
    }
    cfg.findBlock(label)
  }

  private def interpretBlock(block: Block, ctx: Context): Unit = {
    @scala.annotation.tailrec
    def executeCodeOp(list: List[Inst]): Unit = {
      if (list.nonEmpty) {
        list.head match {
          case op: Push => fit(op, ctx)
          case op: BinaryOp => fit(op, ctx)
          case op: Store => fit(op, ctx)
          case op: Load => fit(op, ctx)
          case op: Ret => fit(op, ctx)
          case op: Cmp => fit(op, ctx)
          case op: Call => fit(op, ctx)
          case otherwise => error(s"command skipped: $otherwise")
        }
        executeCodeOp(list.tail)
      }
    }

    executeCodeOp(block.inst)
  }

  def fit(op: Call, ctx: Context): Unit = {
    if (op.name == "pow") {
      val a = ctx.pop
      val b = ctx.pop
      ctx.push(math.pow(b, a))
    } else {
      throw new RuntimeException(s"$op: Undefine function")
    }
  }

  def fit(arg: Goto, ctx: Context, block: Block): Option[Block] = {
    cfg.findBlock(arg.labelName)
  }

  def fit(arg: Push, ctx: Context): Unit = {
    arg.num match {
      case tDouble(op) => ctx.push(op)
      case tInt(op) => ctx.push(op.toDouble)
      case otherwise => throw new RuntimeException(s"$otherwise: Type unsupported yet")
    }

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
      case "DIV" =>
        val a = ctx.pop
        ctx.push(ctx.pop / a)
      case otherwise => throw new RuntimeException(s"$otherwise: Undefine binary operator")
    }
  }

  def fit(arg: Store, ctx: Context): Unit = ctx.registerVariable(arg.name, ctx.pop)

  def fit(arg: Load, ctx: Context): Unit = ctx.push(ctx.loadVariable(arg.name))

  def fit(arg: Ret, ctx: Context): Unit = () //ctx.push(ctx.loadVariable(arg.name))
}
