package IR

import IR.MIR.{BinaryOp, Block, Call, Cmp, CmpOp, Edges, Goto, Graph, If, Load, Push, Ret, Store, UnaryOp}


object ConvertToMIR {
  private val cfg: Graph = Graph()


  def apply(exp: List[Expression]): Graph = {
    convert(exp)
    cfg
  }

  def mkBlock(exp: Flow): Unit = {
    val bl = cfg.lastBlock
    exp match {
      case iif: IfThen => {
        /*
          CMP [op]
          IF L0 L1
           --else code--
          GOTO L1
          L0:
           --if block--
          L1:
        */
        val (ifBl, last) = (Block(), Block())
        val cond = exp.condition
        if (iif.elseBlock.isEmpty) {
          walkCondition(cond, cond.op.not, bl)
          bl.pushInst(If(last.id, ifBl.id))
          cfg.insert(ifBl)
          convert(iif.ifBlock)
          cfg.insert(last)
        } else {
          val elseBl = Block()
          walkCondition(exp.condition, cond.op, bl)
          bl.pushInst(If(ifBl.id, last.id))
          cfg.insert(elseBl)
          convert(iif.elseBlock)

          cfg.insert(ifBl)
          convert(iif.ifBlock)
          elseBl.pushInst(Goto(last.id))
          cfg.insert(last)
        }
      }
      case loop: Loop => {
        /**
         * L0:
         *  --args--
         *  CMP [op]
         *  IF L1
         * L2:
         *   --loop block--
         *   GOTO L0
         * L1:
         *
         */
        val (cmpBlock, last, loopBlock) = (Block(), Block(), Block())
        val cond = loop.condition
        walkCondition(cond, cond.op.not, cmpBlock)
        cmpBlock.pushInst(If(last.id, loopBlock.id))
        cfg.insert(cmpBlock)

        cfg.insert(loopBlock)
        convert(loop.expr)
        loopBlock.pushInst(Goto(cmpBlock.id))
        cfg.insert(last)
      }
      case _ => throw new RuntimeException("Match error")
    }
  }

  def walkCondition(cond: Condition, cmp: CmpOp, block: Block): Unit = {
    walk(cond.left, block)
    walk(cond.right, block)
    block.pushInst(Cmp(cmp))
  }

  def convert(exp: List[Expression]): Unit = {
    for (el <- exp) {
      val currentBlock = cfg.lastBlock
      el match {
        case fl: Flow => mkBlock(fl)
        case _ => el.foreach((e: Expression) => walk(e, currentBlock))
      }
    }
  }

  private def walk(exp: Expression, block: Block): Unit = {
    exp match {
      case Number(double, _) => block.pushInst(Push(double))
      case _: Sum => block.pushInst(BinaryOp("SUM"))
      case _: Sub => block.pushInst(BinaryOp("SUB"))
      case _: Prod => block.pushInst(BinaryOp("PROD"))
      case _: Div => block.pushInst(BinaryOp("DIV"))
      case ld: Variable => block.pushInst(Load(ld.name))
      case _: Not => block.pushInst(UnaryOp("NOT"))
      case a: Assignment => block.pushInst(Store(a.name))
      case f: CallFunction => {
        if(f.name == "pow") block.pushInst(BinaryOp("POW"))
        else block.pushInst(Call(f.name))
      }
      case _:Return => block.pushInst(Ret())
      case _ => throw new RuntimeException("Match error")
    }
  }

}