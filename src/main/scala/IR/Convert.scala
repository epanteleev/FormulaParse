package IR

import IR.MIR._


object Convert {

  def apply(exp: List[Expression]): Graph = {
    val graph = Graph()
    convert(exp, graph)
    graph
  }

  def walkIfThen(exp: IfThen, cfg: Graph): Unit = {
    /*
         CMP [op]
         IF L0 L1
          --else code--
         GOTO L1
         L0:
          --if block--
         L1:
       */
    val bl = cfg.lastBlock
    val (ifBl, last) = (Block(), Block())
    val cond = exp.condition
    if (exp.elseBlock.isEmpty) {
      walkCondition(cond, cond.op.not, bl)
      bl.flowInst = If(last.id, ifBl.id)
      cfg.insert(ifBl)
      convert(exp.ifBlock, cfg)
      cfg.insert(last)
    } else {
      val elseBl = Block()
      walkCondition(exp.condition, cond.op, bl)
      bl.flowInst = If(ifBl.id, last.id)
      cfg.insert(elseBl)
      convert(exp.elseBlock, cfg)

      cfg.insert(ifBl)
      convert(exp.ifBlock, cfg)
      elseBl.flowInst = Goto(last.id)
      cfg.insert(last)
    }
  }

  def walkLoop(exp: Loop, cfg: Graph): Unit = {
    val (cmpBlock, loopBlock, last) = (Block(), Block(), Block())
    val cond = exp.condition
    walkCondition(cond, cond.op.not, cmpBlock)
    cmpBlock.flowInst = If(last.id, loopBlock.id)
    cfg.insert(cmpBlock)

    cfg.insert(loopBlock)
    convert(exp.expr, cfg)
    loopBlock.flowInst = Goto(cmpBlock.id)
    cfg.insert(last)
  }

  def walkCondition(cond: Condition, cmp: CmpOp, block: Block): Unit = {
    walk(cond.left, block)
    walk(cond.right, block)
    block.pushInst(Cmp(cmp))
  }

  def convert(exp: List[Expression], cfg: Graph): Unit = {
    for (el <- exp) {
      val currentBlock = cfg.lastBlock
      el match {
        case fl: Loop => walkLoop(fl, cfg)
        case fl: IfThen => walkIfThen(fl, cfg)
        case _ => el.foreach((e: Expression) => walk(e, currentBlock))
      }
    }
  }

  private def walk(exp: Expression, block: Block): Unit = {
    exp match {
      case Number(typeiId) => block.pushInst(Push(typeiId))
      case _: Sum => block.pushInst(BinaryOp("SUM"))
      case _: Sub => block.pushInst(BinaryOp("SUB"))
      case _: Prod => block.pushInst(BinaryOp("PROD"))
      case _: Div => block.pushInst(BinaryOp("DIV"))
      case ld: Variable => block.pushInst(Load(ld.name))
      case _: Not => block.pushInst(UnaryOp("NOT"))
      case a: Assignment => walkAssignment(a, block)
      case f: CallFunction => block.pushInst(Call(f.name))
      case _: Return => block.flowInst = Ret()
      case op => throw new RuntimeException(s"$op. Match error")
    }
  }
  def walkAssignment(exp: Assignment, block: Block): Unit = {
    exp match {
      case l: Let =>
        block.pushInst(Store(l.name))
        block.addDef(l.name)
      case r: ReDef =>
        block.pushInst(Store(r.name))
        if(block.notDef(r.name))
          throw new RuntimeException(s"undefine symbol ${r.name}. Abort")
    }
  }
}
