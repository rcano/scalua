package scalua
package impl

private[impl] trait BlockHandler { self: LuaTranspiler =>
  import q.reflect.*
  treeHandlers(80) = { case Block(stats, expr) =>
    // in scala3, objects are declared as a ValDef+ClassDef combo in a block, so to process these, we have to scan a block to see if it contains
    // the combo and if so, we'll partition the block in parts to process it
    stats.sliding(2).indexWhere {
      case List(vd: ValDef, cd: ClassDef) if vd.tpt.tpe.typeSymbol == cd.symbol && cd.symbol.flags.is(Flags.Module) => true
      case _ => false
    } match {
      case -1 => // standard transformation
        LuaAst.Block(((stats :+ expr).iterator map transform flatMap { //flatten nested blocks
          case v: LuaAst.Block => v.stats
          case LuaAst.NoTree => Nil
          case other => List(other)
        }).toSeq)

      // found an object definition, split in parts based on where it was found
      case 0 =>
        val singleton = transform(Block(stats.take(2), Literal(UnitConstant()))).asInstanceOf[LuaAst.Singleton]
        transform(Block(stats.drop(2), expr)) match {
          case LuaAst.Block(stats) => LuaAst.Block(singleton +: stats)
          case other => LuaAst.Block(Seq(singleton, other))
        }

      case idx =>
        val pre = transform(Block(stats.take(idx), Literal(UnitConstant()))).asInstanceOf[LuaAst.Block]
        val singleton = transform(Block(stats.slice(idx, idx + 2), Literal(UnitConstant()))).asInstanceOf[LuaAst.Singleton]
        val post = transform(Block(stats.drop(idx + 2), expr)).asInstanceOf[LuaAst.Block]
        LuaAst.Block(pre.stats ++ Seq(singleton) ++ post.stats)

    }
  }
}
