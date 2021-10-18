package scalua
package impl

private[impl] trait ConstantsHandler { self: LuaTranspiler =>
  import q.reflect.*
  treeHandlers(0) = {
    case Inlined(call, binding, expr) =>
      // println(s"""|${Console.BLUE}$call${Console.RESET}
      //             |${Console.YELLOW}$binding${Console.RESET}
      //             |${Console.CYAN}$expr${Console.RESET}""".stripMargin)
      if (binding.isEmpty) transform(expr)
      else
        LuaAst.Block(binding.map(transform) :+ transform(expr))

    case Literal(UnitConstant()) => LuaAst.NoTree
    case Literal(const) => LuaAst.Constant(if (const.value == null) LuaAst.nil else const.value)
  }
}
