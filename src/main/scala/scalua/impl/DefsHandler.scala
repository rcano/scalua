package scalua
package impl

private[impl] trait DefsHandler { self: LuaTranspiler =>
  import q.reflect.*
  
  treeHandlers(50) = {
    case _: TypeDef => LuaAst.NoTree // no types in lua

    // special case case class generated hashCode method to discard it. Lua doesn't do hashcode, they hardcore
    case dd @ DefDef("hashCode", _, retTpe, _) if dd.symbol.flags.is(Flags.Synthetic) && (retTpe.tpe <:< TypeRepr.of[Int]) =>
      LuaAst.NoTree
    case dd @ DefDef("toString", _, retTpe, _) if dd.symbol.flags.is(Flags.Synthetic) && (retTpe.tpe <:< TypeRepr.of[String]) =>
      LuaAst.NoTree
    case dd @ DefDef("canEqual", _, retTpe, _) if dd.symbol.flags.is(Flags.Synthetic) && (retTpe.tpe <:< TypeRepr.of[Boolean]) =>
      LuaAst.NoTree

    case defdef: DefDef if defdef.rhs.isDefined =>
      val variableName =
        if defdef.symbol.flags.is(Flags.Implicit & Flags.Synthetic) && defdef.returnTpt.symbol.flags.is(Flags.Implicit) then
          s"${defdef.name}Implicit"
        else defdef.name

      val bodyNode =
        if defdef.returnTpt.tpe.typeSymbol == defn.UnitClass then LuaAst.Block(Seq(transform(defdef.rhs.get)) :+ LuaAst.NoTree)
        else transform(defdef.rhs.get)

      LuaAst.Var(
        variableName,
        LuaAst.Function(functionArguments(defdef.termParamss.flatMap(_.params)), bodyNode),
        defdef.symbol.flags.is(Flags.Private)
      )

    
    //lambdas are represented by a Block(List(DefDef), Closure)
    case Block(List(d: DefDef), Closure(sym, tpe)) => transform(d).asInstanceOf[LuaAst.Var].assign.value
  }
}
