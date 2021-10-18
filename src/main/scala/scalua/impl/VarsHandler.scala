package scalua
package impl

private[impl] trait VarsHandler { self: LuaTranspiler =>
  import q.reflect.*
  
  treeHandlers(30) = {
    case vd @ ValDef(name, tpt, Some(rhs)) => varDef(vd, name, tpt.tpe, rhs, vd.symbol.flags.is(Flags.Private | Flags.PrivateLocal))

    case ValDef(_, _, None) =>
      LuaAst.NoTree // nothing to do with abstract members or non initialized fields in a class, since lua is dynamic

    case Assign(ident, value) => LuaAst.Assign(transform(ident).toString, transform(value))
  }

  protected def varDef(tree: Tree, name: String, tpe: TypeRepr, value: Tree, local: Boolean): LuaAst.LuaTree = {
    // println(s"$name: ${tpe.typeSymbol} = $value")
    TuplesArity.get(tpe.typeSymbol) match {
      case Some(arity) =>
        transform(value) match {
          case t: LuaAst.Tuple => LuaAst.Var(LuaAst.Assign(Vector.tabulate(arity)(i => LuaAst.Ident(s"${name}_${i + 1}")), t), local)
          case other => LuaAst.Var(LuaAst.Assign(Vector.tabulate(arity)(i => LuaAst.Ident(s"${name}_${i + 1}")), other.toExpr), local)
        }

      case _ => LuaAst.Var(name, transform(value).toExpr, local)
    }
  }
}
