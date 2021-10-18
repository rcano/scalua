package scalua
package impl

import Predef.*

private[impl] trait RefsHandler { self: LuaTranspiler =>
  import q.reflect.*

  treeHandlers(10) = {
    case Select(pref, field) if defn.isTupleClass(pref.symbol.maybeOwner) && field.matches("_\\d+") =>
      pref match {
        case Select(pref, t) => LuaAst.Ref(Some(transform(pref).toExpr), t + field)
        case Ident(t) => LuaAst.Ref(None, t + field)
      }

    case i @ Ident(name) =>
      if (i.tpe <:< TypeRepr.of[LuaAst.LuaTree] && !(i.tpe =:= TypeRepr.of[Nothing])) unsplicedLuaTree(i)
      else {
        if (i.symbol.hasAnnotation(globalAnnotSym)) LuaAst.NoTree
        else if (defn.isTupleClass(i.tpe.typeSymbol)) {
          LuaAst.Tuple(Vector.tabulate(TuplesArity(i.tpe.typeSymbol))(i => LuaAst.Ref(None, s"${name}_${i + 1}")))
        } else
          getRenamed(i) match {
            case Some(s) => LuaAst.Ref(None, s)
            case _ => LuaAst.Ref(None, if (shouldTreatAsModule(i)) luaModuleName(name) else name)
          }
      }
    // case tt: TypeTree => transform(tt.original)
    case tree @ Select(This(clazz), select) =>
      if (tree.tpe <:< TypeRepr.of[LuaAst.LuaTree] && !(tree.tpe =:= TypeRepr.of[Nothing])) unsplicedLuaTree(tree)
      else {
        if (tree.symbol.flags.is(Flags.ParamAccessor) && !tree.symbol.flags.is(Flags.FieldAccessor | Flags.CaseAccessor))
          LuaAst.Ref(None, select) //a param accessor does not need self, since its contextualized to the function
        else LuaAst.Ref(Some(transform(tree.qualifier).toExpr), select)
      }

    case Select(New(typeRef), "<init>") =>
      val className =
        if (typeRef.symbol.flags.is(Flags.Module)) luaModuleName(typeRef.symbol.name)
        else typeRef.symbol.name
      val qual = LuaAst.Ref(None, className)
      LuaAst.Ref(Some(qual), "new")

    case tree @ Select(qual, select) =>
      if (tree.tpe <:< TypeRepr.of[LuaAst.LuaTree] && !(tree.tpe =:= TypeRepr.of[Nothing])) unsplicedLuaTree(tree)
      else {
        val selected = getRenamed(tree) match {
          case Some(s) => s
          case _ => if (shouldTreatAsModule(tree)) luaModuleName(select) else select
        }
        if (selected == "unary_!") LuaAst.UnaryOperation("not ", transform(qual).toExpr)
        else if (qual.symbol.hasAnnotation(globalAnnotSym)) LuaAst.Ref(None, selected)
        else if (defn.isTupleClass(tree.tpe.typeSymbol)) {
          val prefix = Option(transform(qual).toExpr).filterNot(_ == LuaAst.NoTree)
          LuaAst.Tuple(Vector.tabulate(TuplesArity(tree.tpe.typeSymbol))(i => LuaAst.Ref(prefix, s"${select}_${i + 1}")))
        } else if (defn.isTupleClass(tree.symbol.maybeOwner) && select.matches("_\\d+")) {
          transform(qual).asInstanceOf[LuaAst.Tuple].values(select.drop(1).toInt - 1)
        } else {
          // must carefully detect references to no arg methods, because those are still generated as methods in the classes and need to be called like so
          val ref = LuaAst.Ref(Option(transform(qual).toExpr).filterNot(_ == LuaAst.NoTree), selected)
          if (tree.symbol.isDefDef && tree.symbol.paramSymss.isEmpty && !tree.symbol.maybeOwner.flags.is(Flags.Module))
            LuaAst.Dispatch(ref, Seq())
          else ref
        }
      }

    case This(clazz) => LuaAst.Ref(None, "self")
    case NamedArg(_, expr) => transform(expr)
  }

  private def shouldTreatAsModule(tree: Term) =
    tree.symbol.flags.is(Flags.Module) || (tree.symbol.flags.is(Flags.Implicit) && tree.symbol.isType && tree.tpe <:< TypeRepr.of[AnyVal])

  private def unsplicedLuaTree(tree: Tree) = {
    report.error("LuaTree must be spliced using the method splice.", tree.pos)
    LuaAst.NoTree
  }
}
