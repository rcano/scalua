package scalua
package impl

import Predef.*

private[impl] trait TemplatesHandler { self: LuaTranspiler =>
  import q.reflect.*
  
  treeHandlers(60) = {
    case cd: ClassDef =>
      val flatArgs = cd.constructor.termParamss.flatMap(_.params)
      val memberArgs = flatArgs
        .filter(!_.symbol.flags.is(Flags.Local))
        .flatMap(v => functionArguments(Seq(v)).map(v => LuaAst.Var(v, LuaAst.Ref(None, v), false)))
      val clazz = getRenamed(cd).getOrElse(cd.name)
      val prefix = Some(cd.symbol.name.split("\\.").init.mkString(".")).filter(_.nonEmpty)
      val fqdn = cd.symbol.fullName
      val body = transform(Block(cd.body, Literal(UnitConstant()))) match
        case LuaAst.Block(sts) => LuaAst.Block(memberArgs ++ sts)
        case other => LuaAst.Block(memberArgs :+ other)
      LuaAst.Class(prefix, clazz, fqdn, flatArgs.map(_.name), body, cd.symbol.flags.is(Flags.Private))

    // object singletons
    case Block(List(vd: ValDef, cd: ClassDef), Literal(UnitConstant()))
        if vd.tpt.tpe.typeSymbol == cd.symbol && cd.symbol.flags.is(Flags.Module) =>
      val clazz = luaModuleName(getRenamed(cd).getOrElse(vd.name))
      val prefix = Some(cd.symbol.name.split("\\.").init.mkString(".")).filter(_.nonEmpty)
      val fqdn = cd.symbol.fullName
      val body = transform(Block(cd.body, Literal(UnitConstant()))) match
        case b: LuaAst.Block => b
        case other => LuaAst.Block(List(other))
      LuaAst.Singleton(prefix, clazz, fqdn, body, cd.symbol.flags.is(Flags.Private))
  }
}
