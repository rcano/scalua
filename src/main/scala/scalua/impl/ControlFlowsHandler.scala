package scalua
package impl

private[impl] trait ControlFlowsHandler { self: LuaTranspiler =>
  import q.reflect.*
  
  treeHandlers(40) = {
    case If(cond, thenBranch, elseBranch) => LuaAst.IfThenElse(transform(cond).toExpr, transform(thenBranch), transform(elseBranch))
    case While(cond, expr) => LuaAst.While(transform(cond).toExpr, transform(expr))

    case Import(prefix, selectors) => LuaAst.NoTree
  }
}
