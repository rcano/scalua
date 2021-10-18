package scalua
package impl

import Predef.*

private[impl] trait PatMatchHandler { self: LuaTranspiler =>
  import q.reflect.*
  
  treeHandlers(70) = {
    case Match(expr, cases) => patternMatch(expr, cases)
  }

  private def patternMatch(expr: Tree, cases: Seq[CaseDef]): LuaAst.LuaTree = {
    val No = LuaAst.NoTree

    /** helper method to parse cases. Each pattern receives the expression over which it operates, and returns a LuaTree that represents the
      * values bound the LuaTree representing the condition to continue with this branch, and an optional LuaTree that would be setup for
      * the condition testing.
      */
    def pattern(expr: LuaAst.LuaExpr, pat: Tree): (Option[LuaAst.LuaTree], LuaAst.LuaTree) = pat match {
      case Wildcard() => (None, LuaAst.Constant(true))

      case l: Literal => (None, LuaAst.InfixOperation(expr, "==", transform(l).toExpr))
      case r: Ref => (None, LuaAst.InfixOperation(expr, "==", transform(r).toExpr))

      case Bind(name, pat) =>
        val (binds, cond) = pattern(expr, pat)
        (Some(LuaAst.Block(Seq(LuaAst.Var(name, expr, true)).++(binds).filterNot(No.==))), cond)

      case Typed(Wildcard(), tpt) => (None, isInstanceOfCheck(expr, tpt))

      case Alternatives(List(first, rest*)) =>
        val pats = (pattern(expr, first) +: rest.map(pattern(expr, _))) map (_._2)
        (None, pats.reduceLeft((a, b) => LuaAst.InfixOperation(a.toExpr, "or", b.toExpr)))

      case TypedOrTest(p1, _) => pattern(expr, p1)

      case Unapply(ref, implicits, pats) =>
        val patsWithName = pats.map(p => p -> nextName)
        val unapplyExpr = LuaAst.Dispatch(transform(ref).asInstanceOf[LuaAst.Ref], Seq(expr))
        val unapplyOpt = LuaAst.Var(LuaAst.Assign(nextName, unapplyExpr), true)
        //declare the extracted variables
        val unapplyTuple = LuaAst.Var(LuaAst.Assign(patsWithName.map(e => e._2), No), true)
        val unapplyTupleInitialize = LuaAst.Block(
          Seq(
            LuaAst.IfThenElse(
              cond = LuaAst.Invoke(LuaAst.Ref(Some(LuaAst.Ref(None, unapplyOpt.assign.names.head)), "isDefined"), Seq.empty),
              thenBranch = LuaAst.Var(
                LuaAst.Assign(
                  patsWithName.map(e => e._2),
                  LuaAst.Invoke(LuaAst.Ref(Some(LuaAst.Ref(None, unapplyOpt.assign.names.head)), "get"), Seq.empty)
                ),
                true
              ),
              elseBranch = LuaAst.Constant(LuaAst.nil)
            )
          )
        )
        val (binds, cond) = patsWithName
          .map(t => pattern(LuaAst.Ref(None, t._2), t._1))
          .foldLeft[(Seq[LuaAst.LuaTree], LuaAst.LuaTree)]((Vector.empty, No)) {
            case ((bindsAggr, No), (binds, cond)) => (bindsAggr ++ binds, cond)
            case ((bindsAggr, res), (binds, cond)) => (bindsAggr ++ binds, LuaAst.InfixOperation(res.toExpr, "and", cond.toExpr))
          }
        (Some(LuaAst.Block(unapplyOpt +: unapplyTuple +: unapplyTupleInitialize +: binds.filterNot(No.==))), cond)

      case other => report.errorAndAbort(s"Unsupported pattern $pat", pat.pos)
    }

    // FIXME: the expr should be captured to a local variable to prevent reevaluating it on each branch
    val capturedExpr = LuaAst.Var(nextName, transform(expr), true)
    val luaExpr = LuaAst.Ref(None, capturedExpr._1.names.head)
    val luaCases = cases
      .map { _case =>
        val (binds, pat) = pattern(luaExpr, _case.pattern)
        // println("guard: " + _case.guard)
        val guard = _case.guard.map(transform(_)) getOrElse No
        val cond = if (guard != No) LuaAst.InfixOperation(pat.toExpr, "and", guard.toExpr) else pat
        (binds, cond, transform(_case.rhs))
      }
      .foldRight[LuaAst.LuaTree](LuaAst.Constant(LuaAst.nil)) {
        case ((None, cond, thenBranch), elseBranch) => LuaAst.IfThenElse(cond.toExpr, thenBranch, elseBranch)
        case ((Some(setup), cond, thenBranch), elseBranch) =>
          LuaAst.Block(Seq(setup, LuaAst.IfThenElse(cond.toExpr, thenBranch, elseBranch)))
      }
    LuaAst.Block(List(capturedExpr, luaCases))
  }
}
