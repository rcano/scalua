package scalua
package impl

import Predef.*

private[impl] trait FunctionsHandler { self: LuaTranspiler =>
  import q.reflect.*
  
  treeHandlers(20) = {
    case TypeApply(Select(pref, "asInstanceOf" | "$asInstanceOf$"), _) => transform(pref) //casting has no meaning to lua
    case TypeApply(m @ Select(pref, "isInstanceOf"), tpt) if m.symbol.maybeOwner == defn.AnyClass =>
      isInstanceOfCheck(transform(pref).toExpr, tpt.head)
    case TypeApply(pref, _) => transform(pref) //types have no meaning to lua
    case Typed(prefix, _) => transform(prefix) //abscribing has no meaning to lua

    case tree @ Apply(on, args) =>
      def flatten(term: Term, argss: List[List[Term]]): (Term, List[List[Term]]) = term match {
        case Apply(pref, args) => flatten(pref, args :: argss)
        case term => (term, argss)
      }
      val (func, rawArgss) = flatten(on, args :: Nil)
      val argss = rawArgss.map(_.flatMap {
        case Repeated(args, _) => args
        case Typed(Repeated(args, _), _) => args
        case other => List(other)
      })
      println(s"""|$func
                  |$argss
                  |${func.symbol} - ${func.symbol.owner}
                  |===================================""".stripMargin)

      //TODO document the logic of this method, for it is quite large

      val methodName = func.symbol.name
      lazy val funcOwnerTpe = func.symbol.owner.tree.asInstanceOf[TypeTree].tpe
      // only use this when you know the function has a prefix
      lazy val Select(funcPrefix, _) = func

      //attempt to identify default arguments passed
      val trueArgs = func.symbol.paramSymss
        .zip(rawArgss)
        .map(t =>
          t._1 zip t._2 filter {
            case (param, arg) if param.flags.is(Flags.HasDefault) && arg.toString.contains(s"$methodName$$default$$") => false
            case _ => true
          }
        )

      lazy val implicitClassConversionOpt = ImplicitClassConversion.unapply(func)

      // special handling or LuaStdLib methods
      if (func.symbol.owner == TypeRepr.of[LuaStdLib.type].typeSymbol) {
        methodName match {
          case "require" =>
            val arg = transform(argss.head.head) match
              case LuaAst.Constant(v) => s"\"$v\""
              case other => other.toString
            LuaAst.LuaInlined(s"require ${arg}")
          case "cfor" =>
            //need to identify which version of for this is
            val Seq(from, to, step) = argss.head
            val (params, body) = transform(argss.tail.head.head) match {
              case LuaAst.Function(params, body) => (params, body)
              case LuaAst.Block(Seq(LuaAst.Function(params, body))) => (params, body)
            }: @unchecked
            LuaAst.For(
              params.head,
              transform(from).toExpr,
              transform(to).toExpr,
              if (trueArgs.head.size < argss.head.size) LuaAst.Constant(1) else transform(step),
              body
            )
          case "List" =>
            LuaAst.MapLiteral(argss.head.zipWithIndex.map { case (arg, i) => (LuaAst.Constant(i + 1), transform(arg)) })
          case "splice" =>
            LuaAst.StagedNode(argss.head.head.asExprOf[LuaAst.LuaTree])
          case "inlineLua" =>
            LuaAst.LuaInlined(argss.head.head.asExprOf[String].valueOrAbort)
          case _ =>
            LuaAst.Invoke(transform(func).asInstanceOf[LuaAst.Ref], argss.flatten map transform)
        }

      } else if (func.symbol.overridingSymbol(TypeRepr.of[LuaStdLib.IterateApply[_, _, _]].typeSymbol) != Symbol.noSymbol) {
        val LuaAst.Invoke(_, Seq(iterator, state, init, _)) = transform(funcPrefix)
        val (params, body) = transform(argss.head.head) match {
          case LuaAst.Function(params, body) => (params, body)
          case LuaAst.Block(Seq(LuaAst.Function(params, body))) => (params, body)
        }: @unchecked
        LuaAst.Iterate(
          params,
          iterator,
          if (!state.toString.contains("$default$")) Some(state) else None,
          if (!init.toString.contains("$default$")) Some(init) else None,
          body
        )

      } else if (func.symbol.overridingSymbol(TypeRepr.of[LuaStdLib.Map[_, _]].typeSymbol) != Symbol.noSymbol) {
        methodName match {
          case "apply" => LuaAst.Ref(None, transform(funcPrefix).toString + "[" + transform(argss.head.head) + "]")
          case "update" =>
            LuaAst.Var(transform(funcPrefix).toString + "[" + transform(argss.head.head) + "]", transform(argss.head.tail.head), false)
          case "size" => LuaAst.UnaryOperation("#", transform(funcPrefix).toExpr)
        }

      } else if (func.symbol.owner == TypeRepr.of[LuaStdLib.Map.type].typeSymbol) {
        LuaAst.MapLiteral(argss.flatten.map {
          case AsExpr('{ ($a: a) -> $b }) => (transform(a.asTerm), transform(b.asTerm))
          case AsExpr('{ ($a, $b) }) => (transform(a.asTerm), transform(b.asTerm))
        })

      } else if (func.symbol.owner == defn.StringClass) {
        if (methodName == "+") LuaAst.InfixOperation(transform(funcPrefix).toExpr, "..", transform(argss.head.head).toExpr)
        else if (methodName == "length") LuaAst.UnaryOperation("#", transform(funcPrefix).toExpr)
        else {
          report.error(s"Unsupported String api $func", tree.pos)
          LuaAst.NoTree
        }

      } else if (func.symbol.owner == defn.BooleanClass && methodName.matches(raw"&&|\|\|")) {
        LuaAst.InfixOperation(transform(funcPrefix).toExpr, if methodName == "&&" then "and" else "or", transform(argss.head.head).toExpr)
      } else if (
        (func.symbol.owner == defn.AnyRefClass && methodName == "eq") ||
        (func.symbol.owner == defn.ObjectClass && methodName == "eq") || (func.symbol.owner == defn.AnyClass && methodName == "equals")
      ) {
        LuaAst.InfixOperation(transform(funcPrefix).toExpr, "==", transform(argss.head.head).toExpr)
      } else if (
        (func.symbol.owner == defn.AnyRefClass && methodName == "ne") || (func.symbol.owner == defn.ObjectClass && methodName == "ne")
      ) {
        LuaAst.InfixOperation(transform(funcPrefix).toExpr, "~=", transform(argss.head.head).toExpr)
      } else if (methodName == "toString" && func.symbol.flags.is(Flags.Override)) {
        LuaAst.Dispatch(LuaAst.Ref(Some(transform(funcPrefix).toExpr), "__tostring"), Seq())
      } else if (TupleFactories(func.symbol)) {
        LuaAst.Tuple(argss.head.map(transform))

        // detect if getting a value from xxl tuples
      } else if (func.symbol == XxlTupleApply) {
        report.errorAndAbort("XXL tuples are not supported yet", tree.pos)
        val tuple :: idx :: Nil = argss.head // grab the number fom the method that looks like Tuples.apply(nonEmptyTuple, idx)
        idx.asExprOf[Int].value match {
          case None =>
            report.error("Unkown index of tuple", tree.pos)
            LuaAst.NoTree
          case Some(idx) =>
            transform(tuple) //.asInstanceOf[LuaAst.Tuple].values(idx)
        }
        //extension methods via value classes
      } else if (
        func.symbol.owner.flags.is(Flags.Implicit) &&
        funcOwnerTpe <:< TypeRepr.of[AnyVal] &&
        implicitClassConversionOpt.isDefined
      ) {
        val (implicitFunc, arg) = implicitClassConversionOpt.get
        val implicitClassType = implicitFunc.symbol.tree.asInstanceOf[DefDef].returnTpt

        val tripleQuestionMark = Ref(defn.PredefModule).select(defn.PredefModule.methodMember("???").head)
        val newPrefix = Select(Typed(tripleQuestionMark, implicitClassType), func.symbol)
        val transformed = transform(newPrefix.appliedToArgss(argss)).asInstanceOf[LuaAst.Dispatch]

        LuaAst.Dispatch(LuaAst.Ref(Some(transform(arg).toExpr), transformed.sym.name), transformed.args)
      } else {
        val target: LuaAst.Ref = func.symbol
          .getAnnotation(extensionMethodAnnotSym)
          .map(_ => transform(func).asInstanceOf[LuaAst.Ref])
          .getOrElse(transform(func).asInstanceOf[LuaAst.Ref])

        val targetMethodName = target.name

        val functionCall = if (targetMethodName matches "[+-[*]/%^<>]|~=|[!<>=]=") {
          if (targetMethodName.value == "!=") LuaAst.InfixOperation(target.prefix.get, "~=", transform(argss.head.head).toExpr)
          else LuaAst.InfixOperation(target.prefix.get, targetMethodName, transform(argss.head.head).toExpr)
        } else if (func.symbol.hasAnnotation(invokeAnnotSym)) {
          LuaAst.Invoke(target, argss.flatten map transform)
        } else if (func.symbol.hasAnnotation(invokeAsFieldAnnotSym) && argss.flatten.isEmpty) {
          target
        } else {
          if (targetMethodName.value == "apply" && funcOwnerTpe.isFunctionType)
            LuaAst.Invoke(
              LuaAst.Ref(None, target.prefix.get.toString),
              argss.flatten map transform
            ) //TODO: using the prefix.toString is TERRIBLE, need to do something better here
          else LuaAst.Dispatch(target, argss.flatten map transform)
        }

        if (
          func.symbol.flags.is(Flags.Implicit & Flags.Synthetic) &&
          func.symbol.tree.asInstanceOf[DefDef].returnTpt.symbol.flags.is(Flags.Implicit)
        ) {
          functionCall match {
            case i @ LuaAst.Invoke(sym: LuaAst.Ref, _) => i.copy(sym = LuaAst.Ref(sym.prefix, s"${sym.name}Implicit"))
            case d: LuaAst.Dispatch => d.copy(sym = LuaAst.Ref(d.sym.prefix, s"${d.sym.name}Implicit"))
            case other => other
          }
        } else functionCall
      }
  }

  private object AsExpr {
    def unapply(t: Term) = Some(t.asExpr)
  }
}
