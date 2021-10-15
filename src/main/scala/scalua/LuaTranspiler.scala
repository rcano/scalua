package scalua

import Predef.*
import scala.jdk.CollectionConverters.*
import scala.quoted.*
import scala.quoted.runtime.StopMacroExpansion
import scala.util.chaining.*
import scala.runtime.TupleXXL

object LuaAst {

  class Printer(output: StringBuilder, indent: Int = 0) {
    private val indentPrefix = "  " * indent
    def println(l: String): Unit = output ++= indentPrefix ++= l

    def inc: Printer = Printer(output, indent + 1)
  }

  case class PPrinter(depth: Int = 0) {
    private[this] val currIndent = "  " * depth
    def pprint(strs: String*): String = {
      if (strs.isEmpty) ""
      else {
        val it = strs.iterator.flatMap(_.split("\\n"))
        it.foldLeft(new StringBuilder() ++= currIndent ++= it.next)((sb, l) => sb ++= "\n" ++= currIndent ++= l).result()
      }
    }
    def inc = new PPrinter(depth + 1)
  }
  object PPrinter {
    val indent0 = PPrinter(0)
    val indent1 = PPrinter(1)
  }
  implicit class PrinterStringContext(val sc: StringContext) extends AnyVal {
    def print(args: Any*)(implicit printer: PPrinter) = {
      printer pprint sc.s(args: _*)
    }
  }
  sealed trait LuaTree {
    def pprint(implicit p: PPrinter): String
    override def toString = pprint(PPrinter.indent0)
    def toExpr: LuaExpr
  }
  sealed trait LuaExpr extends LuaTree {
    def toExpr = this
  }
  sealed trait LuaStat extends LuaTree {
    def toExpr: LuaExpr = this match {
      case f: Function => LuaInlined(s"($f)()")
      case _: (Block | IfThenElse) => Function(Seq(), this).toExpr
      case _ => throw new IllegalStateException(s"Statement $this cannot be turned to expression")
    }
  }

  case object nil {
    def pprint(implicit p: PPrinter) = print"nil"
  }
  case class Constant(c: Any) extends LuaTree, LuaExpr {
    def pprint(implicit p: PPrinter) = c match
      case s: String => print""""${s.replace("\"", "\\\"")}""""
      case null => print"nil"
      case other => p.pprint(String valueOf other)
  }
  case class LuaInlined(script: String) extends LuaTree, LuaExpr, LuaStat {
    def pprint(implicit p: PPrinter) = p.pprint(script)
    override def toExpr = this
  }
  case class Tuple(values: Seq[LuaTree]) extends LuaTree, LuaExpr {
    def pprint(implicit p: PPrinter) = p.pprint(values.map(_.toString).mkString(", "))
  }
  case class MapLiteral(entries: Seq[(LuaTree, LuaTree)]) extends LuaTree, LuaExpr {
    def pprint(implicit p: PPrinter) = {
      val entriesStr = entries.map(e => s"[${e._1}] = ${e._2}").mkString(", ")
      print"{${entriesStr}}"
    }
  }
  case class Ident private (value: String) { override def toString = value }
  object Ident:
    def apply(name: String): Ident = new Ident(name.replace("$", "_"))
    given Conversion[String, Ident] = apply
    given Conversion[Ident, String] = _.value
  case class Assign(names: Seq[Ident], value: LuaTree) extends LuaTree, LuaStat {
    def pprint(implicit p: PPrinter) = p.pprint(s"${names.mkString(", ")} = $value")
  }
  object Assign { def apply(name: Ident, value: LuaTree) = new Assign(Seq(name), value) }
  case class Var(assign: Assign, local: Boolean) extends LuaTree, LuaStat {
    def pprint(implicit p: PPrinter) = p.pprint(s"${if (local) "local " else ""}$assign")
  }
  object Var { def apply(name: Ident, value: LuaTree, local: Boolean) = new Var(Assign(Seq(name), value), local) }
  case class Ref(prefix: Option[LuaExpr], name: Ident) extends LuaTree, LuaExpr {
    def pprint(implicit p: PPrinter) = p.pprint(prefix.fold("")(p => s"$p.") + name)
  }
  case class Block(stats: Seq[LuaTree]) extends LuaTree, LuaStat { def pprint(implicit p: PPrinter) = stats.map(_.pprint).mkString("\n") }

  /** AST node represting an instance method being invoked (with the colon operator)
    */
  case class Dispatch(sym: Ref, args: Seq[LuaTree]) extends LuaTree, LuaExpr {
    def pprint(implicit p: PPrinter) = p.pprint(sym.prefix.fold("")(p => s"$p:") + sym.name + s"(${args.mkString(", ")})")
  }

  /** AST node represeting a method invocation (with the dot operator)
    */
  case class Invoke(sym: LuaExpr, args: Seq[LuaTree]) extends LuaTree, LuaExpr {
    def pprint(implicit p: PPrinter) = sym.pprint + s"(${args.mkString(", ")})"
  }
  case class InfixOperation(lhs: LuaExpr, op: String, rhs: LuaExpr) extends LuaTree, LuaExpr {
    def pprint(implicit p: PPrinter) = print"$lhs $op $rhs"
  }
  case class UnaryOperation(op: String, expr: LuaExpr) extends LuaTree, LuaExpr {
    def pprint(implicit p: PPrinter) = print"$op$expr"
  }
  case class IfThenElse(cond: LuaExpr, thenBranch: LuaTree, elseBranch: LuaTree) extends LuaTree, LuaStat {
    def pprint(implicit p: PPrinter) = {
      var lines = Vector(s"if $cond then", thenBranch.pprint(PPrinter.indent1))
      if (elseBranch != NoTree && elseBranch != Constant(nil)) lines ++= Vector("else", elseBranch.pprint(PPrinter.indent1))
      lines :+= "end"
      p.pprint(lines: _*)
    }
  }
  case class While(cond: LuaExpr, expr: LuaTree) extends LuaTree, LuaStat {
    def pprint(implicit p: PPrinter) = p.pprint(s"while $cond do", expr.pprint(PPrinter.indent1), "end")
  }
  case class Function(args: Seq[Ident], body: LuaTree) extends LuaTree, LuaStat {
    def pprint(implicit p: PPrinter) = {
      val bodyLines = body match {
        case Block(sts) =>
          sts.init.iterator
            .map {
              case v: Var => v.copy(local = true)
              case other => other
            }
            .map(_.pprint(PPrinter.indent1))
            .toVector :+ returnStat(sts.last).pprint(PPrinter.indent1)
        case other => Vector(returnStat(other).pprint(PPrinter.indent1))
      }
      p.pprint(s"function (${args.mkString(",")})" +: bodyLines :+ "end": _*)

    }
    private def returnStat(stat: LuaTree): LuaTree = stat match {
      case IfThenElse(cond, thenB, elseB) => IfThenElse(cond, returnStat(thenB), returnStat(elseB))
      case Block(sts) => Block(sts.init :+ returnStat(sts.last))
      case other => LuaInlined(s"return $other")
    }
  }
  case class For(iteratorName: Ident, from: LuaExpr, to: LuaExpr, step: LuaTree, code: LuaTree) extends LuaTree, LuaStat {
    def pprint(implicit p: PPrinter) = p.pprint(s"for $iteratorName = $from, $to, $step do", code.pprint(PPrinter.indent1), "end")
  }
  case class Iterate(vars: Seq[Ident], iterator: LuaTree, state: Option[LuaTree], init: Option[LuaTree], code: LuaTree)
      extends LuaTree,
        LuaStat {
    def pprint(implicit p: PPrinter) = {
      p.pprint(
        s"for ${vars.mkString(", ")} in " + (Some(iterator) ++ state ++ init).mkString(", ") + "do",
        code.pprint(PPrinter.indent1),
        "end"
      )
    }
  }
  sealed trait Template extends LuaTree, LuaStat {
    def prefix: Option[String]
    def name: Ident
    def fqdn: String
    def body: Block
    def local: Boolean
  }
  case class Class(prefix: Option[String], name: Ident, fqdn: String, params: Seq[Ident], body: Block, local: Boolean) extends Template {
    def pprint(implicit p: PPrinter) = {
      val classMembers = body.stats.map {
        case Var(Assign(names, f: Function), _) => Var(Assign(names.map[Ident]("self." + _), f.copy(args = "self" +: f.args)), false)
        case Var(Assign(names, assignment), _) => Var(Assign(names.map[Ident]("self." + _), assignment), false)
        case s: Singleton => Block(Seq(s.copy(local = true), Var(s"self.${s.name}", Ref(None, s.name), false)))
        case s: Class => Block(Seq(s.copy(local = true), Var(s"self.${s.name}", Ref(None, s.name), false)))
        case other => other
      }

      val paramsToString = params
        .map(s => InfixOperation(Constant(s + "="), "..", Invoke(Ref(None, "tostring"), Seq(Ref(None, s"v.$s")))))
        .reduceRightOption((a, b) => InfixOperation(a, ".. \",\" .. ", b))

      val toString = Function(
        Seq("v"),
        paramsToString match {
          case Some(paramsToString) => InfixOperation(InfixOperation(Constant(name + "("), "..", paramsToString), "..", Constant(")"))
          case _ => Constant(s"$name")
        }
      )
      val ctor = Var(
        s"$name.new",
        Function(
          "class" +: params,
          Block(
            Seq(
              Var("self", MapLiteral(Seq.empty), true),
              Assign("class.__index", Ref(None, "class")),
              Assign("class.__className", Constant(fqdn)),
              Assign("class.__tostring", toString),
              Block(classMembers),
              Invoke(Ref(None, "setmetatable"), Seq(Ref(None, "self"), Ref(None, "class")))
            )
          )
        ),
        false
      )

      Block(Seq(Var(name, MapLiteral(Seq.empty), local), ctor)).pprint
    }
  }
  case class Singleton(prefix: Option[String], name: Ident, fqdn: String, body: Block, local: Boolean) extends Template {
    def pprint(implicit p: PPrinter) = {
      LuaAst
        .Block(
          Seq(
            LuaAst.Class(prefix, name, fqdn, Seq.empty, body, local),
            LuaAst.Var(name + "_tmp", LuaAst.Dispatch(LuaAst.Ref(Some(LuaAst.Ref(None, name)), "new"), Seq.empty), true),
            LuaAst.Var(name, LuaAst.Ref(None, name + "_tmp"), local),
            LuaAst.Assign(name + "_tmp", LuaAst.Constant(LuaAst.nil)),
          )
        )
        .pprint
    }
  }
  case class StagedNode(tree: Expr[LuaTree]) extends LuaTree, LuaStat, LuaExpr {
    def pprint(implicit p: PPrinter) = ???
    override def toExpr = this
  }
  case object NoTree extends LuaTree, LuaStat, LuaExpr {
    def pprint(implicit p: PPrinter) = ""
    override def toExpr = this
  }
}

class LuaTranspiler(using val q: Quotes) {
  import q.reflect.*

  val globalAnnotSym = TypeRepr.of[global].typeSymbol
  val renamedAnnotSym = TypeRepr.of[renamed].typeSymbol
  val extensionMethodAnnotSym = TypeRepr.of[extensionMethod].typeSymbol
  val invokeAsFieldAnnotSym = TypeRepr.of[invokeAsField].typeSymbol
  val invokeAnnotSym = TypeRepr.of[invoke].typeSymbol

  val TuplesArity: Map[Symbol, Int] = (2 to 22).map(i => defn.TupleClass(i) -> i).toMap
  val TupleFactories = ((1 to 22).map(i => defn.TupleClass(i).companionModule) :+ TypeRepr.of[TupleXXL.type].typeSymbol)
    .map(_.methodMember("apply").head)
    .toSet

  val XxlTupleApply = TypeRepr.of[scala.runtime.Tuples.type].typeSymbol.methodMember("apply").head

  def transform(tree: Tree): LuaAst.LuaTree = try {
    tree match { // the order of the statements determine priority, needs to be handle with care. Incorrect order may trigger SOE
      case Inlined(call, binding, expr) =>
        // println(s"""|${Console.BLUE}$call${Console.RESET}
        //             |${Console.YELLOW}$binding${Console.RESET}
        //             |${Console.CYAN}$expr${Console.RESET}""".stripMargin)
        if (binding.isEmpty) transform(expr)
        else
          LuaAst.Block(binding.map(transform) :+ transform(expr))

      case Literal(UnitConstant()) => LuaAst.NoTree
      case Literal(const) => LuaAst.Constant(if (const.value == null) LuaAst.nil else const.value)

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

      case TypeApply(Select(pref, "asInstanceOf" | "$asInstanceOf$"), _) => transform(pref) //casting has no meaning to lua
      case TypeApply(m @ Select(pref, "isInstanceOf"), tpt) if m.symbol.maybeOwner == defn.AnyClass =>
        isInstanceOfCheck(transform(pref).toExpr, tpt.head)
      case TypeApply(pref, _) => transform(pref) //types have no meaning to lua
      case Typed(prefix, _) => transform(prefix) //abscribing has no meaning to lua

      case Apply(on, args) =>
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

      case vd @ ValDef(name, tpt, Some(rhs)) => varDef(vd, name, tpt.tpe, rhs, vd.symbol.flags.is(Flags.Private | Flags.PrivateLocal))

      case ValDef(_, _, None) =>
        LuaAst.NoTree // nothing to do with abstract members or non initialized fields in a class, since lua is dynamic

      case Assign(ident, value) => LuaAst.Assign(transform(ident).toString, transform(value))

      case If(cond, thenBranch, elseBranch) => LuaAst.IfThenElse(transform(cond).toExpr, transform(thenBranch), transform(elseBranch))
      case While(cond, expr) => LuaAst.While(transform(cond).toExpr, transform(expr))

      case Import(prefix, selectors) => LuaAst.NoTree

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

      case Match(expr, cases) => patternMatch(expr, cases)

      case Block(stats, expr) =>
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
            val b = transform(Block(stats.drop(2), expr)).asInstanceOf[LuaAst.Block]
            LuaAst.Block(singleton +: b.stats)

          case idx =>
            val pre = transform(Block(stats.take(idx), Literal(UnitConstant()))).asInstanceOf[LuaAst.Block]
            val singleton = transform(Block(stats.slice(idx, idx + 2), Literal(UnitConstant()))).asInstanceOf[LuaAst.Singleton]
            val post = transform(Block(stats.drop(idx + 2), expr)).asInstanceOf[LuaAst.Block]
            LuaAst.Block(pre.stats ++ Seq(singleton) ++ post.stats)

        }

      case other =>
        val stack = StackWalker.getInstance.walk(_.iterator.asScala.drop(1).take(10).toSeq).mkString("\n")
        report.errorAndAbort(s"Unsupported tree: ${other.show(using Printer.TreeStructure)}\n$stack", other.pos)
    }
  } catch {
    case e: StopMacroExpansion => throw e
    case util.control.NonFatal(e) =>
      report.errorAndAbort(s"Failed to process tree \n${tree}\ndue to $e\n" + e.getStackTrace.take(10).mkString("\n"), tree.pos)
    case e: StackOverflowError =>
      val portionSize = 200
      val stack = StackWalker.getInstance.walk(stream =>
        val it = stream.iterator.asScala
        val init = it.take(portionSize).toSeq
        val tail = collection.mutable.Queue.empty[StackWalker.StackFrame]
        it foreach (e =>
          if tail.size > portionSize then tail.dequeue()
          tail.enqueue(e)
        )
        init.mkString("\n") + "\n...\n" + tail.mkString("\n")
      )
      report.errorAndAbort(s"Failed to process tree \n${tree}\ndue to $e\n$stack", tree.pos)
  }

  /** produces a LuaAst.Ref based on the ident handling @global and @renamed annotations
    */
  private def getRenamed(tree: Tree): Option[String] = tree.symbol.getAnnotation(renamedAnnotSym) match {
    case Some(renamed) =>
      val '{ scalua.renamed($name: String) } = renamed.asExpr
      Some(name.valueOrAbort)
    case _ => None
  }
  private def luaModuleName(name: String): String = name.replace("$", "_") + "_MODULE"
  private def shouldTreatAsModule(tree: Term) =
    tree.symbol.flags.is(Flags.Module) || (tree.symbol.flags.is(Flags.Implicit) && tree.symbol.isType && tree.tpe <:< TypeRepr.of[AnyVal])
  private def unsplicedLuaTree(tree: Tree) = {
    report.error("LuaTree must be spliced using the method splice.", tree.pos)
    LuaAst.NoTree
  }

  private def functionArguments(args: Seq[ValDef]): Seq[LuaAst.Ident] = {
    args.flatMap { arg =>
      TuplesArity.get(arg.symbol) match {
        case Some(arity) => Vector.tabulate(arity)(i => s"${arg.name}_${i + 1}")
        case _ => Seq(arg.name)
      }
    }
  }

  private def varDef(tree: Tree, name: String, tpe: TypeRepr, value: Tree, local: Boolean): LuaAst.LuaTree = {
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

  private def isInstanceOfCheck(expr: LuaAst.LuaExpr, tpt: TypeTree): LuaAst.LuaTree = {
    if tpt.tpe =:= TypeRepr.of[Byte] || tpt.tpe =:= TypeRepr.of[Short] || tpt.tpe =:= TypeRepr.of[Int]
      || tpt.tpe =:= TypeRepr.of[Long] || tpt.tpe =:= TypeRepr.of[Float]
    then
      report.errorAndAbort(
        s"Cannot instance check in runtime against ${tpt.tpe.show(using Printer.TypeReprCode)}. Lua only supports the number type",
        tpt.pos
      )
    else if tpt.tpe =:= TypeRepr.of[Char] then
      report.errorAndAbort(s"Cannot instance check in runtime against Chars. Lua only supports the string type", tpt.pos)
    else if tpt.tpe =:= TypeRepr.of[Double] || tpt.tpe =:= TypeRepr.of[BigDecimal] || tpt.tpe =:= TypeRepr.of[Number] then
      LuaAst.InfixOperation(LuaAst.Invoke(LuaAst.Ref(None, "type"), Seq(expr)), "==", LuaAst.Constant("number"))
    else if tpt.tpe =:= TypeRepr.of[String] then
      LuaAst.InfixOperation(LuaAst.Invoke(LuaAst.Ref(None, "type"), Seq(expr)), "==", LuaAst.Constant("string"))
    else if tpt.tpe =:= TypeRepr.of[Boolean] then
      LuaAst.InfixOperation(LuaAst.Invoke(LuaAst.Ref(None, "type"), Seq(expr)), "==", LuaAst.Constant("boolean"))
    else if tpt.tpe <:< TypeRepr.of[LuaStdLib.Map[?, ?]] then
      LuaAst.InfixOperation(LuaAst.Invoke(LuaAst.Ref(None, "type"), Seq(expr)), "==", LuaAst.Constant("table"))
    else if tpt.tpe =:= TypeRepr.of[LuaStdLib.List[?]] then
      report.errorAndAbort(s"Cannot instance check in runtime against Lists. Lua implements lists as tables", tpt.pos)
    else if tpt.tpe =:= TypeRepr.of[LuaStdLib.Map] then
      LuaAst.InfixOperation(LuaAst.Invoke(LuaAst.Ref(None, "type"), Seq(expr)), "==", LuaAst.Constant("table"))
    else
      LuaAst.InfixOperation(
        LuaAst.InfixOperation(LuaAst.Invoke(LuaAst.Ref(None, "type"), Seq(expr)), "==", LuaAst.Constant("table")),
        "and",
        LuaAst.InfixOperation(LuaAst.Ref(Some(expr), "__className"), "==", LuaAst.Constant(tpt.symbol.fullName))
      )

  }

  object ImplicitClassConversion:
    def unapply(tree: Tree): Option[(Term, Term)] = tree match
      case Select(Apply(implicitConv, List(arg)), _) if implicitConv.symbol.flags is Flags.Implicit => Some(implicitConv -> arg)
      case _ => None


  private var freshNameCounter = 0
  private def patternMatch(expr: Tree, cases: Seq[CaseDef]): LuaAst.LuaTree = {
    val No = LuaAst.NoTree
    def nextName: LuaAst.Ident = {
      freshNameCounter += 1
      "__x_" + freshNameCounter
    }

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
        val unapplyExpr = LuaAst.Invoke(transform(ref).asInstanceOf[LuaAst.Ref], Seq(expr))
        val unapplyOpt = LuaAst.Var(LuaAst.Assign(nextName, unapplyExpr), true)
        //declare the extracted variables
        val unapplyTuple = LuaAst.Var(LuaAst.Assign(patsWithName.map(e => e._2), LuaAst.Constant(LuaAst.nil)), true)
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
        case ((Some(setup), cond, thenBranch), elseBranch) => LuaAst.Block(Seq(setup, LuaAst.IfThenElse(cond.toExpr, thenBranch, elseBranch)))
      }
    LuaAst.Block(List(capturedExpr, luaCases))
  }

  private object AsExpr {
    def unapply(t: Term) = Some(t.asExpr)
  }

  given ToExpr[LuaAst.Ident] with {
    def apply(i: LuaAst.Ident)(using Quotes) = '{ LuaAst.Ident(${ Expr(i.value) }) }
  }
  given ToExpr[LuaAst.LuaTree] with {
    given ToExpr[LuaAst.LuaExpr] = this.asInstanceOf
    val constantToExpr: ToExpr[Any] = new ToExpr[Any] {
      def apply(c: Any)(using Quotes) = c match {
        case n: (java.lang.Byte | java.lang.Short | java.lang.Integer | java.lang.Long) => Expr(n.longValue)
        case n: (java.lang.Float | java.lang.Double) => Expr(n.doubleValue)
        case c: Char => Expr(c)
        case b: Boolean => Expr(b)
        case s: String => Expr(s)
      }
    }
    def apply(node: LuaAst.LuaTree)(using Quotes) = node match {
      case LuaAst.NoTree => '{ LuaAst.NoTree }
      case LuaAst.Constant(LuaAst.nil) => '{ LuaAst.Constant(LuaAst.nil) }
      case LuaAst.Constant(n) => '{ LuaAst.Constant(${ Expr(n)(using constantToExpr) }) }
      case LuaAst.LuaInlined(n) => '{ LuaAst.LuaInlined(${ Expr(n) }) }
      case LuaAst.Tuple(v) => '{ LuaAst.Tuple(${ Expr(v) }) }
      case LuaAst.Assign(n, v) => '{ LuaAst.Assign(${ Expr(n) }, ${ Expr(v) }) }
      case LuaAst.Var(a, l) => '{ LuaAst.Var(${ apply(a).asExprOf[LuaAst.Assign] }, ${ Expr(l) }) }
      case LuaAst.Ref(prefix, name) => '{ LuaAst.Ref(${ Expr(prefix) }, ${ Expr(name) }) }
      case LuaAst.Block(stats) => '{ LuaAst.Block(${ Expr(stats) }) }
      case LuaAst.Dispatch(sym, args) => '{ LuaAst.Dispatch(${ apply(sym).asExprOf[LuaAst.Ref] }, ${ Expr(args) }) }
      case LuaAst.Invoke(sym, args) => '{ LuaAst.Invoke(${ apply(sym).asExprOf[LuaAst.LuaExpr] }, ${ Expr(args) }) }
      case LuaAst.InfixOperation(l, o, r) => '{ LuaAst.InfixOperation(${ Expr(l) }, ${ Expr(o) }, ${ Expr(r) }) }
      case LuaAst.UnaryOperation(o, e) => '{ LuaAst.UnaryOperation(${ Expr(o) }, ${ Expr(e) }) }
      case LuaAst.IfThenElse(cond, thenB, elseB) => '{ LuaAst.IfThenElse(${ Expr(cond) }, ${ Expr(thenB) }, ${ Expr(elseB) }) }
      case LuaAst.While(cond, expr) => '{ LuaAst.While(${ Expr(cond) }, ${ Expr(expr) }) }
      case LuaAst.Function(args, body) => '{ LuaAst.Function(${ Expr(args) }, ${ Expr(body) }) }
      case LuaAst.For(iteratorName, from, to, step, code) =>
        '{ LuaAst.For(${ Expr(iteratorName) }, ${ Expr(from) }, ${ Expr(to) }, ${ Expr(step) }, ${ Expr(code) }) }
      case LuaAst.Iterate(params, it, state, step, code) =>
        '{ LuaAst.Iterate(${ Expr(params) }, ${ Expr(it) }, ${ Expr(state) }, ${ Expr(step) }, ${ Expr(code) }) }
      case LuaAst.Class(prefix, name, fqdn, params, body, local) =>
        '{
          LuaAst.Class(
            ${ Expr(prefix) },
            ${ Expr(name) },
            ${ Expr(fqdn) },
            ${ Expr(params) },
            ${ apply(body).asExprOf[LuaAst.Block] },
            ${ Expr(local) }
          )
        }
      case LuaAst.Singleton(prefix, name, fqdn, body, local) =>
        '{
          LuaAst.Singleton(${ Expr(prefix) }, ${ Expr(name) }, ${ Expr(fqdn) }, ${ apply(body).asExprOf[LuaAst.Block] }, ${ Expr(local) })
        }
      case LuaAst.MapLiteral(entries) => '{ LuaAst.MapLiteral(${ Expr(entries) }) }
      case LuaAst.StagedNode(tree) => tree
    }
  }
  def transpile(tree: Expr[Any]): Expr[LuaAst.LuaTree] = {
    println(s"Processing\n${tree.asTerm.show(using Printer.TreeStructure)}")
    val luaTree = transform(tree.asTerm)
    report.info(luaTree.pprint(using LuaAst.PPrinter(0)))
    Expr(luaTree)
  }
}

object LuaMacro {

  inline def transpile(inline a: Any): LuaAst.LuaTree = ${ transpileMacro('a) }

  def transpileMacro(a: Expr[Any])(using Quotes): Expr[LuaAst.LuaTree] = new LuaTranspiler().transpile(a)
}
