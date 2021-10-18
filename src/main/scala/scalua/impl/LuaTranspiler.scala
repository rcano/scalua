package scalua
package impl

import Predef.*
import scala.jdk.CollectionConverters.*
import scala.quoted.*
import scala.quoted.runtime.StopMacroExpansion
import scala.util.chaining.*
import scala.runtime.TupleXXL

private[scalua] class LuaTranspiler(using val q: Quotes)
    extends ConstantsHandler,
      RefsHandler,
      FunctionsHandler,
      VarsHandler,
      ControlFlowsHandler,
      DefsHandler,
      TemplatesHandler,
      PatMatchHandler,
      BlockHandler {
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

  lazy val treeHandlers = scala.collection.mutable.LinkedHashMap[Int, PartialFunction[Tree, LuaAst.LuaTree]]()

  private var freshNameCounter = 0
  protected def nextName: LuaAst.Ident = {
    freshNameCounter += 1
    "__x_" + freshNameCounter
  }

  def transform(tree: Tree): LuaAst.LuaTree = try {
    treeHandlers.valuesIterator.find(_.isDefinedAt(tree)) match {
      case Some(handler) => handler(tree)
      case _ =>
        val stack = StackWalker.getInstance.walk(_.iterator.asScala.drop(1).take(10).toSeq).mkString("\n")
        report.errorAndAbort(s"Unsupported tree: ${tree.show(using Printer.TreeStructure)}\n$stack", tree.pos)
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
  protected def getRenamed(tree: Tree): Option[String] = tree.symbol.getAnnotation(renamedAnnotSym) match {
    case Some(renamed) =>
      val '{ scalua.renamed($name: String) } = renamed.asExpr
      Some(name.valueOrAbort)
    case _ => None
  }
  protected def luaModuleName(name: String): String = name.replace("$", "_") + "_MODULE"

  protected def functionArguments(args: Seq[ValDef]): Seq[LuaAst.Ident] = {
    args.flatMap { arg =>
      TuplesArity.get(arg.symbol) match {
        case Some(arity) => Vector.tabulate(arity)(i => s"${arg.name}_${i + 1}")
        case _ => Seq(arg.name)
      }
    }
  }

  protected def isInstanceOfCheck(expr: LuaAst.LuaExpr, tpt: TypeTree): LuaAst.LuaTree = {
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
