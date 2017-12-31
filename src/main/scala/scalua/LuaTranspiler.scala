package scalua

import language.experimental.macros
import Predef._
import scala.reflect.macros.Universe
import scala.reflect.macros.blackbox.Context

object LuaAst {
  case class PPrinter(depth: Int = 0) {
    val currIndent = "  " * depth
    def pprint(str: String) = currIndent + str.replace("\n", "\n" + currIndent)
    def inc = new PPrinter(depth + 1)
  }
  implicit class PrinterStringContext(val sc: StringContext) extends AnyVal {
    def print(args: Any*)(implicit printer: PPrinter) = {
      printer pprint sc.s(args:_*)
    }
  }
  sealed trait LuaTree {
    def pprint(implicit p: PPrinter): String
    override def toString = pprint(PPrinter(0))
  }
  case object nil {
    def pprint(implicit p: PPrinter) = print"nil"
  }
  case class Constant(c: Any) extends LuaTree { def pprint(implicit p: PPrinter) = c match {
      case s: String => print"""${'"' + s.replace("\"", "\\\"") + '"'}"""
      case null => print"nil"
      case other => print"${String valueOf other}"
    }
  }
  case class LuaInlined(script: String) extends LuaTree {
    def pprint(implicit p: PPrinter) = print"$script"
  }
  case class Tuple(values: Seq[LuaTree]) extends LuaTree { def pprint(implicit p: PPrinter) = print"${values.map(_.pprint(p.inc).trim).mkString(", ")}"}
  case class MapLiteral(entries: Seq[(LuaTree, LuaTree)]) extends LuaTree { def pprint(implicit p: PPrinter) =  {
      val entriesStr = entries.map(e => s"[${e._1}] = ${e._2}").mkString(", ")
      print"{$entriesStr}"
    }
  }
  case class Assign(names: Seq[String], value: LuaTree) extends LuaTree { def pprint(implicit p: PPrinter) = print"${names.mkString(", ")} = " + value.pprint.trim }
  object Assign { def apply(name: String, value: LuaTree) = new Assign(Seq(name), value) }
  case class Var(assign: Assign, local: Boolean) extends LuaTree { def pprint(implicit p: PPrinter) = print"${if (local) "local " else ""}" + assign.pprint.trim }
  object Var { def apply(name: String, value: LuaTree, local: Boolean) = new Var(Assign(Seq(name), value), local) }
  case class Ref(prefix: Option[LuaTree], name: String) extends LuaTree { def pprint(implicit p: PPrinter) = print"${prefix.fold("")(_ + ".")}$name" }
  case class Block(stats: Seq[LuaTree]) extends LuaTree { def pprint(implicit p: PPrinter) = {
      stats.map(_.pprint).mkString("\n")
    }
  }
  /**
   * AST node represting an instance method being invoked (with the colon operator)
   */
  case class Dispatch(sym: Ref, args: Seq[LuaTree]) extends LuaTree {
    def pprint(implicit p: PPrinter) = print"${sym.prefix.fold("")(_ + ":")}${sym.name}(${args.mkString(", ").replace("\n", "\n" + p.currIndent)})"
  }
  /**
   * AST node represeting a method invocation (with the dot operator)
   */
  case class Invoke(sym: Ref, args: Seq[LuaTree]) extends LuaTree {
    def pprint(implicit p: PPrinter) = sym.pprint + print"(${args.mkString(", ").replace("\n", "\n" + p.currIndent)})"
  }
  case class InfixOperation(lhs: LuaTree, op: String, rhs: LuaTree) extends LuaTree {
    def pprint(implicit p: PPrinter) = print"$lhs $op $rhs"
  }
  case class UnaryOperation(op: String, expr: LuaTree) extends LuaTree {
    def pprint(implicit p: PPrinter) = print"$op$expr"
  }
  case class IfThenElse(cond: LuaTree, thenBranch: LuaTree, elseBranch: LuaTree) extends LuaTree {
    def pprint(implicit p: PPrinter) = {
      val thenStr = thenBranch.pprint(p.inc)
      val elseStr = if (elseBranch != NoTree) s"\n${p.currIndent}else\n" +  elseBranch.pprint(p.inc) else ""
      print"if $cond then\n$thenStr$elseStr\n" + print"end"
    }
  }
  case class While(cond: LuaTree, expr: LuaTree) extends LuaTree { def pprint(implicit p: PPrinter) = s"while $cond do\n${expr.pprint(p.inc)}\nend" }
  case class Function(args: Seq[String], body: LuaTree) extends LuaTree {
    def pprint(implicit p: PPrinter) = {
      val incp = p.inc
      val bodyStr = {
        implicit val p = incp
        body match {
          case Block(sts) => sts.init.iterator.map {
              case v: Var => v.copy(local = true)
              case other => other
            }.map(_.pprint).mkString("\n") + "\n" + print"return " + sts.last.pprint.trim
          case other => print"return " + other.pprint.trim
        }
      }
      print"function (${args.mkString(",")})\n$bodyStr\n" + print"end"
    }
  }
  case class For(iteratorName: String, from: LuaTree, to: LuaTree, step: LuaTree, code: LuaTree) extends LuaTree {
    def pprint(implicit p: PPrinter) = {
      print"for $iteratorName = $from, $to, $step do\n" +
      code.pprint(p.inc) + "\n" +
      print"end"
    }
  }
  case class Iterate(vars: Seq[String], iterator: LuaTree, state: Option[LuaTree], init: Option[LuaTree], code: LuaTree) extends LuaTree {
    def pprint(implicit p: PPrinter) = {
      print"for ${vars.mkString(", ")} in " + (Some(iterator) ++ state ++ init).mkString(", ") + " do\n" +
      code.pprint(p.inc) + "\n" +
      print"end"
    }
  }
  trait Template extends LuaTree {
    def prefix: Option[String]
    def name: String
    def body: Block
    def local: Boolean
  }
  case class Class(prefix: Option[String], name: String, params: Seq[String], body: Block, local: Boolean) extends Template {
    def pprint(implicit p: PPrinter) = {
      val classMembers = body.stats.map {
        case Var(Assign(names, f: Function), _) => Var(Assign(names.map("self." + _), f.copy(args = "self" +: f.args)), false)
        case Var(Assign(names, assignment), _) => Var(Assign(names.map("self." + _), assignment), false)
        case s: Singleton => Block(Seq(s.copy(local = true), Var(s"self.${s.name}", Ref(None, s.name), false)))
        case s: Class => Block(Seq(s.copy(local = true), Var(s"self.${s.name}", Ref(None, s.name), false)))
        case other => other
      }
      
      val paramsToString = params.map(s => InfixOperation(Constant(s + "="), "..", Invoke(Ref(None, "tostring"), Seq(Ref(None, s"v.$s")))): LuaTree).
      reduceRightOption((a, b) => InfixOperation(a, ".. \",\" .. ", b))
      
      val  toString = Function(Seq("v"),
                               paramsToString match {
          case Some(paramsToString) => InfixOperation(InfixOperation(Constant(name + "("), "..", paramsToString), "..", Constant(")"))
          case _ => Constant(name)
        })
      
      val ctor = Var(s"$name.new", Function("class" +: params, Block(Seq(
              Var("self", MapLiteral(Seq.empty), true),
              Assign("class.__index", Ref(None, "class")),
              Assign("class.__className", Constant((prefix ++ Seq(name)).mkString("."))),
              Assign("class.__tostring", toString),
              Block(classMembers),
              Invoke(Ref(None, "setmetatable"), Seq(Ref(None, "self"), Ref(None, "class")))
            ))), false)

      Block(Seq(Var(name, MapLiteral(Seq.empty), local), ctor)).pprint
    }
  }
  case class Singleton(prefix: Option[String], name: String, body: Block, local: Boolean) extends Template {
    def pprint(implicit p: PPrinter) = {
      LuaAst.Block(Seq(
          LuaAst.Class(prefix, name, Seq.empty, body, local),
          LuaAst.Var(name + "_tmp", LuaAst.Dispatch(LuaAst.Ref(Some(LuaAst.Ref(None, name)), "new"), Seq.empty), true),
          LuaAst.Var(name, LuaAst.Ref(None, name + "_tmp"), local),
          LuaAst.Assign(name + "_tmp", LuaAst.Constant(LuaAst.nil)),
        )).pprint
    }
  }
  case class StagedNode(tree: Universe#Tree) extends LuaTree {
    def pprint(implicit p: PPrinter) = ???
  }
  case object NoTree extends LuaTree {
    def pprint(implicit p: PPrinter) = ""
  }
}

class LuaTranspiler[C <: Context](val context: C) {
  import context.universe._
  def transform(tree: Tree): LuaAst.LuaTree = try {
    tree match { // the order of the statements determine priority, needs to be handle with care. Incorrect order may trigger SOE
      case Literal(Constant(())) => LuaAst.NoTree
      case Literal(Constant(l)) => LuaAst.Constant(if (l == (())) LuaAst.nil else l)

      case Block(stats, expr) => LuaAst.Block(((stats :+ expr).iterator map transform flatMap { //flatten nested blocks
              case v: LuaAst.Block => v.stats
              case LuaAst.NoTree => Nil
              case other => List(other)
            }).toSeq)

      case q"$tuple.$field" if definitions.TupleClass.seq.exists(_ == tree.symbol.owner) && field.encodedName.toString.matches("_\\d+") =>
        tuple match {
          case q"$pref.$t" => LuaAst.Ref(Some(transform(pref)), t.toString + field.encodedName)
          case q"$t" => LuaAst.Ref(None, t.toString + field.encodedName)
        }

      case Ident(name) =>
        if (tree.tpe <:< typeOf[LuaAst.LuaTree] && !(tree.tpe =:= definitions.NothingTpe)) unsplicedLuaTree(tree)
        else {
          if (tree.symbol.annotations.find(_.tree.tpe =:= typeOf[global]).isDefined) LuaAst.NoTree
          else getRenamed(tree) match {
            case Some(s) => LuaAst.Ref(None, s)
            case _ => LuaAst.Ref(None, if (shouldTreatAsModule(tree.symbol)) luaModuleName(name) else name.decodedName.toString)
          }
        }
      case tt: TypeTree => transform(tt.original)
      case Select(ident, select) =>
        if (tree.tpe <:< typeOf[LuaAst.LuaTree] && !(tree.tpe =:= definitions.NothingTpe)) unsplicedLuaTree(tree)
        else {
          val selected = getRenamed(tree) match {
            case Some(s) => s
            case _ => if (shouldTreatAsModule(tree.symbol)) luaModuleName(select) else select.decodedName.toString
          }
          if (selected == "unary_!") LuaAst.UnaryOperation("not ", transform(ident))
          else if (ident.symbol.annotations.find(_.tree.tpe =:= typeOf[global]).isDefined) LuaAst.Ref(None, selected)
          else {
            val prefix = transform(ident) match {
              case LuaAst.NoTree => None
              case other => Some(other)
            }
            LuaAst.Ref(prefix, selected)
          }
        }
      case q"$clazz.this.$select" =>
        if (tree.tpe <:< typeOf[LuaAst.LuaTree] && !(tree.tpe =:= definitions.NothingTpe)) unsplicedLuaTree(tree)
        else {
          if (tree.symbol.asTerm.isParamAccessor && !tree.symbol.asTerm.isAccessor) LuaAst.Ref(None, select.decodedName.toString) //a param accessor does not need self, since its contextualized to the function
          else LuaAst.Ref(None, "self." + select.decodedName)
        }

      case q"$prefix.asInstanceOf[$_]" => transform(prefix) //casting has no meaning to lua
      case q"$prefix: $tpt" => transform(prefix) //casting has no meaning to lua
      case q"$prefix.$method[..$tparams](...$args)" =>
        //TODO document the logic of this method, for it is quite large
        val invokedMethod = prefix.symbol match {
          case s: MethodSymbol => prefix.tpe.member(method).alternatives.find(_.isMethod).get.asMethod
          case other => 
            if (prefix.tpe == null) println(s"DEBUG= prefix $prefix has null type")
            prefix.tpe.member(method).alternatives.find(_.isMethod).get.asMethod
        }
        val selectTree = q"$prefix.$method"
        context.internal.setType(selectTree, invokedMethod.returnType)
        context.internal.setSymbol(selectTree, invokedMethod)
        lazy val methodRef = transform(selectTree).asInstanceOf[LuaAst.Ref]
        val methodName = method.decodedName.toString
        //attempt to identify default arguments passed
        val trueArgs = invokedMethod.paramLists.zip(args).map(t => t._1 zip t._2 filter {
            case (param, arg) if param.asTerm.isParamWithDefault && arg.toString.contains(s"$methodName$$default$$") => false
            case _ => true
          })

//        println(s"Prefix: $prefix, method: $method, invokedMethod: $invokedMethod, decoded method $methodRef, annotations ${invokedMethod.annotations}, is value class ${invokedMethod.owner.isImplicit}, ${invokedMethod.owner.asType}, ${invokedMethod.owner.asType.toType <:< typeOf[AnyVal]}")

        if (invokedMethod.owner == symbolOf[LuaStdLib.type]) {
          if (methodName == "cfor") {
            //need to identify which version of for this is
            val Seq(from, to, step) = args.head
            val (params, body) = transform(args.tail.head.head) match {
              case LuaAst.Function(params, body) => (params, body)
              case LuaAst.Block(Seq(LuaAst.Function(params, body))) => (params, body)
            }
            LuaAst.For(params.head, transform(from), transform(to), if (trueArgs.head.size < args.head.size) LuaAst.Constant(1) else transform(step), body)
          } else if (methodName == "List") {
            LuaAst.MapLiteral(args.head.zipWithIndex.map { case (arg, i) => (LuaAst.Constant(i + 1), transform(arg)) })
          } else if (methodName == "splice") {
            LuaAst.StagedNode(args.head.head)
          } else if (methodName == "inlineLua") {
            LuaAst.LuaInlined(context.eval[String](context.Expr(args.head.head)))
          } else LuaAst.Invoke(methodRef, args.flatten map transform)
          
        } else if (invokedMethod.owner.info.baseType(symbolOf[LuaStdLib.IterateApply[_, _, _]]) != NoType) {
          val LuaAst.Invoke(_, Seq(iterator, state, init, _)) = transform(prefix)
          val (params, body) = transform(args.head.head) match {
            case LuaAst.Function(params, body) => (params, body)
            case LuaAst.Block(Seq(LuaAst.Function(params, body))) => (params, body)
          }
          LuaAst.Iterate(params, iterator,
                         if (!state.toString.contains("$default$")) Some(state) else None,
                         if (!init.toString.contains("$default$")) Some(init) else None,
                         body)
          
        } else if (invokedMethod.owner.info.baseType(symbolOf[LuaStdLib.Map[_, _]]) != NoType) {
          methodName match {
            case "apply" => LuaAst.Ref(None, transform(prefix) + "[" + transform(args.head.head) + "]")
            case "update" => LuaAst.Var(transform(prefix) + "[" + transform(args.head.head) + "]", transform(args.head.tail.head), false)
            case "size" => LuaAst.UnaryOperation("#", transform(prefix))
          }
          
        } else if (invokedMethod.owner == symbolOf[LuaStdLib.Map.type]) {
          LuaAst.MapLiteral(args.flatten.map {
              case q"$prefix.Predef.ArrowAssoc[$_]($a).->[$_]($b)" => (transform(a), transform(b))
              case q"($a, $b)" => (transform(a), transform(b))
            })

        } else if (invokedMethod.owner.asType.toType =:= typeOf[String]) {
          if (methodName == "+" ) LuaAst.InfixOperation(transform(prefix), "..", transform(args.head.head))
          else if (methodName == "length") LuaAst.UnaryOperation("#", transform(prefix))
          else context.abort(tree.pos, s"Unsupported String api $invokedMethod")
          
          //extension methods via value classes
        } else if (invokedMethod.owner.isImplicit && invokedMethod.owner.asType.toType <:< typeOf[AnyVal] && 
                   prefix.collect { case q"$implicitConv($target)" => target }.nonEmpty) {
          val target = prefix.collect { case q"$implicitConv($target)" => target }.head
          val adaptedPrefix = context.internal.setType(q"${invokedMethod.owner}", invokedMethod.owner.typeSignature)
          var adaptedInvocation = q"${adaptedPrefix}.${invokedMethod.name}(...${List(target) :: args})"
          adaptedInvocation = context.internal.setType(adaptedInvocation, invokedMethod.typeSignature)
          adaptedInvocation = context.internal.setSymbol(adaptedInvocation, invokedMethod)
          
//          val invokeAnn = q"new _root_.scalua.invoke()"
//          invokeAnn.foreach(context.internal.setType(_, typeOf[invoke]))
//          context.internal.setAnnotations(adaptedInvocation.symbol, Annotation(invokeAnn))
          transform(adaptedInvocation)
          
        } else {
          val target: LuaAst.Ref = if (invokedMethod.annotations.find(_.tree.tpe =:= typeOf[extensionMethod]).isDefined) {
            prefix.collect { case q"$implicitConv($target)" => LuaAst.Ref(Some(transform(target)), methodRef.name) }.headOption getOrElse methodRef
          } else methodRef
          
          val targetMethodName = target.name
          
          val functionCall = if (targetMethodName matches "[+-[*]/%^<>]|~=|[!<>=]=") {
            if (targetMethodName == "!=") LuaAst.InfixOperation(target.prefix.get, "~=", transform(args.head.head))
            else LuaAst.InfixOperation(target.prefix.get, targetMethodName, transform(args.head.head))
          } else if (invokedMethod.annotations.find(_.tree.tpe =:= typeOf[invoke]).isDefined || invokedMethod.owner.isModuleClass) {
            LuaAst.Invoke(target, args.flatten map transform)
          } else if (invokedMethod.annotations.find(_.tree.tpe =:= typeOf[invokeAsField]).isDefined && args.flatten.isEmpty) {
            target
          } else {
            if (targetMethodName == "apply") LuaAst.Invoke(LuaAst.Ref(None, target.prefix.get.toString), args.flatten map transform) //TODO: using the prefix.toString is TERRIBLE, need to do something better here
            else LuaAst.Dispatch(target, args.flatten map transform)
          }
          
          if (invokedMethod.isImplicit && invokedMethod.isSynthetic && invokedMethod.returnType.typeSymbol.isImplicit) {
            functionCall match {
              case i: LuaAst.Invoke => i.copy(sym = LuaAst.Ref(i.sym.prefix, i.sym.name + "Implicit"))
              case d: LuaAst.Dispatch => d.copy(sym = LuaAst.Ref(d.sym.prefix, d.sym.name + "Implicit"))
              case other => other
            }
          } else functionCall
        }


      case q"${method: TermName}[..$tparams](...$args)" => LuaAst.Invoke(LuaAst.Ref(None, method.decodedName.toString), args.flatten map transform)
      case q"new $clazz[..$tparams](...$args)" => LuaAst.Dispatch(LuaAst.Ref(Some(transform(clazz)), "new"), args.flatten map transform)

      case q"$mods var $name = $value" => varDef(tree, name.decodedName.toString, value, mods.hasFlag(Flag.PRIVATE))
      case q"$mods val $name = $value" => varDef(tree, name.decodedName.toString, value, mods.hasFlag(Flag.PRIVATE))
      case q"$name = $value"  => LuaAst.Assign(transform(name).toString, transform(value))

      case q"if ($cond) $thenBranch else $elseBranch" => LuaAst.IfThenElse(transform(cond), transform(thenBranch), transform(elseBranch))
      case q"while ($cond) $expr" => LuaAst.While(transform(cond), transform(expr))
        
      case q"$mods def $name[..$tparams](...$argss): $ret = $body" =>
        val variableName = if (mods.hasFlag(Flag.IMPLICIT) && mods.hasFlag(Flag.SYNTHETIC) && ret.symbol.isImplicit) name.decodedName.toString + "Implicit"
        else name.decodedName.toString
        val bodyNode = if(ret.tpe =:= definitions.UnitTpe) {
          LuaAst.Block(Seq(transform(body), LuaAst.NoTree))
        } else transform(body)
        LuaAst.Var(variableName, LuaAst.Function(argss.flatten.map(_.name.decodedName.toString), bodyNode), mods.hasFlag(Flag.PRIVATE))
        
      case q"(..$args) => $body" => LuaAst.Function(args.map(_.name.decodedName.toString), transform(body))
        
      case q"$mods class $tname[..$tparams](...$argss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =>
        val flatArgs = argss.flatten
        val memberArgs = flatArgs.filter(!_.mods.hasFlag(Flag.LOCAL)).map(v => LuaAst.Var(v.name.decodedName.toString, LuaAst.Ref(None, v.name.decodedName.toString), false))
        val prefix = Some(tree.symbol.asClass.fullName.split("\\.").init.mkString(".")).filter(_.nonEmpty)
        val clazz = getRenamed(tree) getOrElse tname.decodedName.toString
        LuaAst.Class(prefix, clazz, flatArgs.map(_.name.decodedName.toString), LuaAst.Block(memberArgs ++ (stats map transform)), mods.hasFlag(Flag.PRIVATE))
      case q"$mods object $tname extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =>
        val clazz = getRenamed(tree).getOrElse(tname.decodedName.toString + "_MODULE")
        val prefix = Some(tree.symbol.asModule.fullName.split("\\.").init.mkString(".")).filter(_.nonEmpty)
        LuaAst.Singleton(prefix, clazz, LuaAst.Block(stats map transform), mods.hasFlag(Flag.PRIVATE))

      case q"$expr match { case ..$cases}" => patternMatch(expr, cases)

      case other => context.abort(other.pos, s"Unsupported tree: ${showRaw(other)}")
    }
  } catch {
    case util.control.NonFatal(e) => context.abort(tree.pos, s"Failed to process due to $e\n" + e.getStackTrace.take(10).mkString("\n"))
  }
  /**
   * produces a LuaAst.Ref based on the ident handling @global and @renamed annotations
   */
  private def getRenamed(tree: Tree): Option[String] = tree.symbol.annotations.find(_.tree.tpe =:= typeOf[renamed]) match {
    case Some(renamed) => 
      val List(Literal(Constant(s: String))) = renamed.tree.children.tail
      Some(s)
    case _ => None
  }
  private def luaModuleName(name: Name): String = name.decodedName.toString + "_MODULE"
  private def shouldTreatAsModule(symbol: Symbol) = symbol.isModule || (symbol.isImplicit && symbol.isType && symbol.asType.toType <:< typeOf[AnyVal])
  private def unsplicedLuaTree(tree: Tree) = context.abort(tree.pos, "LuaTree must be spliced using the method splice.")
  
  private def varDef(tree: Tree, name: String, value: Tree, local: Boolean): LuaAst.LuaTree = {
    definitions.TupleClass.seq.zipWithIndex.find(_._1 == tree.symbol.info.typeSymbol) match {
      case Some((_, idx)) =>
        val q"(..$args)" = value
        LuaAst.Var(LuaAst.Assign(Seq.tabulate(idx + 1)(i => s"${name}_${i + 1}"), LuaAst.Tuple(args map transform)), local)
      case _ => LuaAst.Var(name, transform(value), local)
    }
  }

  private def patternMatch(expr: Tree, cases: Seq[CaseDef]): LuaAst.LuaTree = {
    val No = LuaAst.NoTree
    var freshNameCounter = 0
    def nextName = {
      freshNameCounter += 1
      "x$" + freshNameCounter
    }
    /**
     * helper method to parse cases. Each pattern receives the expression over which it operates, and returns
     * a LuaTree that represents the values boundm the LuaTree representing the condition to continue with this branch, and an optional LuaTree
     * that would be setup for the condition testing.
     */
    def pattern(expr: LuaAst.LuaTree, pat: Tree): (Option[LuaAst.LuaTree], LuaAst.LuaTree) = pat match {
      case pq"_" => (None, LuaAst.Constant(true))
      case pq"$name @ $pat" =>
        val (binds, cond) = pattern(expr, pat)
        (Some(LuaAst.Block(Seq(LuaAst.Var(name.decodedName.toString, expr, true)).++(binds).filterNot(No.==))), cond)
      case pq"_: $tpt" => (None, LuaAst.InfixOperation(LuaAst.Ref(Some(expr), "__className"), "==", LuaAst.Constant(tpt.symbol.asType.fullName)))
      case pq"$first | ..$rest" =>
        val pats = (pattern(expr, first) +: rest.map(pattern(expr, _))) map (_._2)
        (None, pats.reduceLeft((a, b) => LuaAst.InfixOperation(a, "or", b)))
      case pq"$ref(..$pats)" =>
        val patsWithName = pats.map(p => p -> nextName)
        val unapplyExpr = LuaAst.Invoke(LuaAst.Ref(Some(transform(ref)), "unapply"), Seq(expr))
        val unapplyOpt = LuaAst.Var(LuaAst.Assign(nextName, unapplyExpr), true)
        //declare the extracted variables
        val unapplyTuple = LuaAst.Var(LuaAst.Assign(patsWithName.map(e => e._2), LuaAst.Constant(LuaAst.nil)), true)
        val unapplyTupleInitialize = LuaAst.Block(Seq(
            LuaAst.IfThenElse(
              cond = LuaAst.Invoke(LuaAst.Ref(Some(LuaAst.Ref(None, unapplyOpt.assign.names.head)), "isDefined"), Seq.empty),
              thenBranch = LuaAst.Var(LuaAst.Assign(patsWithName.map(e => e._2), LuaAst.Invoke(LuaAst.Ref(Some(LuaAst.Ref(None, unapplyOpt.assign.names.head)), "get"), Seq.empty)), true),
              elseBranch = LuaAst.Constant(LuaAst.nil))
          ))

        val (binds, cond) = patsWithName.map(t => pattern(LuaAst.Ref(None, t._2), t._1)).
        foldLeft[(Seq[LuaAst.LuaTree], LuaAst.LuaTree)]((Vector.empty, No)) {
          case ((bindsAggr, No), (binds, cond)) => (bindsAggr ++ binds, cond)
          case ((bindsAggr, res), (binds, cond)) => (bindsAggr ++ binds, LuaAst.InfixOperation(res, "and", cond))
        }
        (Some(LuaAst.Block(unapplyOpt +: unapplyTuple +: unapplyTupleInitialize +: binds.filterNot(No.==))), cond)
      case pq"$literal" => (None, LuaAst.InfixOperation(expr, "==", transform(literal)))
    }

    val luaExpr = transform(expr)
    cases.map { _case =>
      val (binds, pat) = pattern(luaExpr, _case.pat)
      println("guard: " + _case.guard)
      val guard = if (_case.guard != EmptyTree) transform(_case.guard) else No
      val cond = if (guard != No) LuaAst.InfixOperation(pat, "and", guard) else pat
      (binds, cond, transform(_case.body))
    }.foldRight[LuaAst.LuaTree](LuaAst.Constant(LuaAst.nil)) {
      case ((None, cond, thenBranch), elseBranch) => LuaAst.IfThenElse(cond, thenBranch, elseBranch)
      case ((Some(setup), cond, thenBranch), elseBranch) => LuaAst.Block(Seq(setup, LuaAst.IfThenElse(cond, thenBranch, elseBranch)))
    }
  }


  implicit val liftableLuaTree = new Liftable[LuaAst.LuaTree] {
    implicit val t: Liftable[LuaAst.LuaTree] = this
    def apply(node: LuaAst.LuaTree) = node match {
      case LuaAst.NoTree => q"scalua.LuaAst.NoTree"
      case LuaAst.Constant(LuaAst.nil) => q"scalua.LuaAst.Constant(scalua.LuaAst.nil)"
      case LuaAst.Constant(n) => q"scalua.LuaAst.Constant(${Literal(Constant(n))})"
      case LuaAst.LuaInlined(n) => q"scalua.LuaAst.LuaInlined(${Literal(Constant(n))})"
      case LuaAst.Tuple(v) => q"scalua.LuaAst.Tuple(Seq(..$v))"
      case LuaAst.Assign(n, v) => q"scalua.LuaAst.Assign(Seq(..$n), $v)"
      case LuaAst.Var(a, l) => q"scalua.LuaAst.Var(${a: LuaAst.LuaTree}, $l)"
      case LuaAst.Ref(prefix, name) => q"scalua.LuaAst.Ref($prefix, $name)"
      case LuaAst.Block(stats) => q"scalua.LuaAst.Block(Seq(..$stats))"
      case LuaAst.Dispatch(sym, args) => q"scalua.LuaAst.Dispatch(${sym: LuaAst.LuaTree}, Seq(..$args))"
      case LuaAst.Invoke(sym, args) => q"scalua.LuaAst.Invoke(${sym: LuaAst.LuaTree}, Seq(..$args))"
      case LuaAst.InfixOperation(l, o, r) => q"scalua.LuaAst.InfixOperation($l, $o, $r)"
      case LuaAst.UnaryOperation(o, e) => q"scalua.LuaAst.UnaryOperation($o, $e)"
      case LuaAst.IfThenElse(cond, thenB, elseB) => q"scalua.LuaAst.IfThenElse($cond, $thenB, $elseB)"
      case LuaAst.While(cond, expr) => q"scalua.LuaAst.While($cond, $expr)"
      case LuaAst.Function(args, body) => q"scalua.LuaAst.Function(Seq(..$args), $body)"
      case LuaAst.For(iteratorName, from, to, step, code) => q"scalua.LuaAst.For($iteratorName, $from, $to, $step, $code)"
      case LuaAst.Iterate(params, it, state, step, code) => q"scalua.LuaAst.Iterate(Seq(..$params), $it, $state, $step, $code)"
      case LuaAst.Class(prefix, name, params, body, local) => q"scalua.LuaAst.Class($prefix, $name, Seq(..$params), ${body: LuaAst.LuaTree}, $local)"
      case LuaAst.Singleton(prefix, name, body, local) => q"scalua.LuaAst.Singleton($prefix, $name, ${body: LuaAst.LuaTree}, $local)"
      case LuaAst.MapLiteral(entries) => q"scalua.LuaAst.MapLiteral(Seq(..$entries))"
      case LuaAst.StagedNode(tree) => tree.asInstanceOf[Tree]
    }
  }
  def transpile(tree: Tree): Tree = {
    println(s"Processing\n${show(tree)}")
    q"${transform(tree)}"
  }
}

object LuaMacro {

  def transpile(a: Any): LuaAst.LuaTree = macro transpileMacro

  def transpileMacro(c: Context)(a: c.Tree): c.Tree = new LuaTranspiler[c.type](c).transpile(a)
}