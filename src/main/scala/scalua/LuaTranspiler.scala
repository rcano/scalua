package scalua

import language.experimental.macros
import Predef._
import scala.reflect.macros.Universe
import scala.reflect.macros.blackbox.Context

object LuaAst {
  case class PPrinter(depth: Int = 0) {
    val currIndent = "  " * depth
    def pprint(str: String) = currIndent + str
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
    } }
  case class MapLiteral(entries: Seq[(LuaTree, LuaTree)]) extends LuaTree { def pprint(implicit p: PPrinter) =  {
      val entriesStr = entries.map(e => s"[${e._1}] = ${e._2}").mkString(", ")
      print"{$entriesStr}"
    }
  }
  case class Var(name: String, value: LuaTree, local: Boolean) extends LuaTree { def pprint(implicit p: PPrinter) = print"${if (local) "local " else ""}$name = " + value.pprint.trim }
  case class Ref(prefix: Option[LuaTree], name: String) extends LuaTree { def pprint(implicit p: PPrinter) = print"${prefix.fold("")(_ + ".") + name}" }
  case class Block(stats: Seq[LuaTree]) extends LuaTree { def pprint(implicit p: PPrinter) = {
      stats.map(_.pprint).mkString("\n")
    }
  }
  /**
   * AST node represting an instance method being invoked (with the colon operator)
   */
  case class Dispatch(prefix: Option[LuaTree], sym: String, args: Seq[LuaTree]) extends LuaTree {
    def pprint(implicit p: PPrinter) = print"${prefix.fold("")(_ + ":")}$sym(${args.mkString(", ").replace("\n", "\n" + p.currIndent)})"
  }
  /**
   * AST node represeting a method invocation (with the dot operator)
   */
  case class Invoke(prefix: Option[LuaTree], sym: String, args: Seq[LuaTree]) extends LuaTree {
    def pprint(implicit p: PPrinter) = print"${prefix.fold("")(_ + ".")}$sym(${args.mkString(", ").replace("\n", "\n" + p.currIndent)})"
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
      val elseStr = if (elseBranch != Constant(nil)) s"\n${p.currIndent}else\n" +  elseBranch.pprint(p.inc) else ""
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
          case Block(sts) => sts.init.map(_.pprint).mkString("\n") + "\n" + print"return " + sts.last.pprint.trim
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
  case class Class(name: String, params: Seq[String], body: Block, local: Boolean) extends LuaTree {
    def pprint(implicit p: PPrinter) = {
      val classMembers = body.stats.map {
        case Var(name, f: Function, _) => Var(s"self.$name", f.copy(args = "self" +: f.args), false)
        case Var(name, assignment, _) => Var(s"self.$name", assignment, false)
        case s: Singleton => Block(Seq(s.copy(local = true), Var(s"self.${s.name}", Ref(None, s.name), false)))
        case other => other
      }
      val ctor = Var(s"$name.new", Function("class" +: params, Block(Seq(
              Var("self", MapLiteral(Seq.empty), true),
              Var("class.__index", Ref(None, "class"), false),
              Block(classMembers),
              Invoke(None, "setmetatable", Seq(Ref(None, "self"), Ref(None, "class")))
            ))), false)
      Block(Seq(Var(name, MapLiteral(Seq.empty), local), ctor)).pprint
    }
  }
  case class Singleton(name: String, body: Block, local: Boolean) extends LuaTree {
    def pprint(implicit p: PPrinter) = {
      LuaAst.Block(Seq(
          LuaAst.Class(s"${name}_MODULE", Seq.empty, body, true),
          LuaAst.Var(name, LuaAst.Dispatch(Some(LuaAst.Ref(None, s"${name}_MODULE")), "new", Seq.empty), false),
          LuaAst.Var(s"${name}_MODULE", LuaAst.Constant(LuaAst.nil), local)
        )).pprint
    }
  }
  case class StagedNode(tree: Universe#Tree) extends LuaTree {
    def pprint(implicit p: PPrinter) = ???
  }
}

class LuaTranspiler[C <: Context](val context: C) {
  import context.universe._
  def transform(tree: Tree): LuaAst.LuaTree = try {
    tree match { // the order of the statements determine priority, needs to be handle with care. Incorrect order may trigger SOE
      case Literal(Constant(l)) => LuaAst.Constant(if (l == (())) LuaAst.nil else l)

      case Block(stats, expr) => LuaAst.Block((stats :+ expr) map transform)
      case Ident(name) =>
        if (tree.tpe <:< typeOf[LuaAst.LuaTree]) LuaAst.StagedNode(tree)
        else LuaAst.Ref(None, name.decodedName.toString)
      case q"$clazz.this.$select" =>
        if (tree.tpe <:< typeOf[LuaAst.LuaTree]) LuaAst.StagedNode(tree)
        else {
          if (tree.symbol.asTerm.isParamAccessor && !tree.symbol.asTerm.isAccessor) LuaAst.Ref(None, select.decodedName.toString) //a pararm accessor does not need self, since its contextualized to the function
          else LuaAst.Ref(None, "self." + select.decodedName)
        }
      case Select(ident, select) =>
        if (tree.tpe <:< typeOf[LuaAst.LuaTree]) LuaAst.StagedNode(tree)
        else {
          val selected = select.decodedName.toString
          if (selected == "unary_!") LuaAst.UnaryOperation("not ", transform(ident))
          else LuaAst.Ref(Some(transform(ident)), selected)
        }

      case q"$prefix.asInstanceOf[$_]" => transform(prefix) //casting has no meaning to lua
      case q"$prefix.$method[..$tparams](...$args)" =>
        val invokedMethod = prefix.symbol match {
          case s: MethodSymbol => prefix.tpe.member(method).alternatives.find(_.isMethod).get.asMethod
          case other => prefix.tpe.member(method).alternatives.find(_.isMethod).get.asMethod
        }
        val methodName = method.decodedName.toString
        if (invokedMethod.owner == symbolOf[LuaStdLib.type]) {
          if (methodName == "cfor") {
            val Seq(from, to, step) = args.head
            val (params, body) = transform(args.tail.head.head) match {
              case LuaAst.Function(params, body) => (params, body)
              case LuaAst.Block(Seq(LuaAst.Function(params, body))) => (params, body)
            }
            LuaAst.For(params.head, transform(from), transform(to), if (step.toString.contains("cfor$default$")) LuaAst.Constant(1) else transform(step), body)
          } else LuaAst.Invoke(None, methodName, args.flatten map transform)
        } else if (invokedMethod.owner.info.baseType(symbolOf[LuaStdLib.Map[_, _]]) != NoType) {
          method.encodedName.toString match {
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
          else context.abort(tree.pos, "Unsupported String api $invokedMethod")
        } else if (methodName matches "[+-[*]/%^<>]|~=|[!<>=]=") {
          if (methodName == "!=") LuaAst.InfixOperation(transform(prefix), "~=", transform(args.head.head))
          else LuaAst.InfixOperation(transform(prefix), methodName, transform(args.head.head))
        } else {
          val target = if (invokedMethod.annotations.find(_.tree.tpe =:= typeOf[extensionMethod]).isDefined)
            prefix.collect { case q"$implicitConv($target)" => target }.headOption getOrElse prefix
          else
            prefix
          if (invokedMethod.annotations.find(_.tree.tpe =:= typeOf[invoke]).isDefined) {
            LuaAst.Invoke(Some(transform(target)), methodName, args.flatten map transform)
          } else if (invokedMethod.owner.isModuleClass) {
            LuaAst.Invoke(Some(transform(target)), methodName, args.flatten map transform)
          } else {
            LuaAst.Dispatch(Some(transform(target)), methodName, args.flatten map transform)
          }
        }


      case q"${method: TermName}[..$tparams](...$args)" => LuaAst.Invoke(None, method.decodedName.toString, args.flatten map transform)
      case q"new $clazz[..$tparams](...$args)" => LuaAst.Dispatch(Some(transform(clazz)), "new", args.flatten map transform)

      case q"$mods var $name = $value" => LuaAst.Var(name.decodedName.toString, transform(value), mods.hasFlag(Flag.PRIVATE))
      case q"$mods val $name = $value" => LuaAst.Var(name.decodedName.toString, transform(value), mods.hasFlag(Flag.PRIVATE))
      case q"$name = $value"  => LuaAst.Var(transform(name).toString, transform(value), false)

      case q"if ($cond) $thenBranch else $elseBranch" => LuaAst.IfThenElse(transform(cond), transform(thenBranch), transform(elseBranch))
      case q"while ($cond) $expr" => LuaAst.While(transform(cond), transform(expr))
      case q"$mods def $name[..$tparams](...$argss): $ret = $body" => LuaAst.Var(name.decodedName.toString, LuaAst.Function(argss.flatten.map(_.name.decodedName.toString), transform(body)), mods.hasFlag(Flag.PRIVATE))
      case q"(..$args) => $body" => LuaAst.Function(args.map(_.name.decodedName.toString), transform(body))
      case q"$mods class $tname[..$tparams](...$argss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =>
        val flatArgs = argss.flatten
        val memberArgs = flatArgs.filter(!_.mods.hasFlag(Flag.LOCAL)).map(v => LuaAst.Var(v.name.decodedName.toString, LuaAst.Ref(None, v.name.decodedName.toString), false))
        LuaAst.Class(tname.decodedName.toString, flatArgs.map(_.name.decodedName.toString), LuaAst.Block(memberArgs ++ (stats map transform)), mods.hasFlag(Flag.PRIVATE))
      case q"$mods object $tname extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =>
        val clazz = tname.decodedName.toString
        LuaAst.Singleton(clazz, LuaAst.Block(stats map transform), mods.hasFlag(Flag.PRIVATE))

      case other => context.abort(other.pos, s"Unsupported tree: ${showRaw(other)}")
    }
  } catch {
    case util.control.NonFatal(e) => context.abort(tree.pos, s"Failed to process due to $e")
  }
  def transpile(tree: Tree): Tree = {
    println(s"Processing\n$tree")
    q"${transform(tree)}"
  }

  implicit val liftableLuaTree = new Liftable[LuaAst.LuaTree] {
    implicit val t: Liftable[LuaAst.LuaTree] = this
    def apply(node) = node match {
      case LuaAst.Constant(LuaAst.nil) => q"scalua.LuaAst.Constant(scalua.LuaAst.nil)"
      case LuaAst.Constant(n) => q"scalua.LuaAst.Constant(${Literal(Constant(n))})"
      case LuaAst.Var(n, v, l) => q"scalua.LuaAst.Var($n, $v, $l)"
      case LuaAst.Ref(prefix, name) => q"scalua.LuaAst.Ref($prefix, $name)"
      case LuaAst.Block(stats) => q"scalua.LuaAst.Block(Seq(..$stats))"
      case LuaAst.Dispatch(prefix, sym, args) => q"scalua.LuaAst.Dispatch($prefix, $sym, Seq(..$args))"
      case LuaAst.Invoke(prefix, sym, args) => q"scalua.LuaAst.Invoke($prefix, $sym, Seq(..$args))"
      case LuaAst.InfixOperation(l, o, r) => q"scalua.LuaAst.InfixOperation($l, $o, $r)"
      case LuaAst.UnaryOperation(o, e) => q"scalua.LuaAst.UnaryOperation($o, $e)"
      case LuaAst.IfThenElse(cond, thenB, elseB) => q"scalua.LuaAst.IfThenElse($cond, $thenB, $elseB)"
      case LuaAst.While(cond, expr) => q"scalua.LuaAst.While($cond, $expr)"
      case LuaAst.Function(args, body) => q"scalua.LuaAst.Function(Seq(..$args), $body)"
      case LuaAst.For(iteratorName, from, to, step, code) => q"scalua.LuaAst.For($iteratorName, $from, $to, $step, $code)"
      case LuaAst.Class(name, params, body, local) => q"scalua.LuaAst.Class($name, Seq(..$params), ${body: LuaAst.LuaTree}, $local)"
      case LuaAst.Singleton(name, body, local) => q"scalua.LuaAst.Singleton($name, ${body: LuaAst.LuaTree}, $local)"
      case LuaAst.MapLiteral(entries) => q"scalua.LuaAst.MapLiteral(Seq(..$entries))"
      case LuaAst.StagedNode(tree) => tree.asInstanceOf[Tree]
    }
  }
}

object LuaMacro {

  def transpile(a: Any): LuaAst.LuaTree = macro transpileMacro

  def transpileMacro(c: Context)(a: c.Tree): c.Tree = new LuaTranspiler[c.type](c).transpile(a)
}