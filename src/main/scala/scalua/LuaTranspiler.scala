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
    def pprint(implicit p: PPrinter) = 
      if (value != NoTree) p.pprint(s"${names.mkString(", ")} = $value")
      else  p.pprint(s"${names.mkString(", ")}")
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

object LuaMacro {

  inline def transpile(inline a: Any): LuaAst.LuaTree = ${ transpileMacro('a) }

  def transpileMacro(a: Expr[Any])(using Quotes): Expr[LuaAst.LuaTree] = new impl.LuaTranspiler().transpile(a)
}
