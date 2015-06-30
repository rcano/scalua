package scalua 

import scala.reflect.api.Universe

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
  case class Var(name: String, value: LuaTree) extends LuaTree { def pprint(implicit p: PPrinter) = print"$name = $value" }
  case class Ref(prefix: Option[LuaTree], name: String) extends LuaTree { def pprint(implicit p: PPrinter) = print"${prefix.fold("")(_ + ".") + name}" }
  case class Block(stats: Seq[LuaTree]) extends LuaTree { def pprint(implicit p: PPrinter) = {
      stats.map(_.pprint).mkString("\n")
    }
  }
  case class Invoke(prefix: Option[LuaTree], sym: String, args: Seq[LuaTree]) extends LuaTree {
    def pprint(implicit p: PPrinter) = print"${prefix.fold("")(_ + ":")}$sym(${args.mkString(", ").replace("\n", "\n" + p.currIndent)})"
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
      print"function (${args.mkString(",")})\n$bodyStr" + print"\nend"
    }
  }
}

class LuaTranspiler[U <: Universe](val universe: U) extends Transpiler[U] {
  type Ast = LuaAst.LuaTree
  def transform(tree) = {
    import universe._
    tree match { // the order of the statements determine priority, needs to be handle with care. Incorrect order may trigger SOE
      case Literal(Constant(l)) => LuaAst.Constant(if (l == (())) LuaAst.nil else l)
      case q"var $name = $value" => LuaAst.Var(name.decodedName.toString, transform(value))
      case q"val $name = $value" => LuaAst.Var(name.decodedName.toString, transform(value))
      case q"$name = $value" => LuaAst.Var(transform(name).toString, transform(value))

      case Block(stats, expr) => LuaAst.Block((stats :+ expr) map transform)
      case Ident(name) => LuaAst.Ref(None, name.decodedName.toString)
      case q"$clazz.this.$select" => LuaAst.Ref(None, "this." + select.decodedName)
      case q"$ident.$select" => LuaAst.Ref(Some(transform(ident)), select.decodedName.toString)
      case q"$prefix.$method(...$args)" => LuaAst.Invoke(Some(transform(prefix)), method.decodedName.toString, args.flatten map transform)
      case q"${method: TermName}(...$args)" => LuaAst.Invoke(None, method.decodedName.toString, args.flatten map transform)
      case q"if ($cond) $thenBranch else $elseBranch" => LuaAst.IfThenElse(transform(cond), transform(thenBranch), transform(elseBranch))
      case q"while ($cond) $expr" => LuaAst.While(transform(cond), transform(expr))
      case q"def $name[..$tparams](...$argss): $ret = $body" => LuaAst.Var(name.decodedName.toString, LuaAst.Function(argss.flatten.map(_.name.decodedName.toString), transform(body)))
      case q"(..$args) => $body" => LuaAst.Function(args.map(_.name.decodedName.toString), transform(body))
      case q"class $tpname[..$tparams](...$params) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" => LuaAst.Block(stats.map(transform))


      case other => throw new UnsupportedOperationException(s"Tree\n${showRaw(other)}\n is not supported")
    }
  }
}

object LuaTest extends App {
  import scala.reflect.runtime.universe._

  val transpiler = new LuaTranspiler[scala.reflect.runtime.universe.type](scala.reflect.runtime.universe)

  val tree = reify {
    var hello = "world"
    val world = hello
    world.stripSuffix("hahah")
    println("can I even do this?")
    4 * hello.length + 5 * 6
    if (world.length < 3) println("boo") 
    else if (world.length > 7) println("yay")
    else println("meh")

    def testMe(x: Int, y: String) = {
      println("lalala")
      y * x
    }

    for {
      a <- Seq(1, 2, 3)
      b <- Seq(4, 5, 6) if a != b
    } yield a + b

    while (!true) hello = "true"

    class SomeClassHere(a: Int, b: Int) {
      val c = a * b
    }

  }.tree
  println(s"transforming\n$tree")
  val tr = transpiler.transform(tree)
  println("Result:\n" + tr.pprint(new LuaAst.PPrinter(0)))
}
