package scalua

import LuaStdLib._

object PatternMatchTest extends App {

  val res = LuaMacro.transpile {
    case class SomeClass(a: String, b: Int)

    val a = SomeClass("a", 42)
    (a: Any) match {
      case any: SomeClass => print("got any " + any)
      case any: String => print("got any " + any)
      case "someLiteral" => print("someLiteral")
      case number @(41 | 42 | 43) => print(number)
    }
  }
  Predef.println(res)
}
