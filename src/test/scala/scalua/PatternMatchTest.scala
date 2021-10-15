package scalua

import LuaStdLib._
import scala.annotation.compileTimeOnly

object PatternMatchTest extends App {
  val res = LuaMacro.transpile {
    case class SomeClass(a: String, b: Int, nested: SomeClass)
    val a = SomeClass("a", 42, null)

    (a: Any) match {
      case SomeClass("b", 10, sm@SomeClass("what now", 20, null)) if sm.a != "b" => print("found it")
     case number: Number => print("this is supposed to be an int " + number)
     case any: String => print("got any " + any)
     case "someLiteral" => print("someLiteral")
     case number @(41 | 42 | 43) => print(number)
     case SomeClass("1", 2, null) => print("ok")
     case any: SomeClass => print("got any " + any)
     case 41 | SomeClass("a", 42, null) => "b"
    }
  }
  Predef.println(res)
}
