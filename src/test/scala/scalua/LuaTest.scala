package scalua

import language.reflectiveCalls
import Predef.ArrowAssoc

object LuaTest extends App {
  import scala.reflect.runtime.universe._
  import LuaStdLib._

  val someBlock = LuaMacro.transpile {
    print("hello")
  }

  val tr = LuaMacro.transpile {
    var hello = "world"
    val world = hello
    hello = "what " + hello
    print("can I even do this?")
    4 * hello.length + 5 * 6
    if (world.length < 3) print("\\\"boo\"")
    else if (world.length > 7) print("\\\"yay\"")
    else print("meh")

    def testMe(x: Int, y: String) = {
      print("lalala")
    }
    testMe(1, "2")

    for {
      a <- Seq(1, 2, 3)
      b <- Seq(4, 5, 6) if a != b
    } yield a + b

    while (!true) hello = "true"

    class SomeClassHere(a: Int, val b: Int) {
      val c = a * b
      print("The value of a * b is " + c)
      def getB() = b

      object inner {
        val v = 42
      }
    }
    val i = new SomeClassHere(5, 6)
    print(i.b)
    print(i.c)
    print("i.getB is " + i.getB)
    print("inner.v = " + i.inner.v)

    object SomeObject {
      def tryMe() = "yeah"
      val someConstant = 42
    }
    print(SomeObject.someConstant)
    print(SomeObject.tryMe())

    someBlock

    cfor(1, 100, "10".length)(print)

    val myMap = Map(1 -> "ichi", 2 -> "ni", 3 -> "san")
    myMap(2)
    myMap(10) = "jyu"
    myMap.size
    myMap.asInstanceOf[{def AnotherThing: Int}].AnotherThing
  }
  print("Result:\n" + tr.pprint(new LuaAst.PPrinter(0)))
}
