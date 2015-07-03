package scalua

object LuaTest extends App {
  import scala.reflect.runtime.universe._
  import LuaStdLib._

  val someBlock = LuaMacro.transpile {
    print("hello" * 3)
  }

  val tr = LuaMacro.transpile {
    var hello = "world"
    val world = hello
    hello = "what " + hello
    world.stripSuffix("suffix")
    print("can I even do this?")
    4 * hello.length + 5 * 6
    if (world.length < 3) print("\\\"boo\"")
    else if (world.length > 7) print("\\\"yay\"")
    else print("meh")

    def testMe(x: Int, y: String) = {
      print("lalala")
      y * x
    }
    testMe(1, "2")

    for {
      a <- Seq(1, 2, 3)
      b <- Seq(4, 5, 6) if a != b
    } yield a + b

    while (!true) hello = "true"

    class SomeClassHere(a: Int, b: Int) {
      val c = a * b
    }

    someBlock

    val myMap = Map(1 -> "ichi", 2 -> "ni", 3 -> "san")
    myMap(2)
    myMap(10) = "jyu"
    myMap.size
    myMap.asInstanceOf[{def AnotherThing: Int}].AnotherThing
  }
  print("Result:\n" + tr.pprint(new LuaAst.PPrinter(0)))
}
