package scalua

import language.reflectiveCalls
import Predef.ArrowAssoc
import scala.util.chaining.*

class MyNumber(private val thingy: Int) extends AnyVal {
  def +(other: MyNumber): MyNumber = MyNumber(thingy + other.thingy)
}

@main def LuaTest = {
  import LuaStdLib._

  // val myMap = Map(1 -> "ichi", 2 -> "ni", 3 -> "san")
  // val a, b = MyNumber(3)
  // TestMacros.debugMethod(
  //   a + b
  // )

  val someBlock = LuaMacro.transpile {
    object foo {
      val hi = 34

      object bar {
        def thisIsNotFun = "what the hell"

        class CreateMe() {
          def hahah(s: String) = s + thisIsNotFun
        }
      }
    }
    print("the gall " + foo.hi)
    // 1 + 2 + 3
    print("attempt two")
    val poo = Poroto()
    class Poroto()

    val someString = "someString"
    val arraylist = require[java.util.ArrayList[String]]("somaaa")

    // val Seq(a) = Seq(1,2,3)

    (1: Any) match {
      case _ => 1
      case "hey" => 2
      case name @ "pororo" => 3
      case thing: String => 4
      case 1 | 2 => 5
      case `someString` => "pretty"
      case Some(1) | None => 6
    }
    // "result" match {
    //   case "1" =>
    // }
  }
  someBlock.pprint(using new LuaAst.PPrinter(0)) pipe Predef.println

  val tr = LuaMacro.transpile {
    var hello = "world"
    val world = hello
    hello = "what " + hello
    print("can I even do this?")
    print(4 * hello.length + 5 * 6)
    if (world.length < 3) print("\"boo\"")
    else if (world.length > 7 || world.length < 10) print("\"yay\"")
    else print("meh")

    def testMe(x: Int, y: String) = {
      print("lalala")
    }
    testMe(1, "2")

    val tuple = (1,2,3)
    print(tuple._2)

    def testMeTuple(x: Int, y: Int) = (x, y)
    val tuple2 = testMeTuple(1,2)

    for {
      a <- Seq(1, 2, 3)
      b <- Seq(4, 5, 6) if a != b
    } yield a + b

    while (!true) hello = "true"

    val myMap = Map(1 -> "ichi", 2 -> "ni", 3 -> "san")
    myMap(2)
    myMap(10) = "jyu"
    myMap.size()
  }
  print("Result:\n" + tr.pprint(new LuaAst.PPrinter(0)))
}
