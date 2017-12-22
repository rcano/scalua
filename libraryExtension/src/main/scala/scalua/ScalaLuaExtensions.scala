package scalua

import LuaStdLib._
import Predef.{ ??? }

/**
 * Object that contains definitions that enhace lua's stdlib with scala niceties such as collection as monads.
 */
@global
object ScalaLuaExtensions {
  
  implicit class LuaListExt[T](val list: List[T]) extends AnyVal {
    def map[U](f: T => U): List[U] = ???
    def flatMap[U](f: T => List[U]): List[U] = ???
    def withFilter(f: T => Boolean): List[T] = ???
  }
  
  val Lua = LuaMacro.transpile {
    implicit class LuaListExt[T](list: List[T]) {
      def map[U](f: T => U): List[U] = {
        val res = List[U]()
        iterate(ipairs(list)).apply { (idx, value) =>
          res(idx) = f(value)
        }
        res
      }
      def flatMap[U](f: T => List[U]): List[U] = {
        null
      }
      def withFilter(f: T => Boolean): List[T] = {
        null
      }
    }
  }
  
}

object QT extends App {
  import ScalaLuaExtensions._
  Predef.println(LuaMacro.transpile {
      splice(Lua)
      val res = for {
        num <- List(1,2,3)
      } yield num * 2
      print(res)
    })
}
