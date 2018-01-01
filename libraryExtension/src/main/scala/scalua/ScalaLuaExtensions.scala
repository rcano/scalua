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
    def foreach(f: T => Unit): Unit = ???
  }
  implicit class LuaMapExt[K, V](val map: Map[K, V]) extends AnyVal {
    def map[K2, V2](f: ((K, V)) => (K2, V2)): Map[K2, V2] = ???
    def flatMap[K2, V2](f: ((K, V)) => Map[K2, V2]): Map[K2, V2] = ???
    def withFilter(f: ((K, V)) => Boolean): Map[K, V] = ???
    def foreach(f: ((K,V)) => Unit): Unit = ???
  }
  
  val Lua = LuaMacro.transpile {
    object LuaListExt {
      def foreach[T](list: List[T], f: T => Unit): Unit = iterate(ipairs(list)).apply ((idx, value) => f(value))
      def map[T, U](list: List[T], f: T => U): List[U] = {
        val res = List[U]()
        iterate(ipairs(list)).apply { (idx, value) =>
          res(idx) = f(value)
        }
        res
      }
      def flatMap[T, U](list: List[T], f: T => List[U]): List[U] = {
        val res = List[U]()
        iterate(ipairs(list)).apply { (idx, value) =>
          iterate(ipairs(f(value))).apply { (subidx, value2) =>
            res(res.size + 1) = value2
          }
        }
        res
      }
      def withFilter[T](list: List[T], f: T => Boolean): List[T] = {
        val res = List[T]()
        iterate(ipairs(list)).apply { (idx, value) =>
          if (f(value)) {
            res(res.size + 1) = value
          }
        }
        res
      }
    }
    object LuaMapExt {
      def foreach[K, V](map: Map[K, V], f: ((K, V)) => Unit): Unit = iterate(pairs(map)).apply ((k, v) => f((k, v)))
      def map[K, V, K2, V2](map: Map[K, V], f: ((K, V)) => (K2, V2)): Map[K2, V2] = {
        val res = Map[K2, V2]()
        iterate(pairs(map)).apply { (k, v) =>
          val e2 = f((k, v))
          res(e2._1) = e2._2
        }
        res
      }
      def flatMap[K, V, K2, V2](map: Map[K, V], f: ((K, V)) => Map[K2, V2]): Map[K2, V2] = {
        val res = Map[K2, V2]()
        iterate(pairs(map)).apply((k, v) => iterate(pairs(f((k, v)))).apply((k2, v2) => res(k2) = v2))
        res
      }
      def withFilter[K, V](map: Map[K, V], f: ((K, V)) => Boolean): Map[K, V] = {
        val res = Map[K, V]()
        iterate(pairs(map)).apply { (k, v) =>
          if (f((k, v))) res(k) = v
        }
        res
      }
    }
  }
  
}
