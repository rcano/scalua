package scalua

import language.dynamics
import Predef.{ ??? }

object LuaStdLib {

  def print(any: Any): Unit = Predef.println(any)
  def setmetatable(m: Map[_, _], m2: Map[_, _]) = ???
  def require(r: String): Any = ???
  def loadfile(f: String): () => Any = ???
  def loadstring(text: String): () => Any = ???

  trait Map[K, V] extends Dynamic {
    def apply(k: K): Option[V]
    def update(k: K, v: V): Unit
    def size(): Int
  }
  object Map {
    def apply[K, V](entries: (K, V)*): Map[K, V] = new Map[K, V] {
      val map = collection.mutable.Map[K, V]()
      def apply(k) = map.get(k)
      def update(k, v) = map(k) = v
      def size = map.size
    }
  }

  def cfor(from: Int, to: Int, step: Int = 1)(f: Int => Unit) = ???

  implicit class StringLib(val s: String) extends AnyVal {
    @extensionMethod
    def sub(i: Int, j: Int): String = ???
    @extensionMethod
    def sub(i: Int): String = ???

    @extensionMethod
    @renamed("rep")
    def *(i: Int): String = ???

    @extensionMethod
    def lower(): String = ???
    @extensionMethod
    def upper(): String = ???

    @extensionMethod
    def reverse(): String = ???

    @extensionMethod
    @renamed("match")
    def matches(pattern: String, index: Int) = ???
    @extensionMethod
    @renamed("match")
    def matches(pattern: String) = ???

    @extensionMethod
    def format(args: Any*) = ???

    @extensionMethod
    def gmatch(pattern: String)

  }
}
