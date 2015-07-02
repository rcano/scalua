package scalua

import language.dynamics

object LuaStdLib {

  def print(any: Any): Unit = println(any)
  def setmetatable(m: Map[_, _], m2: Map[_, _]) = ???

  trait Map[K, V] extends Dynamic {
    def apply(k: K): Option[V]
    def update(k: K, v: V): Unit
  }
  object Map {
    def apply[K, V](entries: (K, V)*): Map[K, V] = new Map[K, V] {
      val map = collection.mutable.Map[K, V]()
      def apply(k) = map.get(k)
      def update(k, v) = map(k) = v
    }
  }
}
