package scalua

import language.dynamics
import Predef.{ ??? }

object LuaStdLib {

  trait Iterator[R, State]

  def print(any: Any): Unit = Predef.println(any)
  def setmetatable(m: Map[_, _], m2: Map[_, _]) = ???
  def require(r: String): Any = ???
  def loadfile(f: String): () => Any = ???
  def loadstring(text: String): () => Any = ???
  def ipairs[V](m: Map[Int, V]): Iterator[(Int, V), Unit] = ???
  def pairs[K, V](m: Map[K, V]): Iterator[(K, V), Unit] = ???

  trait Map[K, V] {
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

  trait List[T] {
    
  }
  object List {
    def apply[T](elems: T*) = ???
  }

  def cfor(from: Int, to: Int, step: Int = 1)(f: Int => Unit): Unit = ???
  def iterate[R, State, F](iterator: Iterator[R, State], state: State = ???, init: State = ???)(implicit magnet: FunctionMagnet[R, F]): IterateApply[R, State, F] = ???
  sealed trait IterateApply[R, State, F] {
    def apply(f: F): Unit = ???
  }

  sealed trait FunctionMagnet[R, F]
  implicit def fm1[T1]: FunctionMagnet[Tuple1[T1], T1 => Unit] = ???
  implicit def fm2[T1, T2]: FunctionMagnet[(T1, T2), (T1, T2) => Unit] = ???
  implicit def fm3[T1, T2, T3]: FunctionMagnet[(T1, T2, T3), (T1, T2, T3) => Unit] = ???
  implicit def fm4[T1, T2, T3, T4]: FunctionMagnet[(T1, T2, T3, T4), (T1, T2, T3, T4) => Unit] = ???
  implicit def fm5[T1, T2, T3, T4, T5]: FunctionMagnet[(T1, T2, T3, T4, T5), (T1, T2, T3, T4, T5) => Unit] = ???
  implicit def fm6[T1, T2, T3, T4, T5, T6]: FunctionMagnet[(T1, T2, T3, T4, T5, T6), (T1, T2, T3, T4, T5, T6) => Unit] = ???
  implicit def fm7[T1, T2, T3, T4, T5, T6, T7]: FunctionMagnet[(T1, T2, T3, T4, T5, T6, T7), (T1, T2, T3, T4, T5, T6, T7) => Unit] = ???
  implicit def fm8[T1, T2, T3, T4, T5, T6, T7, T8]: FunctionMagnet[(T1, T2, T3, T4, T5, T6, T7, T8), (T1, T2, T3, T4, T5, T6, T7, T8) => Unit] = ???

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
    def format(args: Any*): String = ???

    @extensionMethod
    def gmatch(pattern: String) = ???

  }
}
