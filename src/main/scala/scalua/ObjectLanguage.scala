package scalua 

import scala.reflect.api.Universe

trait Rep[T]

trait ObjectLanguage {

  type IfThenElseImplicits
  def ifThenElse[T](cond: Rep[Boolean], thenBranch: Rep[T], elseBranch: Rep[T])(implicit i: IfThenElseImplicits): Rep[T]

  type DefineValueImplicits
  def defineValue[T](name: String, mutable: Boolean)(implicit i: DefineValueImplicits): Rep[T]
}

trait Transpiler[U <: Universe] {
  type Ast

  val universe: U
  import universe._

  def transform(tree: Tree): Ast
}
