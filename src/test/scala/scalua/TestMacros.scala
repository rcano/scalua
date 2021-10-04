package scalua

import Predef.*
import scala.quoted.*

object TestMacros {
  
  inline def debugMethod(inline a: Any) = ${debugMethodMacro('a)}
  
  private def debugMethodMacro(a: Expr[Any])(using q: Quotes) = {
    import q.reflect.*

    println("does any tuple extends Tuple?")
    println(TypeRepr.of[Tuple2[_, _]] <:< TypeRepr.of[Tuple])

    a.asTerm.asInstanceOf[Inlined].underlyingArgument match {
      case Apply(pref, args) => 
        report.info(pref.show(using Printer.TreeStructure), pref.pos)
        
        '{()}
    }
  }

}
