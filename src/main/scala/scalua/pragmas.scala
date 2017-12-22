package scalua

import scala.annotation.StaticAnnotation

/**
 * Marks a method as simple invoke in lua (using the dot operator) otherwise by default, it uses dispatching with the colon operator
 * since all the methods in scala are actually methods of an instance
 */
class invoke extends StaticAnnotation

/**
 * Marks a method that takes no arguments as simple field in lua.
 */
class invokeAsField extends StaticAnnotation

/**
 * This annotation is used when implicitly augmenting a type to introduce new methods. When this annotation is present in a method,
 * the surrounding implicit transformation is discarded and AST is generated as if the method has always been a part of the augmented type.
 */
class extensionMethod extends StaticAnnotation

/**
 * Renames the annotated method when produced.
 */
class renamed(value: String) extends StaticAnnotation

/**
 * Marks an object as provider of global definitions. This means that it's package and the object itself wont appear as prefixes
 */
class global extends StaticAnnotation
