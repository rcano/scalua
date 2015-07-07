package scalua

import scala.annotation.StaticAnnotation

/**
 * Marks a method as simple invoke in lua (using the dot operator) otherwise by default, it uses dispatching with the colon operator
 * since all the methods in scala are actually methods of an instance
 */
class invoke extends StaticAnnotation

/**
 * This annotation is used when implicitly augmenting a type to introduce new methods. When this annotation is present in a method,
 * the surrounding implicit transformation is discarded and AST is generated as if the method has always been a part of the augmented type.
 */
class extensionMethod extends StaticAnnotation
