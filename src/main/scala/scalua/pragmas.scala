package scalua

import scala.annotation.StaticAnnotation

/**
 * Marks a method as simple invoke in lua (using the dot operator) otherwise by default, it uses dispatching with the colon operator
 * since all the methods in scala are actually methods of an instance
 */
trait invoke extends StaticAnnotation
