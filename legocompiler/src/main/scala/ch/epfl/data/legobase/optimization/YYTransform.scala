package ch.epfl.data
package legobase
package deep

import scala.language.experimental.macros
import scala.language.implicitConversions
import scala.reflect.macros.blackbox.Context
import scala.reflect.runtime.universe._

import ch.epfl.data.pardis.ir._
import ch.epfl.data.pardis.ir.pardisTypeImplicits._
import ch.epfl.yinyang._
import ch.epfl.yinyang.api._
import ch.epfl.yinyang.typetransformers._
import ch.epfl.yinyang.transformers._

import deep._
import shallow.CLibrary._

abstract class DeepYY extends DeepDSL with BaseYinYang with FullyUnstaged with Stager {
  implicit object LiftBoolean extends LiftEvidence[Boolean, Rep[Boolean]] {
    def lift(v: Boolean): Rep[Boolean] = unit(v)
    def hole(tpe: TypeRep[Boolean], symbolId: Int): Rep[Boolean] = ???
  }

  implicit object LiftInt extends LiftEvidence[Int, Rep[Int]] {
    def lift(v: Int): Rep[Int] = unit(v)
    def hole(tpe: TypeRep[Int], symbolId: Int): Rep[Int] = ???
  }

  implicit object LiftString extends LiftEvidence[Predef.String, Rep[Predef.String]] {
    def lift(v: Predef.String): Rep[String] = unit(v)
    def hole(tpe: TypeRep[Predef.String], symbolId: Int): Rep[String] = ???
  }

  implicit object LiftUnit extends LiftEvidence[Unit, Rep[Unit]] {
    def lift(v: Unit): Rep[Unit] = unit(v)
    def hole(tpe: TypeRep[Unit], symbolId: Int): Rep[Unit] = ???
  }

  def main(): Any

  implicit def repTtoT[T](rep: Rep[T]): Rep[T] = rep

  override def stage[T](): T = main().asInstanceOf[T]
}

package object yyTransformer {
  type Rep[T] = Expression[T]
  type TypeRep[T] = PardisType[T]

  implicit def repTtoT[T](rep: Rep[T]): T = rep.asInstanceOf[T]

  def dsl[T](block: => T): Rep[T] = macro _dsl[T]

  def _dsl[T](c: Context)(block: c.Expr[T]): c.Expr[Rep[T]] =
    YYTransformer[c.type, T](c)(
      "ch.epfl.data.legobase.deep.DeepYY",
      new PardisRepTransformer[c.type](c),
      None,
      None,
      Map("shallow" -> false, "debug" -> 3, "featureAnalysing" -> false, "virtualizeLambda" -> true, "ascriptionTransforming" -> false))(block).asInstanceOf[c.Expr[Rep[T]]]
}
