package ch.epfl.data
package legobase
package deep

import scala.language.experimental.macros
import scala.language.implicitConversions
import scala.language.higherKinds
import scala.reflect.macros.blackbox.Context
import scala.reflect.runtime.universe._

import ch.epfl.data.pardis.ir._
import ch.epfl.data.pardis.ir.pardisTypeImplicits._
import ch.epfl.yinyang._
import ch.epfl.yinyang.api._
import ch.epfl.yinyang.typetransformers._
import ch.epfl.yinyang.transformers._

import deep._

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

  def repTtoT[T](rep: Rep[T]): Rep[T] = rep

  override def stage[T](): T = main().asInstanceOf[T]
}

package object yyTransformer {
  type Rep[T] = Expression[T]
  type TypeRep[T] = PardisType[T]

  implicit def repTtoT[T](rep: Rep[T]): T = rep.asInstanceOf[T]
  implicit def defTtoT[T: PardisType](rep: PardisNode[T]): T = rep.asInstanceOf[T]

  class IRPostProcessing[C <: Context](ctx: C) extends PostProcessing(ctx)(Nil) {
    import c.universe._

    class IRPostProcess extends Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case q"class $className extends $parentName { ..$stmts }" =>
          q"class $className extends $parentName { ..${stmts.map(transform(_))} }"
        case tq"${ _ }.$tp[..$tpArgs]"              => tq"to.$tp[..$tpArgs]"
        case q"this.lift"                           => q"this.lift"
        case q"repTtoT"                             => q"this.repTtoT"
        case q"defTtoT"                             => q"to.toAtom"
        case q"${ _ }.this.$method"                 => q"to.$method"
        case Apply(Ident(virt @ TermName(t)), args) => q"to.$virt(..${args.map(transform(_))})"
        case _                                      => super.transform(tree)
      }
    }

    override object PostProcess extends (Tree => Tree) {
      def apply(tree: Tree) = new IRPostProcess().transform(tree)
    }
  }

  class NonNestedPardisRepTransformer[C <: Context](override val c: C) extends PardisRepTransformer[C](c) {
    import c.universe._

    override def rep(inType: Type): Tree = inType match {
      case TypeRef(_, s, args) if s.fullName == "ch.epfl.data.pardis.ir.Expression" || s.fullName == "ch.epfl.data.pardis.ir.Base.Rep" || s.fullName == "ch.epfl.data.pardis.ir.PardisStruct" =>
        AppliedTypeTree(Select(This(TypeName(className)), TypeName("Rep")), args.map(TypeTree(_)))
      case _ =>
        super.rep(inType)
    }
  }

  def dsl[T](block: => T): Rep[T] = macro _dsl[T]

  def _dsl[T](c: Context)(block: c.Expr[T]): c.Expr[Rep[T]] =
    YYTransformer[c.type, T](c)(
      "ch.epfl.data.legobase.deep.DeepYY",
      new NonNestedPardisRepTransformer[c.type](c),
      None,
      None,
      Map("shallow" -> false, "debug" -> 0, "featureAnalysing" -> false, "virtualizeLambda" -> true, "ascriptionTransforming" -> true))(block).asInstanceOf[c.Expr[Rep[T]]]

  def todsl[T](block: => T): Rep[T] = macro _todsl[T]

  def _todsl[T](c: Context)(block: c.Expr[T]): c.Expr[Rep[T]] =
    YYTransformer[c.type, T](c)(
      "ch.epfl.data.legobase.deep.DeepYY",
      new NonNestedPardisRepTransformer[c.type](c),
      Some(new IRPostProcessing(c)),
      None,
      Map("shallow" -> false, "debug" -> 0, "featureAnalysing" -> false, "virtualizeLambda" -> true, "ascriptionTransforming" -> false))(block).asInstanceOf[c.Expr[Rep[T]]]

  def fixTypeReps[D[_], X, U](node: D[_])(block: PardisType[X] => D[X] => U): U = macro _fixTypeReps[D, X, U]

  def _fixTypeReps[D[_], X, U](c: Context)(node: c.Expr[D[_]])(block: c.Expr[PardisType[X] => D[X] => U]): c.Expr[U] = {
    import c.universe._
    val q"$origNode" = node.tree
    block.tree match {
      case q"(..$params1) => (..$params2) => { ..$stmts }" =>
        val ValDef(_, _, tq"${ _ }.PardisType[$tpX]", EmptyTree) = params1.head
        val ValDef(_, _, tq"$tpD[$tpX2]", EmptyTree) = params2.head

        val tree = q"""
          ${block.tree}($origNode.typeT.asInstanceOf[PardisType[$tpX]])($origNode.asInstanceOf[$tpD[$tpX]])
        """
        c.Expr[U](tree)
    }
  }
}
