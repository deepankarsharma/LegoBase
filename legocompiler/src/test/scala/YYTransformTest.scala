package ch.epfl.data
package legobase
package optimization
package test

import scala.language.implicitConversions

import org.scalatest.{ FlatSpec, ShouldMatchers, Inspectors }
import ch.epfl.data.pardis.ir._
import ch.epfl.data.pardis.ir.pardisTypeImplicits._
import ch.epfl.data.pardis.optimization._
import ch.epfl.yinyang._
import ch.epfl.yinyang.typetransformers._

import deep._

class YYTransformTest extends FlatSpec with ShouldMatchers with Inspectors {

  def nodesOf(node: PardisNode[_]): Stream[PardisNode[_]] = node match {
    case PardisBlock(stmts, _) => stmts.toStream.map(_.rhs).flatMap(nodesOf(_))
    case _                     => Stream(node)
  }

  implicit val IR: DeepDSL = new DeepDSL {}

  "DeepYY" should "use the implicit IR that is in scope" in {
    class TestDeep extends DeepYY {
      import IR._

      def main(): Any = __ifThenElse(unit(true), unit(1), unit(2))
    }
  }

  "yyTransform" should "lift constants" in {
    val liftedInt = yyTransformer.dsl(1)
    liftedInt should be(Constant(1))

    val liftedString = yyTransformer.dsl("string")
    liftedString should be(Constant("string"))

    val liftedUnit = yyTransformer.dsl({})
    liftedUnit should be(Constant(()))
  }

  it should "lift if-then-else" in {
    val liftedIfThenElse = yyTransformer.dsl {
      if (true) 1 else 2
    }
    liftedIfThenElse.isInstanceOf[ExpressionSymbol[_]] should be(true)
    forExactly(1, nodesOf(liftedIfThenElse.correspondingNode)) { n =>
      n.isInstanceOf[PardisIfThenElse[_]] should be(true)
    }
  }

  it should "lift function abstraction" in {
    val liftedFunction = yyTransformer.dsl {
      (x: Int) => x * 2
    }
    liftedFunction.isInstanceOf[ExpressionSymbol[_]] should be(true)
    forExactly(1, nodesOf(liftedFunction.correspondingNode)) { n =>
      n.isInstanceOf[PardisLambda[_, _]] should be(true)
    }
  }

  ///*it should "lift while" in {
  //  val liftedWhile = yyTransformer.dsl {
  //    while (List().isEmpty) {
  //      1
  //    }
  //  }
  //  liftedWhile.isInstanceOf[ExpressionSymbol[_]] should be(true)
  //  forExactly(1, nodesOf(liftedWhile.correspondingNode)) { n =>
  //    n.isInstanceOf[PardisWhile] should be(true)
  //  }
  //}*/

  it should "lift variables" in {
    val liftedAssign = yyTransformer.dsl {
      var x = 1
      x = 3
    }
    liftedAssign.isInstanceOf[ExpressionSymbol[_]] should be(true)
    forExactly(1, nodesOf(liftedAssign.correspondingNode)) { n =>
      n.isInstanceOf[PardisAssign[_]] should be(true)
    }
  }

  it should "allow free variables of type Rep[T]" in {
    import yyTransformer.repTtoT
    val cond: Expression[Boolean] = Constant(true)
    val liftedIfThenElse = yyTransformer.dsl {
      if (cond) 0 else 2
    }
    liftedIfThenElse.isInstanceOf[ExpressionSymbol[_]] should be(true)
    forExactly(1, nodesOf(liftedIfThenElse.correspondingNode)) { n =>
      n.isInstanceOf[PardisIfThenElse[_]] should be(true)
    }
  }
}
