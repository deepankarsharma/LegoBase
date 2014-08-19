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
import storagemanager.K2DBScanner

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

  class TestTransformer(override val IR: ScalaToC) extends TopDownTransformerTraverser[ScalaToC] {
    import CNodes._
    import CTypes._
  }

  "Transformations using yyTransform" should "make lowering to C nicer" in {
    implicit val IR = new ScalaToC {}
    val expectedTransformer = new TestTransformer(IR) {
      import IR._
      override def transformDef[T: PardisType](node: from.Def[T]): to.Def[T] = node match {
        case K2DBScannerNext_int(s) =>
          val v = readVar(__newVar[Int](0))
          __ifThenElse[Unit](infix_==(fscanf(s, unit("%d|"), &(v)), eof), break, unit(()))
          ReadVal(v)
        case _ => super.transformDef(node)
      }
    }

    val dslTransformer = new TestTransformer(IR) {
      import ch.epfl.data.pardis.ir.CTypes._
      import yyTransformer.repTtoT

      def fscanf(f: K2DBScanner, s: String, l: Any*): Int = ???
      def popen(f: String, s: Any): FILE = ???
      def pclose(f: FILE): Unit = ???
      def eof(): Int = ???
      // Control statements
      def break(): Unit = ???
      // Memory reference and allocation
      def &[T](v: T): Pointer[T] = ???
      def *[T](v: Pointer[T]): T = ???
      def malloc[T](numElems: Int): Pointer[T] = ???
      def structCopy[T](s: Pointer[T], orig: T): Unit = ???
      def gettimeofday(tv: Pointer[TimeVal]): Unit = ???
      def timeval_subtract(tv1: Pointer[TimeVal], tv2: Pointer[TimeVal], tv3: Pointer[TimeVal]): Long = ???

      override def transformDef[T: PardisType](node: from.Def[T]): to.Def[T] = node match {
        case IR.K2DBScannerNext_int(s) =>
          val v = yyTransformer.todsl {
            var allocatedV: Int = 0
            val v = allocatedV
            if (fscanf(s, "%d|", &(v)) == eof) break else ()
            v
          }
          IR.ReadVal(v)
        case _ => super.transformDef(node)
      }
    }

    val prog = IR.reifyBlock {
      val scanner = IR.k2DBScannerNew(IR.unit("filename"))
      IR.k2DBScannerNext_int(scanner)
    }

    println(s"ORIGINAL: $prog")
    println(s"EXPECTED: ${expectedTransformer(prog)}")
    println(s"ACTUAL  : ${dslTransformer(prog)}")
  }

}
