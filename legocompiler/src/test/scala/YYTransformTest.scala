package ch.epfl.data
package legobase
package optimization
package test

import scala.language.implicitConversions

import org.scalatest.{ FlatSpec, ShouldMatchers, Inspectors }
import ch.epfl.data.pardis.ir._
import ch.epfl.data.pardis.ir.CNodes._
import ch.epfl.data.pardis.ir.CTypes._
import ch.epfl.data.pardis.ir.pardisTypeImplicits._
import ch.epfl.data.pardis.optimization._
import ch.epfl.yinyang._
import ch.epfl.yinyang.typetransformers._
import storagemanager.K2DBScanner

import deep._

class YYTransformTest extends FlatSpec with ShouldMatchers with Inspectors {

  def nodesOf(node: PardisNode[_]): Stream[PardisNode[_]] = node match {
    case PardisBlock(stmts, _)       => stmts.toStream.map(_.rhs).flatMap(nodesOf(_))
    case PardisIfThenElse(_, tb, eb) => Stream(node) ++ nodesOf(tb) ++ nodesOf(eb)
    case _                           => Stream(node)
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

  it should "lift while" ignore {
    /*val liftedWhile = yyTransformer.dsl {
      while (List().isEmpty) {
        1
      }
    }
    liftedWhile.isInstanceOf[ExpressionSymbol[_]] should be(true)
    forExactly(1, nodesOf(liftedWhile.correspondingNode)) { n =>
      n.isInstanceOf[PardisWhile] should be(true)
    }*/
  }

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

  import yyTransformer.{ repTtoT, defTtoT }

  class TestTransformer(override val IR: ScalaToC) extends TopDownTransformerTraverser[ScalaToC] {
    // Dummy classes for referring to case class type arguments
    class X
    class Y
    class Z
  }

  trait ShallowC {
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
    def readVar[T](v: T): T = ???

    //TODO: this doesn't really belong in ShallowC
    def newRecord[T](args: (String, Boolean, Any)*): T = ???
  }

  "Transformations using yyTransform" should "make lowering to C nicer" in {
    val IR = new ScalaToC {}

    val dslTransformer = new TestTransformer(IR) with ShallowC {
      override def transformDef[T: PardisType](node: from.Def[T]): to.Def[T] = node match {
        case IR.K2DBScannerNext_int(s) =>
          val v = yyTransformer.todsl {
            var allocatedV: Int = 0
            val v = readVar(allocatedV)
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

    nodesOf(dslTransformer(prog)).map(_.getClass) should contain inOrder (
      classOf[IR.K2DBScannerNew],
      classOf[PardisNewVar[_]],
      classOf[PardisReadVar[_]],
      classOf[PTRADDRESS[_]],
      classOf[FScanf],
      classOf[EOF],
      classOf[IR.Equal[_, _]],
      classOf[PardisIfThenElse[_]],
      classOf[Break],
      classOf[PardisReadVal[_]])
  }

  it should "cope with TypeReps" in {
    val IR = new ScalaToC {}
    val dslTransformer = new TestTransformer(IR) with ShallowC {
      override def transformDef[T: PardisType](node: from.Def[T]): to.Def[T] = (node match {
        case us @ PardisStruct(tag, elems) =>
          val s = us.asInstanceOf[PardisStruct[X]]
          implicit val stp = s.tp.asInstanceOf[PardisType[X]]

          val x = yyTransformer.todsl {
            val x = malloc[X](1)
            structCopy[X](x, s) //TODO: this annotation shouldn't be needed
            x
          }
          IR.ReadVal(x)

        case _ => super.transformDef(node)
      }).asInstanceOf[to.Def[T]]
    }

    val prog = IR.reifyBlock {
      IR.record_new[Int](Seq(("a", false, IR.unit(1)), ("b", false, IR.unit('c'))))
    }

    nodesOf(dslTransformer(prog)).map(_.getClass) should contain inOrder (
      classOf[Malloc[_]],
      classOf[PardisStruct[_]],
      classOf[StructCopy[_]],
      classOf[PardisReadVal[_]])

    forExactly(1, nodesOf(dslTransformer(prog))) { n =>
      n.getClass should be(classOf[Malloc[_]])
      n.asInstanceOf[Malloc[_]].typeT should be(typeInt)
    }
    forExactly(1, nodesOf(dslTransformer(prog))) { n =>
      n.getClass should be(classOf[PardisReadVal[_]])
      n.tp should be(typePointer(typeInt))
    }
  }

  it should "cope with more complicated TypeReps" in {
    val IR = new ScalaToC {}
    val dslTransformer = new TestTransformer(IR) with ShallowC {
      override def transformDef[T: PardisType](node: from.Def[T]): to.Def[T] = (node match {
        case ua @ IR.ArrayNew(x) =>
          // Get type of elements stored in array
          implicit val elemType = ua.typeT.asInstanceOf[PardisType[X]]
          val a = ua.asInstanceOf[IR.ArrayNew[X]]

          // Allocate original array
          val array = {
            if (typeRep[X].isPrimitive) IR.malloc[X](x)
            else IR.malloc[Pointer[X]](x)
          }
          // Create wrapper with length
          val s = IR.__new[CArray[Array[X]]](("array", false, array), ("length", false, x))
          val m = IR.malloc[CArray[Array[X]]](IR.unit(1))
          IR.structCopy(m, s)
          IR.ReadVal(m)

        //TODO: non-virtualized if-then-else
        /*val r = yyTransformer.todsl {
            // Allocate original array
            val array = {
              val prim = malloc[X](x)
              val nonPrim = malloc[Pointer[X]](x)
              if (typeRep[X].isPrimitive) prim else nonPrim
            }
            // Create wrapper with length
            val s = newRecord[CArray[Array[X]]](("array", false, array), ("length", false, x))
            val m = malloc[CArray[Array[X]]](1)
            structCopy(m, s)
            m
          }
          IR.ReadVal(r)*/

        case _ => super.transformDef(node)
      }).asInstanceOf[to.Def[T]]
    }

    val prog = IR.reifyBlock {
      IR.arrayNew[Char](IR.unit(5))
    }

    //TODO: check there is an array somewhere in there
    nodesOf(dslTransformer(prog)).map(_.getClass) should contain allOf (
      classOf[Malloc[_]],
      classOf[PardisStruct[_]],
      classOf[StructCopy[_]],
      classOf[PardisReadVal[_]])

    forExactly(1, nodesOf(dslTransformer(prog))) { n =>
      n.getClass should be(classOf[PardisReadVal[_]])
      n.tp should be(typeRep[Pointer[CArray[Array[Char]]]])
    }
  }
}
