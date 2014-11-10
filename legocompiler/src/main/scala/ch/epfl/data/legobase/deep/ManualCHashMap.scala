package ch.epfl.data
package legobase
package deep

import pardis.ir._
import pardis.types.PardisTypeImplicits._
import pardis.deep.scalalib._
import pardis.deep.scalalib.collection._
import pardis.shallow.c.CLangTypes
import pardis.shallow.c.GLibTypes._
import pardis.optimization._

trait ManualHMO extends RecursiveRuleBasedTransformer[LoweringLegoBase] {
  import IR._

  def shouldUseDirectKeys[K, V](node: HashMapNew[K, V]) = {
    implicit val ktype = transformType(node.typeA).asInstanceOf[TypeRep[K]]
    implicit val vtype = transformType(node.typeB).asInstanceOf[TypeRep[V]]
    typeRep[K].isPrimitive || typeRep[K] == typeRep[String] || typeRep[K] == typeRep[OptimalString]
  }

  object HashMapNew1 {
    def unapply[T: TypeRep](node: Def[T]): Option[Def[T]] = node match {
      case _ => None
    }
  }

  object HashMapNew2 {
    def unapply[T: TypeRep](node: Def[T]): Boolean = node match {
      case n @ HashMapNew() if shouldUseDirectKeys(n) => true
      case _ => false
    }
  }

  rewrite += rule {
    case nm @ HashMapNew3(_, _) =>
      apply(HashMapNew()(nm.typeA, ArrayBufferType(nm.typeB)))
    case nm @ HashMapNew4(_, _) =>
      apply(HashMapNew()(nm.typeA, nm.typeB))
  }

  class K
  class V

  rewrite += rule {
    case node @ HashMapNew() if !shouldUseDirectKeys(node) =>
      implicit val ktype = transformType(node.typeA).asInstanceOf[TypeRep[K]]
      implicit val vtype = transformType(node.typeB).asInstanceOf[TypeRep[V]]
      def hashFunc[T: TypeRep] =
        doLambda[gconstpointer, Int]((s: Rep[gconstpointer]) => {
          infix_hashCode[T](infix_asInstanceOf[T](s))
        })
      def equalsFunc[T: TypeRep] =
        doLambda2[gconstpointer, gconstpointer, Int]((s1: Rep[gconstpointer], s2: Rep[gconstpointer]) => {
          infix_==[T, T](infix_asInstanceOf[T](s1), infix_asInstanceOf[T](s2)).asInstanceOf[Rep[Int]]
        })

      LGHashTableHeader.g_hash_table_new(CLang.&(hashFunc[LPointer[K]]), CLang.&(equalsFunc[LPointer[K]]))
  }
}

// Additional manual changes needed:
//
//  - Replace self.<init> with __newCHashMap in HashMapNew2 case
//  - Remove the unit(()) at the end of the HashMapNew2 case
//  - Remove (evidence$3, evidence$4) at the end of the HashMapNew2 case
//  - Extend ManualHMO

class ManualHashMapOptimizations(override val IR: LoweringLegoBase) extends RecursiveRuleBasedTransformer[LoweringLegoBase](IR) {
  import IR._
  import CTypes._

  type Rep[T] = IR.Rep[T]

  // Dummy classes K and V
  class K
  class V

  override def transformType[T: TypeRep]: TypeRep[Any] = ({
    val tp = typeRep[T]
    tp match {
      case HashMapType(t1, t2)   => typeLPointer(typeLGHashTable)
      case ArrayBufferType(args) => typePointer(typeGArray(transformType(args)))
      case _                     => super.transformType[T]
    }
  }).asInstanceOf[TypeRep[Any]]

  //FIXME: need to do something about the typeclasses
  implicit def ctype[T] = null.asInstanceOf[CLangTypes.CType[T]]

  implicit class HashMapRep1[K, V](self: Rep[HashMap[K, V]]) {
    def gHashTable: Rep[LPointer[LGHashTable]] = self.asInstanceOf[Rep[LPointer[LGHashTable]]]
  }

  rewrite += rule {
    case nm @ HashMapNew3(_, _) =>
      apply(HashMapNew()(nm.typeA, ArrayBufferType(nm.typeB)))
    case nm @ HashMapNew4(_, _) =>
      apply(HashMapNew()(nm.typeA, nm.typeB))
  }

  rewrite += rule {
    case node @ HashMapNew() =>
      implicit val ktype = transformType(node.typeA).asInstanceOf[TypeRep[K]]
      implicit val vtype = transformType(node.typeB).asInstanceOf[TypeRep[V]]
      def hashFunc[T: TypeRep] =
        doLambda[gconstpointer, Int]((s: Rep[gconstpointer]) => {
          infix_hashCode[T](infix_asInstanceOf[T](s))
        })
      def equalsFunc[T: TypeRep] =
        doLambda2[gconstpointer, gconstpointer, Int]((s1: Rep[gconstpointer], s2: Rep[gconstpointer]) => {
          infix_==[T, T](infix_asInstanceOf[T](s1), infix_asInstanceOf[T](s2)).asInstanceOf[Rep[Int]]
        })

      if (typeRep[K].isPrimitive
        || typeRep[K] == typeRep[String]
        || typeRep[K] == typeRep[OptimalString])
        LGHashTableHeader.g_hash_table_new(CLang.&(hashFunc[K]), CLang.&(equalsFunc[K]))
      else
        LGHashTableHeader.g_hash_table_new(CLang.&(hashFunc[LPointer[K]]), CLang.&(equalsFunc[LPointer[K]]))
  }

  //def update(k: K, v: V) = {
  //  g_hash_table_insert(gHashTable, as[gconstpointer](k), as[gconstpointer](v))
  //}
  rewrite += rule {
    case node @ HashMapUpdate(s, k, v) =>
      val self = s.asInstanceOf[Rep[HashMap[K, V]]]
      val key = k.asInstanceOf[Rep[K]]
      val value = v.asInstanceOf[Rep[V]]
      System.out.println(s"Self type: ${self.tp}")
      implicit val ktype = transformType(self.tp.typeArguments(0)).asInstanceOf[TypeRep[K]]
      implicit val vtype = transformType(self.tp.typeArguments(1)).asInstanceOf[TypeRep[V]]

      LGHashTableHeader.g_hash_table_insert(self.gHashTable, infix_asInstanceOf[gconstpointer](key), infix_asInstanceOf[gconstpointer](value))
  }

  //def apply(k: K): V = {
  //  as[V](g_hash_table_lookup(gHashTable, as[gconstpointer](k)))
  //}
  rewrite += rule {
    case node @ HashMapApply(s, k) =>
      val self = s.asInstanceOf[Rep[HashMap[K, V]]]
      val key = k.asInstanceOf[Rep[K]]
      System.out.println(s"Self type: ${self.tp}")
      implicit val ktype = transformType(self.tp.typeArguments(0)).asInstanceOf[TypeRep[K]]
      implicit val vtype = transformType(self.tp.typeArguments(1)).asInstanceOf[TypeRep[V]]

      infix_asInstanceOf[V](LGHashTableHeader.g_hash_table_lookup(self.gHashTable, infix_asInstanceOf[gconstpointer](key)))
  }

  //def size: Int = g_hash_table_size(gHashTable)
  rewrite += rule {
    case node @ HashMapSize(s) =>
      val self = s.asInstanceOf[Rep[HashMap[K, V]]]
      System.out.println(s"Self type: ${self.tp}")
      implicit val ktype = transformType(self.tp.typeArguments(0)).asInstanceOf[TypeRep[K]]
      implicit val vtype = transformType(self.tp.typeArguments(1)).asInstanceOf[TypeRep[V]]

      LGHashTableHeader.g_hash_table_size(self.gHashTable)
  }

  def __newCSet[T](set: Rep[LPointer[LGList[T]]]) = set

  //def keySet: CSet[K] = {
  //  new CSet[K](g_hash_table_get_keys(gHashTable).asInstanceOf[LPointer[LGList[K]]])
  //}
  rewrite += rule {
    case node @ HashMapKeySet(s) =>
      val self = s.asInstanceOf[Rep[HashMap[K, V]]]
      System.out.println(s"Self type: ${self.tp}")
      implicit val ktype = transformType(self.tp.typeArguments(0)).asInstanceOf[TypeRep[K]]
      implicit val vtype = transformType(self.tp.typeArguments(1)).asInstanceOf[TypeRep[V]]

      __newCSet(LGHashTableHeader.g_hash_table_get_keys(self.gHashTable))
  }

  //def contains(k: K): Boolean = {
  //  g_hash_table_lookup(gHashTable, as[gconstpointer](k)) != NULL[Any]
  //}
  rewrite += rule {
    case node @ HashMapContains(s, k) =>
      val self = s.asInstanceOf[Rep[HashMap[K, V]]]
      val key = k.asInstanceOf[Rep[K]]
      System.out.println(s"Self type: ${self.tp}")
      implicit val ktype = transformType(self.tp.typeArguments(0)).asInstanceOf[TypeRep[K]]
      implicit val vtype = transformType(self.tp.typeArguments(1)).asInstanceOf[TypeRep[V]]

      infix_!=(LGHashTableHeader.g_hash_table_lookup(self.gHashTable, infix_asInstanceOf[gconstpointer](key)), CLang.NULL[Any])
  }

  //def remove(k: K) = {
  //  val v = apply(k)
  //  g_hash_table_remove(gHashTable, as[gconstpointer](k))
  //  v
  //}
  rewrite += rule {
    case node @ HashMapRemove(s, k) =>
      System.out.println(s"HASHMAPREMOVE")
      System.out.println(s"K from node: ${node.typeA}")
      System.out.println(s"V from node: ${node.typeB}")
      System.out.println(s"self type: ${s.tp}")
      val self = s.asInstanceOf[Rep[HashMap[K, V]]]
      val key = k.asInstanceOf[Rep[K]]
      System.out.println(s"Self type: ${self.tp}")
      implicit val ktype = transformType(self.tp.typeArguments(0)).asInstanceOf[TypeRep[K]]
      implicit val vtype = transformType(self.tp.typeArguments(1)).asInstanceOf[TypeRep[V]]

      val v = self.apply(key)
      LGHashTableHeader.g_hash_table_remove(self.gHashTable, infix_asInstanceOf[gconstpointer](key))
      v
  }

  //def getOrElseUpdate(k: K, v: => V): V = {
  //  val res = g_hash_table_lookup(gHashTable, as[gconstpointer](k))
  //  if (res != NULL[V]) as[V](res)
  //  else {
  //    this(k) = v
  //    v
  //  }
  //}
  rewrite += rule {
    case node @ HashMapGetOrElseUpdate(s, k, v) =>
      val self = s.asInstanceOf[Rep[HashMap[K, V]]]
      val key = k.asInstanceOf[Rep[K]]
      val value = v.asInstanceOf[Block[V]]
      System.out.println(s"Self type: ${self.tp}")
      implicit val ktype = transformType(self.tp.typeArguments(0)).asInstanceOf[TypeRep[K]]
      implicit val vtype = transformType(self.tp.typeArguments(1)).asInstanceOf[TypeRep[V]]

      val res = LGHashTableHeader.g_hash_table_lookup(self.gHashTable, infix_asInstanceOf[gconstpointer](key))
      __ifThenElse(infix_!=(res, CLang.NULL[Any]), infix_asInstanceOf[V](res), {
        val v = inlineBlock(value)
        self(key) = v
        v
      })
  }
}
