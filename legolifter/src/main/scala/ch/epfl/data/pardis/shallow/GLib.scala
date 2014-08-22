package ch.epfl.data
package c

object GLib {
  import CLang._

  class GHashTable[K, V]
  class GList[T]

  // Doubly linked lists functions
  type GLPointer[T] = Pointer[GList[T]]
  type GDestroy[T] = Pointer[Pointer[T] => Unit]
  type GFunc[T] = Pointer[(Pointer[T], Any) => Unit]
  type GCompareFunc[T] = Pointer[(Pointer[T], Pointer[T]) => Int]

  def g_list_append[T](list: GLPointer[T], data: Pointer[T]): GLPointer[T] = ???
  def g_list_prepend[T](list: GLPointer[T], data: Pointer[T]): GLPointer[T] = ???
  def g_list_insert[T](list: GLPointer[T], data: Pointer[T], position: Int): GLPointer[T] = ???
  def g_list_insert_before[T](list: GLPointer[T], sibling: GLPointer[T], data: Pointer[T]): GLPointer[T] = ???
  def g_list_remove[T](list: GLPointer[T], data: Pointer[T]): GLPointer[T] = ???
  def g_list_remove_link[T](list: GLPointer[T], llink: GLPointer[T]): GLPointer[T] = ???
  def g_list_delete_link[T](list: GLPointer[T], llink: GLPointer[T]): GLPointer[T] = ???
  def g_list_remove_all[T](list: GLPointer[T], data: Pointer[T]): GLPointer[T] = ???
  def g_list_free(list: GLPointer[_]): Unit = ???
  def g_list_free_full[T](list: GLPointer[T], freeFunc: GDestroy[T]): Unit = ???
  def g_list_alloc[T](): GLPointer[T] = ???
  def g_list_length(list: GLPointer[_]): Int = ???
  def g_list_concat[T](list1: GLPointer[T], list2: GLPointer[T]): GLPointer[T] = ???
  def g_list_foreach[T](list: GLPointer[T], func: GFunc[T], userData: Any): Unit = ???
  def g_list_first[T](list: GLPointer[T]): GLPointer[T] = ???
  def g_list_last[T](list: GLPointer[T]): GLPointer[T] = ???
  def g_list_nth[T](list: GLPointer[T]): GLPointer[T] = ???
  def g_list_nth_data[T](list: GLPointer[T]): Pointer[T] = ???
  def g_list_nth_prev[T](list: GLPointer[T]): GLPointer[T] = ???
  def g_list_find[T](list: GLPointer[T], data: Pointer[T]): GLPointer[T] = ???
  def g_list_find_custom[T](list: GLPointer[T], data: Pointer[T], func: GCompareFunc[T]): GLPointer[T] = ???
  def g_list_position[T](list: GLPointer[T], llink: GLPointer[T]): Int = ???
  def g_list_index[T](list: GLPointer[T], data: Pointer[T]): Int = ???

  // Hash table functions
  type GHPointer[K, V] = Pointer[GHashTable[K, V]]
  type GHashFunc[K] = Pointer[Pointer[K] => Int]
  type GEqualFunc[K] = Pointer[(Pointer[K], Pointer[K]) => Boolean]
  type GHFunc[K, V] = Pointer[(Pointer[K], Pointer[V], Pointer[Any]) => Unit]
  type GHRFunc[K, V] = Pointer[(Pointer[K], Pointer[V], Pointer[Any]) => Boolean]

  def g_hash_table_new[K, V](hash: GHashFunc[K], equals: GEqualFunc[K]): GHPointer[K, V] = ???
  def g_hash_table_insert[K, V](ht: GHPointer[K, V], key: Pointer[K], value: Pointer[V]): Unit = ???
  def g_hash_table_replace[K, V](ht: GHPointer[K, V], key: Pointer[K], value: Pointer[V]): Unit = ???
  def g_hash_table_size(ht: GHPointer[_, _]): Int = ???
  def g_hash_table_lookup[K, V](ht: GHPointer[K, V], key: Pointer[K]): Pointer[V] = ???
  def g_hash_table_lookup_extended[K, V](ht: GHPointer[K, V], key: Pointer[K], origKey: Pointer[K], value: Pointer[V]): Boolean = ???
  def g_hash_table_foreach[K, V](ht: GHPointer[K, V], func: GHFunc[K, V], userData: Any): Unit = ???
  def g_hash_table_find[K, V](ht: GHPointer[K, V], pred: GHRFunc[K, V], userData: Any): Pointer[V] = ???
  def g_hash_table_remove[K, V](ht: GHPointer[K, V], pred: GHRFunc[K, V], userData: Any): Int = ???
  def g_hash_table_get_keys[K](ht: GHPointer[K, _]): Pointer[GList[K]] = ???
  def g_hash_table_get_values[V](ht: GHPointer[_, V]): Pointer[GList[V]] = ???
}
