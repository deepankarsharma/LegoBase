package ch.epfl.data

/**
 * Defines the core of the C language in Scala
 *
 * In general rather than using Pointer[Any] to represent void*, the methods
 * take a type argument which can be seen as a cast. So:
 * {{{
 * int* ptr = malloc(4 * sizeof(int))
 * }}}
 * Would be written as:
 * {{{
 * val ptr = malloc[Int](4)
 * }}}
 */
package c {
  object CLang {
    class Pointer[T]
    //TODO: structs and unions

    def break: Unit = ???
    def &[T](v: T): Pointer[T] = ???
    def *[T](v: Pointer[T]): T = ???
    def ?:[T](cond: Boolean, thenBody: => T, elseBody: => T): T = ???
    def ->[T, U](struct: Pointer[T], field: Symbol): U = ???
  }

  /**
   * <stdlib.h>
   *
   * calloc is omitted because it is too similar to malloc.
   * valloc is omitted bacause it doens't make sense to talk about pages.
   */
  object CStdLib {
    import CLang._
    def malloc[T](count: Int): Pointer[T] = ???
    def free[T](ptr: Pointer[T]) = ???

    def abort() = ???
    def exit(status: Int) = ???
  }

  /**
   * <stdio.h>
   *
   * Modes are given as case classes rather than strings for more type safety.
   *
   */
  object CStdIO {
    import CLang._
    class CFile

    abstract class Mode
    case object R extends Mode
    case object RP extends Mode
    case object W extends Mode
    case object WP extends Mode
    case object A extends Mode
    case object AP extends Mode

    def fopen(filename: String, mode: Mode): Pointer[CFile] = ???
    def popen(f: String, mode: Mode): Pointer[CFile] = ???
    def fscanf(f: Pointer[CFile], s: String, l: Pointer[Any]*): Int = ???
    def fprintf(f: Pointer[CFile], content: String) = ???
    def fread[T](dest: Pointer[T], n: Int): Int = ???
    def fwrite[T](source: Pointer[T], n: Int): Int = ???
    def feof(f: Pointer[CFile]): Boolean = ???
    def fclose(f: Pointer[CFile]): Int = ???
    def pclose(f: Pointer[CFile]): Int = ???
    def fseek(f: Pointer[CFile], offset: Long, whence: Int): Int = ???
    def fgetpos(f: Pointer[CFile], pos: Pointer[Long]): Int = ???
    def fsetpos(f: Pointer[CFile], pos: Pointer[Long]): Int = ???
  }

  /** <time.h> */
  object CTime {
    import CLang._
    class CTime

    /** Stores the current time in tloc and returns it */
    def time(tloc: Pointer[CTime]): CTime = ???
    def difftime(time1: Pointer[CTime], time2: Pointer[CTime]): Double = ???
  }

  /** <string.h> */
  object CString {
    import CLang._
    type CString = Pointer[Char]

    def memcpy[T](dst: Pointer[T], src: Pointer[T], count: Int) = ???
  }
}
