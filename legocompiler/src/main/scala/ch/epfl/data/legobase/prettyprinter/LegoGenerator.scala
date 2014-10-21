package ch.epfl.data
package legobase
package prettyprinter

import pardis.utils.Document
import pardis.ir._
import pardis.prettyprinter._
import scala.language.implicitConversions

class LegoScalaGenerator(val outputFileName: String = "generatedProgram") extends ScalaCodeGenerator {

  override def getHeader: Document = s"""package ch.epfl.data
package legobase

import scala.collection.mutable.Set
import scala.collection.mutable.HashMap
import scala.collection.mutable.TreeSet
import scala.collection.mutable.ArrayBuffer
import storagemanager.K2DBScanner
import storagemanager.Loader
import queryengine.GenericEngine
import pardis.shallow.OptimalString

object OrderingFactory {
  def apply[T](fun: (T, T) => Int): Ordering[T] = new Ordering[T] {
    def compare(o1: T, o2: T) = fun(o1, o2)
  }
}
"""

  override def getTraitSignature(): Document = s"""object $outputFileName extends LegoRunner {
  def executeQuery(query: String): Unit = main()
  def main(args: Array[String]) {
    run(args)
  }
  def main() = 
  """
  //Temporary fix for def main(), check if generated code for Scala runs

  def apply(program: PardisProgram) {
    generate(program, outputFileName)
  }
}

class LegoCGenerator(val outputFileName: String = "generatedProgram", val verb: Boolean = false) extends CCodeGenerator(verb) {
  def apply(program: PardisProgram) {
    generate(program, outputFileName)
  }
}
