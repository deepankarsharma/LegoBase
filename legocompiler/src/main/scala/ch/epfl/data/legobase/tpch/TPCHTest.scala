package ch.epfl.data
package legobase
package tpch

import deep._
import prettyprinter._
import optimization._
import pardis.optimization._
import pardis.ir._
import pardis.types.PardisTypeImplicits._
import ch.epfl.data.legobase.compiler._

object TPCHTest extends LegoRunner {

  def main(args: Array[String]) {
    if (args.length < 3) {
      System.out.println("ERROR: Invalid number (" + args.length + ") of command line arguments!")
      System.out.println("USAGE: run <data_folder> <scaling_factor_number> <list of queries to run> <copy>?")
      System.out.println("     : data_folder_name should contain folders named sf0.1 sf1 sf2 sf4 etc")
      System.exit(0)
    }
    Config.checkResults = false

    run(args)
  }

  val compiler = new LegoCompiler()

  /* For the moment this transformation is only valid for C code generation */
  val hashMapToArray = true

  def executeQuery(query: String): Unit = {
    val context = new LoweringLegoBase {}
    import context._
    query match {
      case "Q1"    => compiler.compileQuery(context, reifyBlock { Queries.Q1(unit(Config.numRuns)) }, 1, false, false)
      case "Q1_C"  => compiler.compileQuery(context, reifyBlock { Queries.Q1(unit(Config.numRuns)) }, 1, false, true)
      case "Q2"    => compiler.compileQuery(context, reifyBlock { Queries.Q2(unit(Config.numRuns)) }, 2, false, false)
      case "Q2_C"  => compiler.compileQuery(context, reifyBlock { Queries.Q2(unit(Config.numRuns)) }, 2, false, true)
      case "Q3"    => compiler.compileQuery(context, reifyBlock { Queries.Q3(unit(Config.numRuns)) }, 3, false, false)
      case "Q3_C"  => compiler.compileQuery(context, reifyBlock { Queries.Q3(unit(Config.numRuns)) }, 3, false, true)
      case "Q4"    => compiler.compileQuery(context, reifyBlock { Queries.Q4(unit(Config.numRuns)) }, 4, false, false)
      case "Q4_C"  => compiler.compileQuery(context, reifyBlock { Queries.Q4(unit(Config.numRuns)) }, 4, false, true)
      case "Q5"    => compiler.compileQuery(context, reifyBlock { Queries.Q5(unit(Config.numRuns)) }, 5, false, false)
      case "Q5_C"  => compiler.compileQuery(context, reifyBlock { Queries.Q5(unit(Config.numRuns)) }, 5, false, true)
      case "Q6"    => compiler.compileQuery(context, reifyBlock { Queries.Q6(unit(Config.numRuns)) }, 6, false, false)
      case "Q6_C"  => compiler.compileQuery(context, reifyBlock { Queries.Q6(unit(Config.numRuns)) }, 6, false, true)
      case "Q7"    => compiler.compileQuery(context, reifyBlock { Queries.Q7(unit(Config.numRuns)) }, 7, false, false)
      case "Q7_C"  => compiler.compileQuery(context, reifyBlock { Queries.Q7(unit(Config.numRuns)) }, 7, false, true)
      case "Q8"    => compiler.compileQuery(context, reifyBlock { Queries.Q8(unit(Config.numRuns)) }, 8, false, false)
      case "Q8_C"  => compiler.compileQuery(context, reifyBlock { Queries.Q8(unit(Config.numRuns)) }, 8, false, true)
      case "Q9"    => compiler.compileQuery(context, reifyBlock { Queries.Q9(unit(Config.numRuns)) }, 9, false, false)
      case "Q9_C"  => compiler.compileQuery(context, reifyBlock { Queries.Q9(unit(Config.numRuns)) }, 9, false, true)
      case "Q10"   => compiler.compileQuery(context, reifyBlock { Queries.Q10(unit(Config.numRuns)) }, 10, false, false)
      case "Q10_C" => compiler.compileQuery(context, reifyBlock { Queries.Q10(unit(Config.numRuns)) }, 10, false, true)
      case "Q11"   => compiler.compileQuery(context, reifyBlock { Queries.Q11(unit(Config.numRuns)) }, 11, false, false)
      case "Q11_C" => compiler.compileQuery(context, reifyBlock { Queries.Q11(unit(Config.numRuns)) }, 11, false, true)
      case "Q12"   => compiler.compileQuery(context, reifyBlock { Queries.Q12(unit(Config.numRuns)) }, 12, false, false)
      case "Q12_C" => compiler.compileQuery(context, reifyBlock { Queries.Q12(unit(Config.numRuns)) }, 12, false, true)
      case "Q13"   => compiler.compileQuery(context, reifyBlock { Queries.Q13(unit(Config.numRuns)) }, 13, false, false)
      case "Q13_C" => compiler.compileQuery(context, reifyBlock { Queries.Q13(unit(Config.numRuns)) }, 13, false, true)
      case "Q14"   => compiler.compileQuery(context, reifyBlock { Queries.Q14(unit(Config.numRuns)) }, 14, false, false)
      case "Q14_C" => compiler.compileQuery(context, reifyBlock { Queries.Q14(unit(Config.numRuns)) }, 14, false, true)
      case "Q15"   => compiler.compileQuery(context, reifyBlock { Queries.Q15(unit(Config.numRuns)) }, 15, false, false)
      case "Q15_C" => compiler.compileQuery(context, reifyBlock { Queries.Q15(unit(Config.numRuns)) }, 15, false, true)
      case "Q16"   => compiler.compileQuery(context, reifyBlock { Queries.Q16(unit(Config.numRuns)) }, 16, false, false)
      case "Q16_C" => compiler.compileQuery(context, reifyBlock { Queries.Q16(unit(Config.numRuns)) }, 16, false, true)
      case "Q17"   => compiler.compileQuery(context, reifyBlock { Queries.Q17(unit(Config.numRuns)) }, 17, false, false)
      case "Q17_C" => compiler.compileQuery(context, reifyBlock { Queries.Q17(unit(Config.numRuns)) }, 17, false, true)
      case "Q18"   => compiler.compileQuery(context, reifyBlock { Queries.Q18(unit(Config.numRuns)) }, 18, false, false)
      case "Q18_C" => compiler.compileQuery(context, reifyBlock { Queries.Q18(unit(Config.numRuns)) }, 18, false, true)
      case "Q19"   => compiler.compileQuery(context, reifyBlock { Queries.Q19(unit(Config.numRuns)) }, 19, false, false)
      case "Q19_C" => compiler.compileQuery(context, reifyBlock { Queries.Q19(unit(Config.numRuns)) }, 19, false, true)
      case "Q20"   => compiler.compileQuery(context, reifyBlock { Queries.Q20(unit(Config.numRuns)) }, 20, false, false)
      case "Q20_C" => compiler.compileQuery(context, reifyBlock { Queries.Q20(unit(Config.numRuns)) }, 20, false, true)
      case "Q21"   => compiler.compileQuery(context, reifyBlock { Queries.Q21(unit(Config.numRuns)) }, 21, false, false)
      case "Q21_C" => compiler.compileQuery(context, reifyBlock { Queries.Q21(unit(Config.numRuns)) }, 21, false, true)
      case "Q22"   => compiler.compileQuery(context, reifyBlock { Queries.Q22(unit(Config.numRuns)) }, 22, false, false)
      case "Q22_C" => compiler.compileQuery(context, reifyBlock { Queries.Q22(unit(Config.numRuns)) }, 22, false, true)
    }
  }
}
