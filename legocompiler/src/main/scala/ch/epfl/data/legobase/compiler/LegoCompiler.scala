package ch.epfl.data
package legobase
package compiler

import deep._
import prettyprinter._
import optimization._
import pardis.optimization._
import pardis.ir._
import pardis.types.PardisTypeImplicits._

class LegoCompiler {

  /* For the moment this transformation is only valid for C code generation */
  val hashMapToArray = true

  def compileQuery(context: LoweringLegoBase, block: pardis.ir.PardisBlock[Unit], number: Int, shallow: Boolean, generateCCode: Boolean) {
    def writeASTToDumpFile(b0: pardis.ir.PardisBlock[Unit]) {
      val pw = new java.io.PrintWriter(new java.io.File("tree_debug_dump.txt"))
      pw.println(b0.toString)
      pw.flush()
    }

    // Lowering (e.g. case classes to records)
    val loweredBlock = {
      if (shallow) block
      else {
        val lowering = new LBLowering(context, context, generateCCode)
        val loweredBlock0 = lowering.lower(block)
        val parameterPromotion = new ParameterPromotion(context)
        parameterPromotion.optimize(loweredBlock0)
      }
    }

    val afterHashMapToArray = {
      if (hashMapToArray && generateCCode) {
        val hmHoist = new HashMapHoist(context)
        val hm2Arr = new HashMapToArrayTransformer(context)
        val afterPE = new PartiallyEvaluate(context).optimize(new DCE(context).optimize(loweredBlock))
        //writeASTToDumpFile(afterPE)
        val hmBlock = hm2Arr.optimize(hmHoist.optimize(afterPE))
        hmBlock
      } else {
        loweredBlock
      }
    }

    writeASTToDumpFile(afterHashMapToArray)

    // DCE
    val dce = new DCE(context)
    val dceBlock = dce.optimize(afterHashMapToArray)

    // Partial evaluation
    val partiallyEvaluator = new PartiallyEvaluate(context)
    val partiallyEvaluatedBlock = partiallyEvaluator.optimize(dceBlock)
    // val partiallyEvaluatedBlock = dceBlock

    // Convert Scala constructs to C
    val finalBlock = {
      if (generateCCode) {
        val cBlock = CTransformersPipeline(context, partiallyEvaluatedBlock)
        val dceC = new DCECLang(context)
        dceC.optimize(cBlock)
      } else partiallyEvaluatedBlock
    }

    // System.out.println(finalBlock)
    // Generate final program 
    val ir2Program = new { val IR = context } with IRToProgram {}
    val finalProgram = ir2Program.createProgram(finalBlock)
    if (generateCCode) (new LegoCGenerator("Q" + number, false)).apply(finalProgram)
    else (new LegoScalaGenerator("Q" + number)).apply(finalProgram)
  }
}
