package core

import chisel3._
import chisel3.simulator.EphemeralSimulator._

import java.nio.file.{Files, Paths}

import svsim.{CommonCompilationSettings, CommonSettingsModifications}
import svsim.CommonCompilationSettings.VerilogPreprocessorDefine

/**
  * Simple runner for the Core simulator.
  *
  * Usage examples:
  * - sbt "runMain core.RunSim"                      // uses ./imem.hex and ./dmem.hex, 100 cycles
  * - sbt "runMain core.RunSim --cycles 200"        // override cycles
  * - sbt "runMain core.RunSim --imem foo.hex --dmem bar.hex --cycles 150"
  */
object RunSim {
  def main(args: Array[String]): Unit = {
    // Defaults
    var imemPath: String = "imem.hex"
    var dmemPath: String = "dmem.hex"
    var cycles: Int = 64

    // Very small arg parser
    val it = args.iterator
    while (it.hasNext) {
      it.next() match {
        case "--imem" if it.hasNext => imemPath = it.next()
        case "--dmem" if it.hasNext => dmemPath = it.next()
        case "--cycles" if it.hasNext => cycles = it.next().toInt
        case other =>
          System.err.println(s"Unknown or incomplete option: $other")
          System.err.println("Supported: --imem <path> --dmem <path> --cycles <n>")
          sys.exit(2)
      }
    }

    val imemAbs = Paths.get(imemPath).toAbsolutePath.normalize()
    val dmemAbs = Paths.get(dmemPath).toAbsolutePath.normalize()

    if (!Files.exists(imemAbs)) {
      System.err.println(s"imem file not found: $imemAbs")
      sys.exit(1)
    }
    if (!Files.exists(dmemAbs)) {
      System.err.println(s"dmem file not found: $dmemAbs")
      sys.exit(1)
    }

    // Ensure the Verilog initial memory blocks are enabled (needed for hex preloading)
    implicit val commonSettingsModifications: CommonSettingsModifications =
      (c: CommonCompilationSettings) =>
        c.copy(verilogPreprocessorDefines =
          c.verilogPreprocessorDefines :+ VerilogPreprocessorDefine("ENABLE_INITIAL_MEM_", "1"))

    println(s"Running CoreWithTaps with imem=$imemAbs dmem=$dmemAbs for $cycles cycles...")

    simulate(new CoreWithTaps(32, imemAbs.toString, Some(dmemAbs.toString))) { dut =>
      // Reset for 1 cycle, then run
      dut.reset.poke(1)
      dut.clock.step(1)
      dut.reset.poke(0)

      dut.clock.step(cycles)

      // Read and print registers
      println(s"Registers after $cycles cycles:")
      val n = dut.regsTap.length
      var i = 0
      while (i < n) {
        val v = dut.regsTap(i).peek().litValue.longValue & 0xFFFFFFFFL
        // x00..x31, 8-hex uppercase with 0x prefix
        println(f"x$i%02d: 0x$v%08X")
        i += 1
      }
    }
  }
}
