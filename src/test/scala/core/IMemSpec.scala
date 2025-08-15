package core

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.funspec.AnyFunSpec
import java.nio.file.Files
import java.nio.charset.StandardCharsets
import chisel3.simulator.EphemeralSimulator._
import java.nio.file.{Files, Paths, Path}
import java.nio.charset.StandardCharsets
import svsim.{CommonCompilationSettings, CommonSettingsModifications, BackendSettingsModifications, Backend}
import svsim.CommonCompilationSettings.VerilogPreprocessorDefine
import svsim.verilator.Backend.CompilationSettings.{TraceStyle, TraceKind}

class IMemChiselSimSpec extends AnyFunSpec with ChiselSim {

  implicit override def commonSettingsModifications: CommonSettingsModifications =
    (common: CommonCompilationSettings) =>
      common.copy(
        verilogPreprocessorDefines =
          common.verilogPreprocessorDefines :+ VerilogPreprocessorDefine("ENABLE_INITIAL_MEM_", "1")
      )

  describe("IMem") {
    it("returns the correct word for given byte addresses") {

      val lines = Seq(
        "DEADBEEF", // @0x00
        "00000000", // @0x04
        "11223344", // @0x08
        "AABBCCDD", // @0x0C
        "0BADF00D", // @0x10
        "CAFEBABE", // @0x14
        "FEEDFACE", // @0x18
        "01020304"  // @0x1C
      )
      val tmp = Files.createTempFile("imem_", ".hex")
      Files.write(tmp, lines.mkString("\n").getBytes(StandardCharsets.US_ASCII))

      simulate(
        new IMem(32, depthWords = 8, hexPath = Some(tmp.toString)),
        firtoolOpts=Array("-disable-all-randomization")
      ) { dut =>
        def expectWord(byteAddr: Long, hex: String): Unit = {
          dut.io.addr.poke(byteAddr.U)
          dut.io.data.expect(BigInt(hex, 16).U)
        }

        expectWord(0x00, "DEADBEEF")
        expectWord(0x04, "00000000")
        expectWord(0x08, "11223344")
        expectWord(0x0C, "AABBCCDD")
        expectWord(0x10, "0BADF00D")
        expectWord(0x1C, "01020304")
      }
    }
  }
}

class CoreLbWavesSpec extends AnyFunSpec with ChiselSim {

  override def buildDir: Path = Paths.get("waves")

  implicit override def commonSettingsModifications: CommonSettingsModifications =
    (common: CommonCompilationSettings) => common.copy(
      verilogPreprocessorDefines =
        common.verilogPreprocessorDefines :+ VerilogPreprocessorDefine("ENABLE_INITIAL_MEM_", "1"))

  describe("Core: multi-instruction LB program → waveform") {
    it("runs a short LB-only program and dumps a VCD") {

      val root    = buildDir
      Files.createDirectories(root)

      // IMem program (little program of 4 LB)
      //
      // Encodings (RV32I, LB: opcode 0x03, funct3=000, rs1=x0, rd=1..4, imm=0..3):
      //   0x00000083  lb x1, 0(x0)
      //   0x00100103  lb x2, 1(x0)
      //   0x00200183  lb x3, 2(x0)
      //   0x00300203  lb x4, 3(x0)
      val progHexLines = Seq(
        "00000083",
        "00100103",
        "00200183",
        "00300203"
      )
      val progPath = root.resolve("lb_prog.hex")
      Files.write(progPath, progHexLines.mkString("\n").getBytes(StandardCharsets.US_ASCII))

      // DMem contents: one 32-bit word at address 0 with bytes [MSB..LSB] = 80 FF 01 7F
      // Byte mapping in your DMem: addr0 -> bits[7:0], addr1 -> [15:8], etc.
      // So loads see: 0x7F, 0x01, 0xFF, 0x80 (and sign-extend accordingly).
      val dataHexLines = Seq("80FF017F")
      val dataPath = root.resolve("lb_data.hex")
      Files.write(dataPath, dataHexLines.mkString("\n").getBytes(StandardCharsets.US_ASCII))

      val progAbs  = progPath.toAbsolutePath.normalize()
      val dataAbs  = dataPath.toAbsolutePath.normalize()

      // --- Run simulation and create VCD ---
      println(s"IMem hex:  $progAbs")
      println(s"DMem hex:  $dataAbs")

      // --- Run simulation and create VCD ---
      // Notes:
      //  * enableWaves() turns on dumping at time 0
      //  * pass -DemitVcd=1 to sbt/scalatest so the simulator is compiled with wave support
      //  * subdirectory keeps this test's artifacts grouped for easy discovery
      simulate(
        new Core(32, progAbs.toString, Some(dataAbs.toString)),
        firtoolOpts = Array("-disable-all-randomization"),
        subdirectory = Some("CoreLbWaves")
      ) { dut =>
        enableWaves()        // start VCD now (requires -DemitVcd=1 at runtime)
        dut.reset.poke(1)
        dut.clock.step(2)
        dut.reset.poke(0)
        dut.clock.step(12)   // enough cycles to fetch/execute the 4 LBs (PC += 4 each cycle)
        // no expects—this test is just for waveform capture
      }
    }
  }
}