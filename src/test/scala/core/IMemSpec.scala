package core

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.funspec.AnyFunSpec
import java.nio.file.Files
import java.nio.charset.StandardCharsets
import chisel3.simulator.EphemeralSimulator._
import svsim.{CommonCompilationSettings, CommonSettingsModifications}
import svsim.CommonCompilationSettings.VerilogPreprocessorDefine

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
        new IMem(depthWords = 8, hexPath = Some(tmp.toString)),
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