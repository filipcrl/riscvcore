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

class CoreSpec extends AnyFunSpec with ChiselSim {

  implicit override def commonSettingsModifications: CommonSettingsModifications =
    (c: CommonCompilationSettings) =>
      c.copy(verilogPreprocessorDefines =
        c.verilogPreprocessorDefines :+ VerilogPreprocessorDefine("ENABLE_INITIAL_MEM_", "1"))

  private def hexPair(root: Path, base: String, prog: Seq[String], data: Seq[String]): (Path, Path) = {
    Files.createDirectories(root)
    val p = root.resolve(s"${base}_prog.hex")
    val d = root.resolve(s"${base}_data.hex")
    Files.write(p, prog.mkString("\n").getBytes(StandardCharsets.US_ASCII))
    Files.write(d, data.mkString("\n").getBytes(StandardCharsets.US_ASCII))
    (p.toAbsolutePath.normalize(), d.toAbsolutePath.normalize())
  }

  private def regsPeek(dut: CoreWithTaps): IndexedSeq[BigInt] =
    (0 until dut.regsTap.length).map(i => dut.regsTap(i).peek().litValue)

  private def run(prog: Seq[String], data: Seq[String], base: String)(check: IndexedSeq[BigInt] => Unit): Unit = {
    val (p, d) = hexPair(buildDir, base, prog, data)
    simulate(new CoreWithTaps(32, p.toString, Some(d.toString)),
      firtoolOpts = Array("-disable-all-randomization")) { dut =>
      val wantWaves =
        getOption("emitVcd").isDefined ||
        getOption("emitVpd").isDefined ||
        getOption("emitFsdb").isDefined

      println(wantWaves, getOption("emitVcd"), getOption("emitVpd"), getOption("emitFsdb"))

      if (wantWaves) enableWaves()

      dut.reset.poke(1); dut.clock.step(1); dut.reset.poke(0)
      dut.clock.step(60)
      check(regsPeek(dut))
    }
  }

  private def h(s: String): BigInt = BigInt(s.replace("_", ""), 16)

  describe("core") {

    it("loads") {
      val prog = Seq(
        "00000083", // lb  x1, 0(x0)
        "00100103", // lb  x2, 1(x0)
        "00200183", // lb  x3, 2(x0)
        "00300203", // lb  x4, 3(x0)
        "00001283", // lh  x5, 0(x0)
        "00201303", // lh  x6, 2(x0)
        "00002383", // lw  x7, 0(x0)
        "00005403", // lhu x8, 0(x0)
        "00205483", // lhu x9, 2(x0)
        "00004503", // lbu x10, 0(x0)
        "00104583", // lbu x11, 1(x0)
        "00204603", // lbu x12, 2(x0)
        "00304683"  // lbu x13, 3(x0)
      )
      val data = Seq(
        "80FF017F"  // mem[0..3] = 7F 01 FF 80 (LE) → 0x80FF017F
      )
      run(prog, data, "loads") { r =>
        val exp = Map(
          1 -> h("0000007F"), 2 -> h("00000001"), 3 -> h("FFFFFFFF"), 4 -> h("FFFFFF80"),
          5 -> h("0000017F"), 6 -> h("FFFF80FF"), 7 -> h("80FF017F"),
          8 -> h("0000017F"), 9 -> h("000080FF"),
          10 -> h("0000007F"), 11 -> h("00000001"), 12 -> h("000000FF"), 13 -> h("00000080")
        )
        exp.foreach { case (i, v) =>
          assert(r(i) == v, f"x$i = 0x${r(i)}%08x != 0x${v}%08x")
        }
      }
    }

    it("stores") {
      val prog = Seq(
        "00000083", // lb  x1, 0(x0)
        "00100223", // sb  x1, 4(x0)
        "00404503", // lbu x10, 4(x0)
        "00205183", // lhu x3, 2(x0)
        "00301323", // sh  x3, 6(x0)
        "00605583", // lhu x11, 6(x0)
        "00002203", // lw  x4, 0(x0)
        "00402423", // sw  x4, 8(x0)
        "00802603"  // lw  x12, 8(x0)
      )
      val data = Seq("80FF017F")
      run(prog, data, "stores") { r =>
        assert(r(10) == h("0000007f"))
        assert(r(11) == h("000080ff"))
        assert(r(12) == h("80ff017f"))
      }
    }
  }
}