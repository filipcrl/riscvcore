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

    it("shifts") {
      val prog = Seq(
        "00002083", // lw   x1, 0(x0)      ; x1 = 0x80000001
        "00402103", // lw   x2, 4(x0)      ; x2 = 31
        "002091b3", // sll  x3, x1, x2     ; 0x80000001 << 31 -> 0x80000000
        "0020d233", // srl  x4, x1, x2     ; 0x80000001 >>> 31 -> 0x00000001
        "4020d2b3", // sra  x5, x1, x2     ; 0x80000001 >>s 31 -> 0xFFFFFFFF
        "00109313", // slli x6, x1, 1      ; 0x80000001 << 1  -> 0x00000002
        "0010d393", // srli x7, x1, 1      ; 0x80000001 >>> 1 -> 0x40000000
        "4010d413"  // srai x8, x1, 1      ; 0x80000001 >>s 1  -> 0xC0000000
      )
      val data = Seq(
        "80000001", // mem[0..3] = 01 00 00 80 (LE)
        "0000001F"  // mem[4..7] = 1F 00 00 00 (31)
      )
      run(prog, data, "shifts") { r =>
        val exp = Map(
          1 -> h("80000001"),
          2 -> h("0000001F"),
          3 -> h("80000000"),
          4 -> h("00000001"),
          5 -> h("FFFFFFFF"),
          6 -> h("00000002"),
          7 -> h("40000000"),
          8 -> h("C0000000")
        )
        exp.foreach { case (i, v) =>
          assert(r(i) == v, f"x$i = 0x${r(i)}%08x != 0x${v}%08x")
        }
      }
    }

    it("add_sub_lui_auipc") {
      val prog = Seq(
        "12345537", // lui   x10, 0x12345      ; x10 = 0x12345000
        "02a00093", // addi  x1,  x0, 42       ; x1  = 42
        "fff00113", // addi  x2,  x0, -1       ; x2  = -1 (0xFFFFFFFF)
        "002081b3", // add   x3,  x1, x2       ; 42 + (-1) = 41
        "40208233", // sub   x4,  x1, x2       ; 42 - (-1) = 43
        "00001597"  // auipc x11, 0x1          ; x11 = PC(0x14) + 0x1000 = 0x1014
      )
      val data = Seq("00000000")
      run(prog, data, "add_sub_lui_auipc") { r =>
        val exp = Map(
          10 -> h("12345000"), // LUI
          1  -> h("0000002A"), // ADDI +42
          2  -> h("FFFFFFFF"), // ADDI -1
          3  -> h("00000029"), // ADD
          4  -> h("0000002B"), // SUB
          11 -> h("00001014")  // AUIPC at PC=0x14
        )
        exp.foreach { case (i, v) =>
          assert(r(i) == v, f"x$i = 0x${r(i)}%08x != 0x${v}%08x")
        }
      }
    }
  }
}