package core

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import chisel3.util.experimental.loadMemoryFromFileInline

/*
              +----------------+
              |                |
              |      CPU       |
              |                |
              +--------+-------+
                       |
         +-------------+--------------+
         |                            |
         v                            v
+-------------------+      +-------------------+
| Instruction Cache |      |    Data Cache     |
+---------+---------+      +---------+---------+
          |                           |
          +------------+--------------+
                       |
              +--------v--------+
              |     AXI Bus     |
              +--------+--------+
                       |
                +------v------+ 
                |   Memory    |
                +-------------+
*/

class Top(xlen: Int, sets: Int = 64) extends Module {
  val io = IO(new Bundle {
    val axi = new AXIMasterIO(32, 512, 4)
  })

  val core = Module(new Core(xlen, "", None))
  val ic   = Module(new Cache(xlen, CacheConfig(32, 64, 512, sets), readOnly = true))
  val dc   = Module(new Cache(xlen, CacheConfig(32, 64, 512, sets)))
  val arb  = Module(new AxiArbiter(2, addrBits = 32, idBits = 4, dataBits = 512))

  // Connect core to caches
  ic.io.mem <> core.io.imem
  dc.io.mem <> core.io.dmem

  // Connect caches to arbiter
  arb.io.in(0) <> ic.io.arb
  arb.io.in(1) <> dc.io.arb

  // Export AXI master
  io.axi <> arb.io.axi
}

// Simple AXI memory model with separate banks for I$ (id=0) and D$ (id=1)
class AxiSimpleMem(
  addrBits: Int = 32,
  dataBits: Int = 512,
  idBits: Int = 4,
  depthWords: Int = 65536,
  progHex: Option[String] = None,
  dataHex: Option[String] = None
) extends Module {
  require(dataBits == 512)

  val io = IO(new Bundle {
    val axi = Flipped(new AXIMasterIO(addrBits, dataBits, idBits))
  })

  val imem = Mem(depthWords, Vec(4, UInt(8.W)))
  val dmem = Mem(depthWords, Vec(4, UInt(8.W)))
  progHex.foreach(p => loadMemoryFromFileInline(imem, p))
  dataHex.foreach(p => loadMemoryFromFileInline(dmem, p))

  // Write path (single 512-bit beat)
  val wActive = RegInit(false.B)
  val wAddr   = RegInit(0.U(addrBits.W))
  val wId     = RegInit(0.U(idBits.W))
  val wLen    = RegInit(0.U(8.W))
  val wCount  = RegInit(0.U(8.W))

  io.axi.writeAddr.ready := !wActive
  when(io.axi.writeAddr.fire) {
    wActive := true.B
    wAddr   := io.axi.writeAddr.bits.addr
    wId     := io.axi.writeAddr.bits.id
    wLen    := io.axi.writeAddr.bits.len
    wCount  := 0.U
  }

  val wBankIsD = (wId =/= 0.U) // id 0=I$, others=D$

  io.axi.writeData.ready := wActive
  when(io.axi.writeData.fire) {
    val baseWord = (wAddr >> 2).asUInt // 32-bit words
    val data512 = io.axi.writeData.bits.data
    val strb64  = io.axi.writeData.bits.strb

    // Write 16 words (4 bytes each)
    for (j <- 0 until 16) {
      val wordBytes = VecInit.tabulate(4)(k => data512(32*j + 8*k + 7, 32*j + 8*k))
      val mask4     = VecInit.tabulate(4)(k => strb64(4*j + k))
      when(wBankIsD) {
        dmem.write(baseWord + j.U, wordBytes, mask4)
      }.otherwise {
        imem.write(baseWord + j.U, wordBytes, mask4)
      }
    }

    wActive := false.B // single beat
  }

  val bValid = RegInit(false.B)
  io.axi.writeResp.valid := bValid || (!wActive && (wLen === 0.U))
  io.axi.writeResp.bits.id := wId
  io.axi.writeResp.bits.resp := 0.U
  when(io.axi.writeResp.fire) { bValid := false.B }
  when(!wActive && (wLen === 0.U)) { bValid := true.B }

  // Read path (single 512-bit beat)
  val rActive = RegInit(false.B)
  val rAddr   = RegInit(0.U(addrBits.W))
  val rId     = RegInit(0.U(idBits.W))
  val rLen    = RegInit(0.U(8.W))
  val rCount  = RegInit(0.U(8.W))

  io.axi.readAddr.ready := !rActive
  when(io.axi.readAddr.fire) {
    rActive := true.B
    rAddr   := io.axi.readAddr.bits.addr
    rId     := io.axi.readAddr.bits.id
    rLen    := io.axi.readAddr.bits.len
    rCount  := 0.U
  }

  val rBankIsD = (rId =/= 0.U)
  val baseWordR = (rAddr >> 2).asUInt
  // Build 512b line from 16x32b words (little endian packing)
  val words = Wire(Vec(16, UInt(32.W)))
  for (j <- 0 until 16) {
    val vec4 = Mux(rBankIsD, dmem.read(baseWordR + j.U), imem.read(baseWordR + j.U))
    words(j) := Cat(vec4.reverse)
  }
  val rDataBeat = Cat(words.reverse)

  io.axi.readData.valid := rActive
  io.axi.readData.bits.id   := rId
  io.axi.readData.bits.data := rDataBeat
  io.axi.readData.bits.resp := 0.U
  io.axi.readData.bits.last := true.B
  when(io.axi.readData.fire) { rActive := false.B }
}

// Test harness: Top + AxiSimpleMem and register taps
class TopWithMem(xlen: Int, progHex: String, dataHex: Option[String], sets: Int = 64) extends Module {
  val top = Module(new Top(xlen, sets))
  val mem = Module(new AxiSimpleMem(32, 512, 4, depthWords = 65536, progHex = Some(progHex), dataHex = dataHex))
  mem.io.axi <> top.io.axi

  val regsTap = IO(Output(Vec(32, UInt(xlen.W))))
  regsTap := BoringUtils.tapAndRead[Vec[UInt]](top.core.regs.regs)
}
