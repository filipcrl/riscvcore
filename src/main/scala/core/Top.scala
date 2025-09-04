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
    val axi = new AXIMasterIO(32, 64, 4)
  })

  val core = Module(new Core(xlen, "", None))
  val ic   = Module(new Cache(xlen, CacheConfig(32, 64, 512, sets), readOnly = true))
  val dc   = Module(new Cache(xlen, CacheConfig(32, 64, 512, sets)))
  val arb  = Module(new AxiArbiter(2, addrBits = 32, idBits = 4, dataBits = 64))

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
  dataBits: Int = 64,
  idBits: Int = 4,
  depthWords: Int = 65536,
  progHex: Option[String] = None,
  dataHex: Option[String] = None
) extends Module {
  require(dataBits == 64)

  val io = IO(new Bundle {
    val axi = Flipped(new AXIMasterIO(addrBits, dataBits, idBits))
  })

  val imem = Mem(depthWords, Vec(4, UInt(8.W)))
  val dmem = Mem(depthWords, Vec(4, UInt(8.W)))
  progHex.foreach(p => loadMemoryFromFileInline(imem, p))
  dataHex.foreach(p => loadMemoryFromFileInline(dmem, p))

  // Write path
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
    val baseWord = (wAddr >> 2).asUInt + (wCount << 1)
    val lowW  = io.axi.writeData.bits.data(31,0)
    val highW = io.axi.writeData.bits.data(63,32)
    val strb  = io.axi.writeData.bits.strb

    val lowVec  = VecInit.tabulate(4)(i => lowW(8*i+7, 8*i))
    val highVec = VecInit.tabulate(4)(i => highW(8*i+7, 8*i))
    val maskLow  = VecInit.tabulate(4)(i => strb(i))
    val maskHigh = VecInit.tabulate(4)(i => strb(4+i))

    when(wBankIsD) {
      dmem.write(baseWord,      lowVec,  maskLow)
      dmem.write(baseWord + 1.U, highVec, maskHigh)
    }.otherwise {
      imem.write(baseWord,      lowVec,  maskLow)
      imem.write(baseWord + 1.U, highVec, maskHigh)
    }

    wCount := wCount + 1.U
    when(io.axi.writeData.bits.last) { wActive := false.B }
  }

  val bValid = RegInit(false.B)
  io.axi.writeResp.valid := bValid || (!wActive && (wLen === 0.U))
  io.axi.writeResp.bits.id := wId
  io.axi.writeResp.bits.resp := 0.U
  when(io.axi.writeResp.fire) { bValid := false.B }
  when(!wActive && (wLen === 0.U)) { bValid := true.B }

  // Read path
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
  val curWordIndex = (rAddr >> 2).asUInt + (rCount << 1)
  val lowVec  = Mux(rBankIsD, dmem.read(curWordIndex),     imem.read(curWordIndex))
  val highVec = Mux(rBankIsD, dmem.read(curWordIndex + 1.U), imem.read(curWordIndex + 1.U))
  val lowWord  = Cat(lowVec.reverse)
  val highWord = Cat(highVec.reverse)
  val rDataBeat = Cat(highWord, lowWord)

  io.axi.readData.valid := rActive
  io.axi.readData.bits.id   := rId
  io.axi.readData.bits.data := rDataBeat
  io.axi.readData.bits.resp := 0.U
  io.axi.readData.bits.last := (rCount === rLen)
  when(io.axi.readData.fire) {
    rCount := rCount + 1.U
    when(io.axi.readData.bits.last) { rActive := false.B }
  }
}

// Test harness: Top + AxiSimpleMem and register taps
class TopWithMem(xlen: Int, progHex: String, dataHex: Option[String], sets: Int = 64) extends Module {
  val top = Module(new Top(xlen, sets))
  val mem = Module(new AxiSimpleMem(32, 64, 4, depthWords = 65536, progHex = Some(progHex), dataHex = dataHex))
  mem.io.axi <> top.io.axi

  val regsTap = IO(Output(Vec(32, UInt(xlen.W))))
  regsTap := BoringUtils.tapAndRead[Vec[UInt]](top.core.regs.regs)
}
