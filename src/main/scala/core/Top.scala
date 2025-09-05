package core

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import chisel3.util.experimental.loadMemoryFromFileInline

// Top: CPU + caches + AXI + peripherals
class Top(xlen: Int, sets: Int = 64, initFile: Option[String] = None) extends Module {
  val io = IO(new Bundle {
    val axi  = new AXIMasterIO(32, 512, 4)
    val leds = Output(UInt(4.W))
  })

  val core = Module(new Core(xlen, "", None))
  val ic   = Module(new Cache(xlen, CacheConfig(32, 64, 512, sets, initFile), readOnly = true))
  val dc   = Module(new Cache(xlen, CacheConfig(32, 64, 512, sets)))
  val arb  = Module(new AxiArbiter(2, addrBits = 32, idBits = 4, dataBits = 512))
  val per  = Module(new Peripherials(xlen))

  ic.io.mem <> core.io.imem

  val dmem      = Wire(new MemPort(xlen))
  val d2dc      = Wire(new MemPort(xlen))
  val d2per     = Wire(new MemPort(xlen))
  core.io.dmem <> dmem

  val toPeriph  = dmem.req.bits.addr(xlen-1)
  val isRead    = !dmem.req.bits.wstrb.orR

  val respFromPeriph = RegInit(false.B)
  when(dmem.req.fire && isRead) { respFromPeriph := toPeriph }

  d2dc.req.bits  := dmem.req.bits
  d2per.req.bits := dmem.req.bits
  d2dc.req.valid  := dmem.req.valid && !toPeriph
  d2per.req.valid := dmem.req.valid &&  toPeriph
  dmem.req.ready  := Mux(toPeriph, d2per.req.ready, d2dc.req.ready)

  dmem.resp.valid := Mux(respFromPeriph, d2per.resp.valid, d2dc.resp.valid)
  dmem.resp.bits  := Mux(respFromPeriph, d2per.resp.bits,  d2dc.resp.bits)
  d2per.resp.ready := dmem.resp.ready && respFromPeriph
  d2dc.resp.ready  := dmem.resp.ready && !respFromPeriph

  dc.io.mem  <> d2dc
  per.io.mem <> d2per

  arb.io.in(0) <> ic.io.arb
  arb.io.in(1) <> dc.io.arb

  io.axi <> arb.io.axi

  io.leds := per.io.leds
}

// AxiSimpleMem: simple AXI BRAM model
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

  val wBankIsD = (wId =/= 0.U)

  io.axi.writeData.ready := wActive
  when(io.axi.writeData.fire) {
    val baseWord = (wAddr >> 2).asUInt
    val data512 = io.axi.writeData.bits.data
    val strb64  = io.axi.writeData.bits.strb

    for (j <- 0 until 16) {
      val wordBytes = VecInit.tabulate(4)(k => data512(32*j + 8*k + 7, 32*j + 8*k))
      val mask4     = VecInit.tabulate(4)(k => strb64(4*j + k))
      when(wBankIsD) {
        dmem.write(baseWord + j.U, wordBytes, mask4)
      }.otherwise {
        imem.write(baseWord + j.U, wordBytes, mask4)
      }
    }

    wActive := false.B
  }

  val bValid = RegInit(false.B)
  io.axi.writeResp.valid := bValid || (!wActive && (wLen === 0.U))
  io.axi.writeResp.bits.id := wId
  io.axi.writeResp.bits.resp := 0.U
  when(io.axi.writeResp.fire) { bValid := false.B }
  when(!wActive && (wLen === 0.U)) { bValid := true.B }

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

// TopWithMem: cache-top test harness with taps
class TopWithMem(xlen: Int, progHex: String, dataHex: Option[String], sets: Int = 64) extends Module {
  val top = Module(new Top(xlen, sets))
  val mem = Module(new AxiSimpleMem(32, 512, 4, depthWords = 65536, progHex = Some(progHex), dataHex = dataHex))
  mem.io.axi <> top.io.axi

  val regsTap = IO(Output(Vec(32, UInt(xlen.W))))
  regsTap := BoringUtils.tapAndRead[Vec[UInt]](top.core.regs.regs)

  val leds = IO(Output(UInt(4.W)))
  leds := top.io.leds
}
