package core

import chisel3._
import chisel3.util._

// Simple peripheral request/response interface (ready/valid)
class PeripheralReq(xlen: Int) extends Bundle {
  val addr3 = UInt(3.W)
  val wdata = UInt(xlen.W)
  val wstrb = UInt((xlen/8).W)
  val size  = UInt(2.W)
}

class PeripheralPort(xlen: Int) extends Bundle {
  val req  = Decoupled(new PeripheralReq(xlen))
  val resp = Flipped(Decoupled(UInt(xlen.W)))
}

// LEDPeripheral: 4-bit LED device (RW at subaddr 000)
class LEDPeripheral(xlen: Int) extends Module {
  val io = IO(new Bundle {
    val p = Flipped(new PeripheralPort(xlen))
    val leds = Output(UInt(4.W))
  })

  val ledReg = RegInit(0.U(4.W))
  io.leds := ledReg

  io.p.req.ready := true.B
  io.p.resp.valid := false.B
  io.p.resp.bits  := 0.U

  val rValid = RegInit(false.B)
  val rData  = RegInit(0.U(xlen.W))
  io.p.resp.valid := rValid
  io.p.resp.bits  := rData
  when(io.p.resp.fire) { rValid := false.B }

  val isWrite = io.p.req.valid && io.p.req.bits.wstrb.orR
  val isRead  = io.p.req.valid && !io.p.req.bits.wstrb.orR

  val addrIsZero = (io.p.req.bits.addr3 === 0.U)

  when(io.p.req.fire && isWrite && addrIsZero) {
    val wbyte0 = io.p.req.bits.wdata(7,0)
    when(io.p.req.bits.wstrb(0)) {
      ledReg := wbyte0(3,0)
    }
  }

  when(io.p.req.fire && isRead) {
    val rd = WireDefault(0.U(xlen.W))
    when(addrIsZero) { rd := ledReg }
    rData  := rd
    rValid := true.B
  }
}

// Peripherials: one-hot mux for peripherals; LED on bit 0
class Peripherials(xlen: Int) extends Module {
  val io = IO(new Bundle {
    val mem  = Flipped(new MemPort(xlen))
    val leds = Output(UInt(4.W))
  })

  val led = Module(new LEDPeripheral(xlen))
  io.leds := led.io.leds

  val selOH = io.mem.req.bits.addr(10, 3)
  val toLED = selOH(0)

  val respToLED = RegInit(false.B)
  val isRead    = !io.mem.req.bits.wstrb.orR
  when(io.mem.req.fire && isRead) { respToLED := toLED }

  io.mem.req.ready := false.B
  io.mem.resp.valid := false.B
  io.mem.resp.bits  := 0.U

  led.io.p.req.valid := io.mem.req.valid && toLED
  led.io.p.req.bits.addr3 := io.mem.req.bits.addr(2,0)
  led.io.p.req.bits.wdata := io.mem.req.bits.wdata
  led.io.p.req.bits.wstrb := io.mem.req.bits.wstrb
  led.io.p.req.bits.size  := io.mem.req.bits.size

  io.mem.req.ready := led.io.p.req.ready && toLED

  io.mem.resp.valid := Mux(respToLED, led.io.p.resp.valid, false.B)
  io.mem.resp.bits  := Mux(respToLED, led.io.p.resp.bits, 0.U)
  led.io.p.resp.ready := io.mem.resp.ready && respToLED
}
