package core

// Inspired by
// https://github.com/maltanar/axi-in-chisel/blob/master/AXIDefs.scala

import chisel3._
import chisel3.util._

// AXI channel data definitions

class AXIAddress(addrWidthBits: Int, idBits: Int) extends Bundle {
  // address for the transaction, should be burst aligned if bursts are used
  val addr  = UInt(addrWidthBits.W)
  // size of data beat in bytes (log2 of bytes-per-beat)
  val size  = UInt(3.W)
  // number of data beats -1 in burst: max 255 for incrementing, 15 for wrapping
  val len   = UInt(8.W)
  // burst mode: 0 for fixed, 1 for incrementing, 2 for wrapping
  val burst = UInt(2.W)
  // transaction ID for multiple outstanding requests
  val id    = UInt(idBits.W)
  // set to 1 for exclusive access
  val lock  = Bool()
  // cachability, typically 0010 or 0011
  val cache = UInt(4.W)
  // generally ignored, set to all zeroes
  val prot  = UInt(3.W)
  // not implemented, set to zeroes
  val qos   = UInt(4.W)
}

class AXIWriteData(dataWidthBits: Int) extends Bundle {
  val data = UInt(dataWidthBits.W)
  val strb = UInt((dataWidthBits/8).W)
  val last = Bool()
}

class AXIWriteResponse(idBits: Int) extends Bundle {
  val id   = UInt(idBits.W)
  val resp = UInt(2.W)
}

class AXIReadData(dataWidthBits: Int, idBits: Int) extends Bundle {
  val data = UInt(dataWidthBits.W)
  val id   = UInt(idBits.W)
  val last = Bool()
  val resp = UInt(2.W)
}

class AXIMasterIO(addrWidthBits: Int, dataWidthBits: Int, idBits: Int) extends Bundle {
  // write address channel
  val writeAddr = Decoupled(new AXIAddress(addrWidthBits, idBits))
  // write data channel
  val writeData = Decoupled(new AXIWriteData(dataWidthBits))
  // write response channel (for memory consistency)
  val writeResp = Flipped(Decoupled(new AXIWriteResponse(idBits)))

  // read address channel
  val readAddr  = Decoupled(new AXIAddress(addrWidthBits, idBits))
  // read data channel
  val readData  = Flipped(Decoupled(new AXIReadData(dataWidthBits, idBits)))

  // rename signals to be compatible with those in the Xilinx template
  def renameSignals(): Unit = {
    // write address channel
    writeAddr.bits.addr.suggestName("M_AXI_AWADDR")
    writeAddr.bits.prot.suggestName("M_AXI_AWPROT")
    writeAddr.bits.size.suggestName("M_AXI_AWSIZE")
    writeAddr.bits.len.suggestName("M_AXI_AWLEN")
    writeAddr.bits.burst.suggestName("M_AXI_AWBURST")
    writeAddr.bits.lock.suggestName("M_AXI_AWLOCK")
    writeAddr.bits.cache.suggestName("M_AXI_AWCACHE")
    writeAddr.bits.qos.suggestName("M_AXI_AWQOS")
    writeAddr.bits.id.suggestName("M_AXI_AWID")
    writeAddr.valid.suggestName("M_AXI_AWVALID")
    writeAddr.ready.suggestName("M_AXI_AWREADY")
    // write data channel
    writeData.bits.data.suggestName("M_AXI_WDATA")
    writeData.bits.strb.suggestName("M_AXI_WSTRB")
    writeData.bits.last.suggestName("M_AXI_WLAST")
    writeData.valid.suggestName("M_AXI_WVALID")
    writeData.ready.suggestName("M_AXI_WREADY")
    // write response channel
    writeResp.bits.resp.suggestName("M_AXI_BRESP")
    writeResp.bits.id.suggestName("M_AXI_BID")
    writeResp.valid.suggestName("M_AXI_BVALID")
    writeResp.ready.suggestName("M_AXI_BREADY")
    // read address channel
    readAddr.bits.addr.suggestName("M_AXI_ARADDR")
    readAddr.bits.prot.suggestName("M_AXI_ARPROT")
    readAddr.bits.size.suggestName("M_AXI_ARSIZE")
    readAddr.bits.len.suggestName("M_AXI_ARLEN")
    readAddr.bits.burst.suggestName("M_AXI_ARBURST")
    readAddr.bits.lock.suggestName("M_AXI_ARLOCK")
    readAddr.bits.cache.suggestName("M_AXI_ARCACHE")
    readAddr.bits.qos.suggestName("M_AXI_ARQOS")
    readAddr.bits.id.suggestName("M_AXI_ARID")
    readAddr.valid.suggestName("M_AXI_ARVALID")
    readAddr.ready.suggestName("M_AXI_ARREADY")
    // read data channel
    readData.bits.id.suggestName("M_AXI_RID")
    readData.bits.data.suggestName("M_AXI_RDATA")
    readData.bits.resp.suggestName("M_AXI_RRESP")
    readData.bits.last.suggestName("M_AXI_RLAST")
    readData.valid.suggestName("M_AXI_RVALID")
    readData.ready.suggestName("M_AXI_RREADY")
  }
}
