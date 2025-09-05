package core

import chisel3._
import chisel3.util._
import chisel3.experimental.requireIsChiselType
import chisel3.util.experimental.loadMemoryFromFileInline

/* Memory interface */
class MemReq(xlen: Int) extends Bundle {
  val addr = UInt(xlen.W)
  val wdata = UInt(xlen.W)
  val wstrb = UInt((xlen/8).W)
  val size = UInt(2.W)
}

class MemPort(xlen: Int) extends Bundle {
  val req = Decoupled(new MemReq(xlen))
  val resp = Flipped(Decoupled(UInt(xlen.W)))
}

class ArbiterReq() extends Bundle {
  val addr  = UInt(32.W)
  val wdata = UInt(512.W) // for line ops; for beat ops, use bits(63,0)
  val wstrb = UInt(64.W)   // for 512-bit beat writes
  val wen   = Bool()      // 1=write, 0=read
  val isLine = Bool()     // 1=line (8-beat) op, 0=single beat
}

class ArbiterPort() extends Bundle {
  val req = Decoupled(new ArbiterReq())
  val resp = Flipped(Decoupled(UInt(512.W)))
}

class CacheLine(val beatsPerLine: Int, val dataBits: Int, val tagBits: Int) extends Bundle {
  val valid = Bool()
  val tag   = UInt(tagBits.W)
  val data  = Vec(beatsPerLine, UInt(dataBits.W))
}

/* Memory that will be inferted to the block RAM on the FPGA */
class BRam[T <: Data](val depth: Int, gen: T, initFile: Option[String] = None) extends Module {
  requireIsChiselType(gen)
  val addrW = log2Ceil(depth)

  val io = IO(new Bundle {
    val wen   = Input(Bool())
    val waddr = Input(UInt(addrW.W))
    val wdata = Input(gen)

    val ren   = Input(Bool())
    val raddr = Input(UInt(addrW.W))
    val rdata = Output(gen)
  })

  val mem = SyncReadMem(depth, gen)
  initFile.foreach { f => loadMemoryFromFileInline(mem, f) }

  when (io.wen) { mem.write(io.waddr, io.wdata) }

  val raw = mem.read(io.raddr, io.ren)

  val bypassThisCycle = io.wen && io.ren && (io.waddr === io.raddr)
  val bypassNextCycle = RegNext(bypassThisCycle, init=false.B)
  val wdataNextCycle  = RegNext(io.wdata)

  io.rdata := Mux(bypassNextCycle, wdataNextCycle, raw)
}

case class CacheConfig(
  addrBits: Int = 32,
  dataBits: Int = 64,
  lineBits: Int = 512,
  sets: Int = 8
)

class Cache(xlen: Int, cfg: CacheConfig = CacheConfig(), readOnly: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val mem = Flipped(new MemPort(xlen))
    val arb = new ArbiterPort()
  })

  val beatsPerLine = cfg.lineBits / cfg.dataBits
  val offsetBits = log2Ceil(cfg.lineBits) - 3
  val indexBits = log2Ceil(cfg.sets)
  val tagBits = cfg.addrBits - indexBits - offsetBits

  val lineT = new CacheLine(beatsPerLine, cfg.dataBits, tagBits)
  val lines = Module(new BRam(cfg.sets, lineT))

  // Defaults
  lines.io.wen := false.B 
  lines.io.waddr := 0.U
  lines.io.wdata := 0.U
  lines.io.ren := false.B
  lines.io.raddr := 0.U
  lines.io.rdata := 0.U

  io.mem.req.ready := false.B
  io.mem.resp.valid := false.B
  io.mem.resp.bits := 0.U

  io.arb.req.valid := false.B
  io.arb.req.bits.addr  := 0.U
  io.arb.req.bits.wdata := 0.U
  io.arb.req.bits.wstrb := 0.U
  io.arb.req.bits.wen   := false.B
  io.arb.req.bits.isLine:= true.B
  io.arb.resp.ready := false.B

  // Helpers
  def idxOf(addr: UInt) = addr(offsetBits+indexBits-1, offsetBits)
  def tagOf(addr: UInt) = addr(cfg.addrBits-1, offsetBits+indexBits)
  def beatOf(addr: UInt) = addr(5,3)
  def halfSel(addr: UInt) = addr(2)

  val reqAddr = RegInit(0.U(cfg.addrBits.W))
  val reqWdata = RegInit(0.U(xlen.W))
  val reqWstrb = RegInit(0.U((xlen/8).W))
  val reqSize = RegInit(0.U(2.W))
  val reqIsWrite = RegInit(false.B)
  val rData = RegInit(0.U.asTypeOf(lineT))
  val wData = RegInit(0.U.asTypeOf(lineT))
  val hitReg = RegInit(false.B)

  // FSM
  val sWait :: sRead :: sHit :: sMiss :: sFill :: sResp :: sWriteReq :: sWriteWait :: sWriteAck :: Nil = Enum(9)
  val state = RegInit(sWait)

  switch(state) {
    is(sWait) {
      io.mem.req.ready := true.B
      when(io.mem.req.fire) {
        reqAddr := io.mem.req.bits.addr
        reqWdata := io.mem.req.bits.wdata
        reqWstrb := io.mem.req.bits.wstrb
        reqSize := io.mem.req.bits.size
        reqIsWrite := io.mem.req.bits.wstrb.orR

        when(io.mem.req.bits.wstrb.orR) {
          state := sWriteReq
        }.otherwise {
          lines.io.ren := true.B
          lines.io.raddr := idxOf(io.mem.req.bits.addr)

          state := sRead
        }
      }
    }
    is(sRead) {
      var nextRData = lines.io.rdata 
      var hit = nextRData.valid && (nextRData.tag === tagOf(reqAddr))

      rData := nextRData
      state := Mux(hit, sHit, sMiss)
    }
    is(sHit) {
      val b = beatOf(reqAddr)
      val half = halfSel(reqAddr)
      val beat = rData.data(b)
      val word = Mux(half.asBool, beat(63,32), beat(31,0))
      io.mem.resp.valid := true.B
      io.mem.resp.bits := word
      when(io.mem.resp.fire) { state := sWait }
    }
    is(sMiss) {
      io.arb.req.valid := true.B
      io.arb.req.bits.addr := Cat(reqAddr(cfg.addrBits-1, 6), 0.U(6.W))
      io.arb.req.bits.wen := false.B
      io.arb.req.bits.isLine := true.B
      when(io.arb.req.fire) { state := sFill }
    }
    is(sFill) {
      io.arb.resp.ready := true.B
      when(io.arb.resp.fire) {
        val idx = idxOf(reqAddr)
        val tag = tagOf(reqAddr)
        val line = io.arb.resp.bits
        val lineData = 0.U.asTypeOf(lineT)
        for (i <- 0 until beatsPerLine) {
          val lo = i * cfg.dataBits
          val hi = lo + cfg.dataBits - 1
          lineData.data(i) := line(hi, lo)
        }
        lineData.tag := tag
        lineData.valid := true.B

        lines.io.wen := true.B
        lines.io.waddr := idx
        lines.io.wdata := lineData

        wData := lineData
        state := sResp
      }
    }
    is(sResp) {
      val b = beatOf(reqAddr)
      val half = halfSel(reqAddr)
      val beat = wData.data(b)
      val word = Mux(half.asBool, beat(63,32), beat(31,0))
      io.mem.resp.valid := true.B
      io.mem.resp.bits := word
      when(io.mem.resp.fire) { state := sWait }
    }
  }

  if (!readOnly) {
    // Compute 512-bit aligned write data and 64-bit strobe for beat writes
    val byteOff64  = reqAddr(5,0) // 0..63 within 512-bit beat
    val wdata512   = (reqWdata.asUInt << (byteOff64 << 3)).asUInt
    val bytes      = (1.U << reqSize).asUInt // 1,2,4
    val ones64wide = ((1.U(65.W) << bytes) - 1.U)(63,0)
    val strobeMask = ((ones64wide << byteOff64)(63,0))

    switch(state) {
      is(sWriteReq) {
        // Forward 1-beat 512b write to AXI (partial via WSTRB)
        io.arb.req.valid := true.B
        io.arb.req.bits.addr := Cat(reqAddr(cfg.addrBits-1, 6), 0.U(6.W))
        io.arb.req.bits.wen := true.B
        io.arb.req.bits.isLine := false.B
        io.arb.req.bits.wdata := wdata512
        io.arb.req.bits.wstrb := strobeMask
        when(io.arb.req.fire) { state := sWriteWait }
      }
      is(sWriteWait) {
        io.arb.resp.ready := true.B
        when(io.arb.resp.fire) { state := sWriteAck }
      }
      is(sWriteAck) {
        // Update cache line on hit only
        when(hitReg) {
          val idx = idxOf(reqAddr)
          val b = beatOf(reqAddr)
          val oldBeat = lines(idx).data(b)
          val beatByteOff = reqAddr(5,3) // which 64b slot
          val localWdata64 = (wdata512 >> (beatByteOff << 6))(63,0)
          val localStrobe8 = (strobeMask >> (beatByteOff << 3))(7,0)
          val bytesVec = Wire(Vec(8, UInt(8.W)))
          for (i <- 0 until 8) {
            val sel = localStrobe8(i)
            val wbyte = localWdata64(8*i+7, 8*i)
            val obyte = oldBeat(8*i+7, 8*i)
            bytesVec(i) := Mux(sel, wbyte, obyte)
          }
          lines(idx).data(b) := Cat(bytesVec.reverse)
        }
        state := sWait
      }
    }
  }
}

class AxiArbiter(nPorts: Int, addrBits: Int = 32, idBits: Int = 4, dataBits: Int = 512) extends Module {
  require(nPorts > 0)
  require(dataBits == 512, "Arbiter is tuned for 512-bit AXI datapath")

  val io = IO(new Bundle {
    val in  = Vec(nPorts, Flipped(new ArbiterPort()))
    val axi = new AXIMasterIO(addrBits, dataBits, idBits)
  })

  // Defaults
  io.axi.writeAddr.valid := false.B
  io.axi.writeAddr.bits.addr  := 0.U
  io.axi.writeAddr.bits.size  := (log2Ceil(dataBits/8)).U // 6 for 512b
  io.axi.writeAddr.bits.len   := 0.U // single beat
  io.axi.writeAddr.bits.burst := 1.U // INCR
  io.axi.writeAddr.bits.id    := 0.U
  io.axi.writeAddr.bits.lock  := 0.U
  io.axi.writeAddr.bits.cache := 0.U
  io.axi.writeAddr.bits.prot  := 0.U
  io.axi.writeAddr.bits.qos   := 0.U

  io.axi.writeData.valid := false.B
  io.axi.writeData.bits.data := 0.U
  io.axi.writeData.bits.strb := Fill(dataBits/8, 1.U(1.W))
  io.axi.writeData.bits.last := false.B

  io.axi.writeResp.ready := false.B

  io.axi.readAddr.valid := false.B
  io.axi.readAddr.bits.addr  := 0.U
  io.axi.readAddr.bits.size  := (log2Ceil(dataBits/8)).U
  io.axi.readAddr.bits.len   := 0.U // single 512b beat
  io.axi.readAddr.bits.burst := 1.U // INCR
  io.axi.readAddr.bits.id    := 0.U
  io.axi.readAddr.bits.lock  := 0.U
  io.axi.readAddr.bits.cache := 0.U
  io.axi.readAddr.bits.prot  := 0.U
  io.axi.readAddr.bits.qos   := 0.U

  io.axi.readData.ready := false.B

  for (i <- 0 until nPorts) {
    io.in(i).req.ready := false.B
    io.in(i).resp.valid := false.B
    io.in(i).resp.bits := 0.U
  }

  // Internal state
  val selW = if (nPorts <= 1) 1 else log2Ceil(nPorts)
  val curPort  = RegInit(0.U(selW.W))
  val isWrite  = RegInit(false.B)
  val isLineOp = RegInit(false.B)
  val addrReg  = RegInit(0.U(addrBits.W))
  val beatCnt  = RegInit(0.U(3.W)) // unused for 512b single-beat
  val wstrbReg = RegInit(0.U((dataBits/8).W))

  val wlineReg = Reg(UInt(dataBits.W))
  val rlineReg = Reg(UInt(dataBits.W))

  val sIdle :: sWriteAddr :: sWriteBeats :: sWriteResp :: sWriteDone :: sReadAddr :: sReadBeats :: sReadResp :: Nil = Enum(8)
  val state = RegInit(sIdle)

  val validsVec = VecInit(Seq.tabulate(nPorts)(i => io.in(i).req.valid))
  val valids = validsVec.asUInt
  val grantOH = PriorityEncoderOH(valids)
  val haveReq = valids.orR
  val nextPort = OHToUInt(grantOH)

  switch(state) {
    is(sIdle) {
      when(haveReq) {
        val i = nextPort
        // Accept the request
        io.in(i).req.ready := true.B
        when(io.in(i).req.fire) {
          curPort  := i
          isWrite  := io.in(i).req.bits.wen
          isLineOp := io.in(i).req.bits.isLine
          // Address: align to 64B beat
          addrReg  := Cat(io.in(i).req.bits.addr(addrBits-1, 6), 0.U(6.W))
          // Capture write data (full 512b)
          wlineReg := io.in(i).req.bits.wdata
          // Beat strobe for beat write
          wstrbReg := io.in(i).req.bits.wstrb
          state := Mux(io.in(i).req.bits.wen, sWriteAddr, sReadAddr)
        }
      }
    }

    // Write path
    is(sWriteAddr) {
      io.axi.writeAddr.valid := true.B
      io.axi.writeAddr.bits.addr := addrReg
      io.axi.writeAddr.bits.id   := curPort
      when(io.axi.writeAddr.fire) {
        state := sWriteBeats
      }
    }
    is(sWriteBeats) {
      io.axi.writeData.valid := true.B
      io.axi.writeData.bits.data := wlineReg
      io.axi.writeData.bits.strb := wstrbReg
      io.axi.writeData.bits.last := true.B
      when(io.axi.writeData.fire) { state := sWriteResp }
    }
    is(sWriteResp) {
      io.axi.writeResp.ready := true.B
      when(io.axi.writeResp.fire) { state := sWriteDone }
    }
    is(sWriteDone) {
      // Ack to requester (no data payload needed)
      io.in(curPort).resp.valid := true.B
      when(io.in(curPort).resp.fire) { state := sIdle }
    }

    // Read path
    is(sReadAddr) {
      io.axi.readAddr.valid := true.B
      io.axi.readAddr.bits.addr := addrReg
      io.axi.readAddr.bits.id   := curPort
      when(io.axi.readAddr.fire) { state := sReadBeats }
    }
    is(sReadBeats) {
      io.axi.readData.ready := true.B
      when(io.axi.readData.fire) {
        rlineReg := io.axi.readData.bits.data
        when(io.axi.readData.bits.last) { state := sReadResp }
      }
    }
    is(sReadResp) {
      // Return the 512b line
      io.in(curPort).resp.valid := true.B
      io.in(curPort).resp.bits := rlineReg
      when(io.in(curPort).resp.fire) { state := sIdle }
    }
  }
}
