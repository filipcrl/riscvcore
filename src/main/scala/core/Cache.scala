package core

import chisel3._
import chisel3.util._

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
  val wstrb = UInt(8.W)   // for beat writes only
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

case class CacheConfig(
  addrBits: Int = 32,
  dataBits: Int = 64,
  lineBits: Int = 512,
  sets: Int = 32
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
  val lines = RegInit(VecInit(Seq.fill(cfg.sets)(0.U.asTypeOf(lineT))))

  // Defaults
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

  val reqAddr   = RegInit(0.U(cfg.addrBits.W))
  val reqWdata  = RegInit(0.U(xlen.W))
  val reqWstrb  = RegInit(0.U((xlen/8).W))
  val reqSize   = RegInit(0.U(2.W))
  val reqIsWrite= RegInit(false.B)
  val hitReg    = RegInit(false.B)

  // FSM
  val sWait :: sHit :: sMiss :: sFill :: sResp :: sWriteReq :: sWriteWait :: sWriteAck :: Nil = Enum(8)
  val state = RegInit(sWait)

  switch(state) {
    is(sWait) {
      io.mem.req.ready := true.B
      when(io.mem.req.fire) {
        val a = io.mem.req.bits.addr
        val i = idxOf(a)
        val t = tagOf(a)
        val h = lines(i).valid && (lines(i).tag === t)
        reqAddr   := a
        reqWdata  := io.mem.req.bits.wdata
        reqWstrb  := io.mem.req.bits.wstrb
        reqSize   := io.mem.req.bits.size
        reqIsWrite:= io.mem.req.bits.wstrb.orR
        hitReg    := h
        when(io.mem.req.bits.wstrb.orR) {
          if (readOnly) {
            assert(false.B, "Read-only cache received a write request")
          } else {
            state := sWriteReq
          }
        }.otherwise {
          state := Mux(h, sHit, sMiss)
        }
      }
    }
    is(sHit) {
      val b = beatOf(reqAddr)
      val half = halfSel(reqAddr)
      val beat = lines(idxOf(reqAddr)).data(b)
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
        for (i <- 0 until beatsPerLine) {
          val lo = i * cfg.dataBits
          val hi = lo + cfg.dataBits - 1
          lines(idx).data(i) := line(hi, lo)
        }
        lines(idx).tag := tag
        lines(idx).valid := true.B
        state := sResp
      }
    }
    is(sResp) {
      val b = beatOf(reqAddr)
      val half = halfSel(reqAddr)
      val beat = lines(idxOf(reqAddr)).data(b)
      val word = Mux(half.asBool, beat(63,32), beat(31,0))
      io.mem.resp.valid := true.B
      io.mem.resp.bits := word
      when(io.mem.resp.fire) { state := sWait }
    }
  }

  if (!readOnly) {
    def byteOffset(addr: UInt) = addr(2,0)
    val byteOff = byteOffset(reqAddr)
    val wdata64 = (reqWdata.asUInt << (byteOff << 3)).asUInt
    val bytes = (1.U << reqSize).asUInt // 1,2,4
    val ones = ((1.U(9.W) << bytes) - 1.U)(7,0)
    val strobeMask = ((ones << byteOff)(7,0))

    switch(state) {
      is(sWriteReq) {
        io.arb.req.valid := true.B
        io.arb.req.bits.addr := Cat(reqAddr(cfg.addrBits-1, 3), 0.U(3.W))
        io.arb.req.bits.wen := true.B
        io.arb.req.bits.isLine := false.B
        io.arb.req.bits.wdata := wdata64.pad(512)
        io.arb.req.bits.wstrb := strobeMask
        when(io.arb.req.fire) { state := sWriteWait }
      }
      is(sWriteWait) {
        io.arb.resp.ready := true.B
        when(io.arb.resp.fire) { state := sWriteAck }
      }
      is(sWriteAck) {
        when(hitReg) {
          val idx = idxOf(reqAddr)
          val b = beatOf(reqAddr)
          val oldBeat = lines(idx).data(b)
          val bytesVec = Wire(Vec(8, UInt(8.W)))
          for (i <- 0 until 8) {
            val sel = strobeMask(i)
            val wbyte = wdata64(8*i+7, 8*i)
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

class ICache(xlen: Int, cfg: CacheConfig = CacheConfig()) extends Module {
  require(cfg.dataBits == 64)
  require(cfg.lineBytes == 64)
  val io = IO(new Bundle {
    val mem = Flipped(new MemPort(xlen))
    val arb = new ArbiterPort()
  })

  // Derived params
  val beatBytes = cfg.dataBits/8
  val beatsPerLine = cfg.lineBytes/beatBytes // 8
  val offsetBits = log2Ceil(cfg.lineBytes)   // 6
  val indexBits  = log2Ceil(cfg.sets)
  val tagBits    = cfg.addrBits - indexBits - offsetBits

  // State: direct-mapped arrays
  val dataArray = RegInit(VecInit(Seq.fill(cfg.sets)(VecInit(Seq.fill(beatsPerLine)(0.U(cfg.dataBits.W))))))
  val tagArray  = Reg(Vec(cfg.sets, UInt(tagBits.W)))
  val validArray= RegInit(VecInit(Seq.fill(cfg.sets)(false.B)))

  // Request latching
  val reqAddr   = RegInit(0.U(cfg.addrBits.W))
  val reqActive = RegInit(false.B)

  // Defaults
  io.mem.req.ready := !reqActive
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
  def beatOf(addr: UInt) = addr(5,3) // 0..7
  def halfSel(addr: UInt) = addr(2)  // 0 lower 32b, 1 upper 32b

  when(io.mem.req.fire) {
    reqAddr := io.mem.req.bits.addr
    reqActive := true.B
  }

  val idx = idxOf(reqAddr)
  val tag = tagOf(reqAddr)
  val hit = reqActive && validArray(idx) && (tagArray(idx) === tag)

  when(reqActive) {
    when(hit) {
      val b = beatOf(reqAddr)
      val half = halfSel(reqAddr)
      val beat = dataArray(idx)(b)
      val word = Mux(half.asBool, beat(63,32), beat(31,0))
      io.mem.resp.valid := true.B
      io.mem.resp.bits := word
      when(io.mem.resp.fire) {
        reqActive := false.B
      }
    }.otherwise {
      // Miss: issue a line read to arbiter
      io.arb.req.valid := true.B
      io.arb.req.bits.addr := Cat(reqAddr(cfg.addrBits-1, 6), 0.U(6.W))
      io.arb.req.bits.wen := false.B
      io.arb.req.bits.isLine := true.B
      when(io.arb.req.fire) {
        // Wait for line fill
        io.arb.resp.ready := true.B
      }
      when(io.arb.resp.fire) {
        // Fill line
        val line = io.arb.resp.bits
        for (i <- 0 until beatsPerLine) {
          val lo = i*cfg.dataBits
          val hi = lo + cfg.dataBits - 1
          dataArray(idx)(i) := line(hi, lo)
        }
        tagArray(idx) := tag
        validArray(idx) := true.B
      }
      // After receiving line, respond to CPU
      val haveLine = WireDefault(false.B)
      haveLine := io.arb.resp.fire
      when(haveLine) {
        val b = beatOf(reqAddr)
        val half = halfSel(reqAddr)
        val beat = dataArray(idx)(b)
        val word = Mux(half.asBool, beat(63,32), beat(31,0))
        io.mem.resp.valid := true.B
        io.mem.resp.bits := word
        when(io.mem.resp.fire) { reqActive := false.B }
      }
    }
  }
}

class DCache(xlen: Int, cfg: CacheConfig = CacheConfig()) extends Module {
  require(cfg.dataBits == 64)
  require(cfg.lineBytes == 64)
  val io = IO(new Bundle {
    val mem = Flipped(new MemPort(xlen))
    val arb = new ArbiterPort()
  })

  // Derived params
  val beatBytes = cfg.dataBits/8
  val beatsPerLine = cfg.lineBytes/beatBytes // 8
  val offsetBits = log2Ceil(cfg.lineBytes)   // 6
  val indexBits  = log2Ceil(cfg.sets)
  val tagBits    = cfg.addrBits - indexBits - offsetBits

  // State: direct-mapped arrays
  val dataArray = RegInit(VecInit(Seq.fill(cfg.sets)(VecInit(Seq.fill(beatsPerLine)(0.U(cfg.dataBits.W))))))
  val tagArray  = Reg(Vec(cfg.sets, UInt(tagBits.W)))
  val validArray= RegInit(VecInit(Seq.fill(cfg.sets)(false.B)))

  // Request latching
  val reqAddr   = RegInit(0.U(cfg.addrBits.W))
  val reqWdata  = RegInit(0.U(xlen.W))
  val reqWstrb  = RegInit(0.U((xlen/8).W))
  val reqSize   = RegInit(0.U(2.W))
  val reqIsWrite= RegInit(false.B)
  val reqActive = RegInit(false.B)

  // Defaults
  io.mem.req.ready := !reqActive
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
  def beatOf(addr: UInt) = addr(5,3) // 0..7
  def wordSel(addr: UInt) = addr(2)  // 0 lower 32b, 1 upper 32b
  def byteOffset(addr: UInt) = addr(2,0) // within 64-bit beat

  when(io.mem.req.fire) {
    reqAddr := io.mem.req.bits.addr
    reqWdata := io.mem.req.bits.wdata
    reqWstrb := io.mem.req.bits.wstrb
    reqSize := io.mem.req.bits.size
    reqIsWrite := io.mem.req.bits.wstrb.orR
    reqActive := true.B
  }

  val idx = idxOf(reqAddr)
  val tag = tagOf(reqAddr)
  val hit = reqActive && validArray(idx) && (tagArray(idx) === tag)

  // Compute 64-bit aligned write data and 8-bit strobe for beat writes
  val byteOff = byteOffset(reqAddr) // 0..7
  val wdata64 = (reqWdata.asUInt << (byteOff << 3)).asUInt
  val bytes = (1.U << reqSize).asUInt // 1,2,4
  val ones = ((1.U(9.W) << bytes) - 1.U)(7,0)
  val strobeMask = ((ones << byteOff)(7,0))

  when(reqActive) {
    when(!reqIsWrite) {
      // LOAD path
      when(hit) {
        val b = beatOf(reqAddr)
        val sel = wordSel(reqAddr)
        val beat = dataArray(idx)(b)
        val word = Mux(sel.asBool, beat(63,32), beat(31,0))
        io.mem.resp.valid := true.B
        io.mem.resp.bits := word
        when(io.mem.resp.fire) { reqActive := false.B }
      }.otherwise {
        // Miss: read a full line
        io.arb.req.valid := true.B
        io.arb.req.bits.addr := Cat(reqAddr(cfg.addrBits-1, 6), 0.U(6.W))
        io.arb.req.bits.wen := false.B
        io.arb.req.bits.isLine := true.B
        when(io.arb.req.fire) { io.arb.resp.ready := true.B }
        when(io.arb.resp.fire) {
          val line = io.arb.resp.bits
          for (i <- 0 until beatsPerLine) {
            val lo = i*cfg.dataBits
            val hi = lo + cfg.dataBits - 1
            dataArray(idx)(i) := line(hi, lo)
          }
          tagArray(idx) := tag
          validArray(idx) := true.B
        }
        val haveLine = WireDefault(false.B)
        haveLine := io.arb.resp.fire
        when(haveLine) {
          val b = beatOf(reqAddr)
          val sel = wordSel(reqAddr)
          val beat = dataArray(idx)(b)
          val word = Mux(sel.asBool, beat(63,32), beat(31,0))
          io.mem.resp.valid := true.B
          io.mem.resp.bits := word
          when(io.mem.resp.fire) { reqActive := false.B }
        }
      }
    }.otherwise {
      // STORE path (write-through, no-allocate)
      // Forward 1-beat write to AXI
      io.arb.req.valid := true.B
      io.arb.req.bits.addr := Cat(reqAddr(cfg.addrBits-1, 3), 0.U(3.W))
      io.arb.req.bits.wen := true.B
      io.arb.req.bits.isLine := false.B
      io.arb.req.bits.wdata := wdata64.pad(512)
      io.arb.req.bits.wstrb := strobeMask
      when(io.arb.req.fire) { io.arb.resp.ready := true.B }
      when(io.arb.resp.fire) {
        // Update cache line on hit only
        when(hit) {
          val b = beatOf(reqAddr)
          val oldBeat = dataArray(idx)(b)
          // Merge bytes according to strobe
          val newBeat = Wire(UInt(64.W))
          val bytesVec = Wire(Vec(8, UInt(8.W)))
          for (i <- 0 until 8) {
            val sel = strobeMask(i)
            val wbyte = wdata64(8*i+7, 8*i)
            val obyte = oldBeat(8*i+7, 8*i)
            bytesVec(i) := Mux(sel, wbyte, obyte)
          }
          newBeat := Cat(bytesVec.reverse)
          dataArray(idx)(b) := newBeat
        }
        // Store completes immediately to core; it does not wait on resp
        reqActive := false.B
      }
    }
  }
}

class AxiArbiter(nPorts: Int, addrBits: Int = 32, idBits: Int = 4, dataBits: Int = 64) extends Module {
  require(nPorts > 0)
  require(dataBits == 64, "Arbiter is currently tuned for 64-bit AXI datapath")

  val io = IO(new Bundle {
    val in  = Vec(nPorts, Flipped(new ArbiterPort()))
    val axi = new AXIMasterIF(addrBits, dataBits, idBits)
  })

  // Defaults
  io.axi.writeAddr.valid := false.B
  io.axi.writeAddr.bits.addr  := 0.U
  io.axi.writeAddr.bits.size  := (log2Ceil(dataBits/8)).U
  io.axi.writeAddr.bits.len   := 7.U // 8 beats
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
  io.axi.readAddr.bits.len   := 7.U // 8 beats
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
  val beatCnt  = RegInit(0.U(3.W)) // 0..7
  val wstrbReg = RegInit(0.U(8.W))

  val wlineVec = Reg(Vec(8, UInt(dataBits.W)))
  val rlineVec = Reg(Vec(8, UInt(dataBits.W)))

  val sIdle :: sWriteAddr :: sWriteBeats :: sWriteResp :: sWriteDone :: sReadAddr :: sReadBeats :: sReadResp :: Nil = Enum(8)
  val state = RegInit(sIdle)

  // Helper: pick first-valid input by fixed priority (0 highest)
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
          // Address: for line ops, align to 64B; for beat ops, 8B alignment
          addrReg  := Mux(io.in(i).req.bits.isLine,
                          Cat(io.in(i).req.bits.addr(addrBits-1, 6), 0.U(6.W)),
                          Cat(io.in(i).req.bits.addr(addrBits-1, 3), 0.U(3.W)))
          // Capture write data
          val line = io.in(i).req.bits.wdata
          for (b <- 0 until 8) {
            val lo = b * dataBits
            val hi = lo + dataBits - 1
            wlineVec(b) := line(hi, lo)
          }
          // Beat strobe for beat write
          wstrbReg := io.in(i).req.bits.wstrb
          beatCnt := 0.U
          state := Mux(io.in(i).req.bits.wen, sWriteAddr, sReadAddr)
        }
      }
    }

    // Write path
    is(sWriteAddr) {
      io.axi.writeAddr.valid := true.B
      io.axi.writeAddr.bits.addr := addrReg
      io.axi.writeAddr.bits.id   := curPort
      io.axi.writeAddr.bits.len  := Mux(isLineOp, 7.U, 0.U)
      when(io.axi.writeAddr.fire) {
        state := sWriteBeats
      }
    }
    is(sWriteBeats) {
      io.axi.writeData.valid := true.B
      io.axi.writeData.bits.data := wlineVec(beatCnt)
      io.axi.writeData.bits.strb := Mux(isLineOp, Fill(dataBits/8, 1.U(1.W)), wstrbReg)
      io.axi.writeData.bits.last := Mux(isLineOp, (beatCnt === 7.U), true.B)
      when(io.axi.writeData.fire) {
        when(Mux(isLineOp, beatCnt === 7.U, true.B)) { state := sWriteResp }
        beatCnt := beatCnt + 1.U
      }
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
      when(io.axi.readAddr.fire) {
        beatCnt := 0.U
        state := sReadBeats
      }
    }
    is(sReadBeats) {
      io.axi.readData.ready := true.B
      when(io.axi.readData.fire) {
        rlineVec(beatCnt) := io.axi.readData.bits.data
        when(beatCnt === 7.U && io.axi.readData.bits.last) { state := sReadResp }
        beatCnt := beatCnt + 1.U
      }
    }
    is(sReadResp) {
      // Concatenate beats into a 512b line, LSB = first beat
      io.in(curPort).resp.valid := true.B
      io.in(curPort).resp.bits := Cat(rlineVec.reverse)
      when(io.in(curPort).resp.fire) { state := sIdle }
    }
  }
}
