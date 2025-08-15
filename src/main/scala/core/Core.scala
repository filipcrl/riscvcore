package core

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFileInline

class IMem(xlen: Int, depthWords: Int = 65536, hexPath: Option[String] = None) extends Module {
  val io = IO(new Bundle {
    val addr = Input(UInt(xlen.W))
    val data = Output(UInt(32.W))
  })

  val mem = Mem(depthWords, UInt(32.W))
  io.data := mem(io.addr >> 2)

  hexPath.foreach(p => loadMemoryFromFileInline(mem, p))
}

class DMem(xlen: Int, depthWords: Int = 65536, hexPath: Option[String] = None) extends Module {
  val io = IO(new Bundle {
    val raddr = Input(UInt(xlen.W))
    val ld_type = Input(UInt(3.W))
    val rdata = Output(UInt(32.W))
    // val wen = Input(Bool())
    // val waddr = Input(UInt(xlen.W))
    // val wdata = Input(UInt(xlen.W))
  })

  val mem = Mem(depthWords, UInt(32.W))
  hexPath.foreach(p => loadMemoryFromFileInline(mem, p))

  import Control._

  val word = mem(io.raddr >> 2)
  val offset = io.raddr(1, 0)
  val byte = (word >> (offset << 3))(7, 0)

  when(io.ld_type === LD_LB) {
    io.rdata := byte.asSInt.pad(32).asUInt
  }.otherwise {
    io.rdata := 0.U(32.W)
  }
}

class RegFile(xlen: Int) extends Module {
  val io = IO(new Bundle {
    val raddr1 = Input(UInt(5.W))
    val raddr2 = Input(UInt(5.W))
    val rdata1 = Output(UInt(xlen.W))
    val rdata2 = Output(UInt(xlen.W))
    val wen = Input(Bool())
    val waddr = Input(UInt(5.W))
    val wdata = Input(UInt(xlen.W))
  })

  val regs = Mem(32, UInt(xlen.W)) 

  io.rdata1 := Mux(io.raddr1.orR, regs(io.raddr1), 0.U)
  io.rdata2 := Mux(io.raddr2.orR, regs(io.raddr2), 0.U)

  when(io.wen & io.waddr.orR) {
    regs(io.waddr) := io.wdata
  }
}

object Alu {
  val ALU_ADD = 0.U(4.W)
  val ALU_SUB = 1.U(4.W)
  val ALU_AND = 2.U(4.W)
  val ALU_OR = 3.U(4.W)
  val ALU_XOR = 4.U(4.W)
  val ALU_SLT = 5.U(4.W)
  val ALU_SLL = 6.U(4.W)
  val ALU_SLTU = 7.U(4.W)
  val ALU_SRL = 8.U(4.W)
  val ALU_SRA = 9.U(4.W)
  val ALU_COPY_A = 10.U(4.W)
  val ALU_COPY_B = 11.U(4.W)
  val ALU_XXX = 15.U(4.W)
}

class Alu(xlen: Int) extends Module {
  import Alu._

  val io = IO(new Bundle {
    val A = Input(UInt(xlen.W))
    val B = Input(UInt(xlen.W))
    val alu_op = Input(UInt(4.W))
    val out = Output(UInt(xlen.W))
  })

  val shamt = io.B(4, 0).asUInt

  io.out := MuxLookup(io.alu_op, io.B)(
    Seq(
      ALU_ADD -> (io.A + io.B),
      ALU_SUB -> (io.A - io.B),
      ALU_SRA -> (io.A.asSInt >> shamt).asUInt,
      ALU_SRL -> (io.A >> shamt),
      ALU_SLL -> (io.A << shamt),
      ALU_SLT -> (io.A.asSInt < io.B.asSInt),
      ALU_SLTU -> (io.A < io.B),
      ALU_AND -> (io.A & io.B),
      ALU_OR -> (io.A | io.B),
      ALU_XOR -> (io.A ^ io.B),
      ALU_COPY_A -> io.A
    )
  )
}

object Control {
  val Y = true.B
  val N = false.B

  // B_sel
  val B_XXX = 0.U(1.W)
  val B_IMM = 0.U(1.W)
  val B_RS2 = 1.U(1.W)

  // imm_sel
  val IMM_X = 0.U(3.W)
  val IMM_I = 1.U(3.W)
  val IMM_S = 2.U(3.W)
  val IMM_U = 3.U(3.W)
  val IMM_J = 4.U(3.W)
  val IMM_B = 5.U(3.W)
  val IMM_Z = 6.U(3.W)

  // wb_sel
  val WB_ALU = 0.U(1.W)
  val WB_MEM = 1.U(1.W)

  // st_type
  val ST_XXX = 0.U(2.W)
  val ST_SW = 1.U(2.W)
  val ST_SH = 2.U(2.W)
  val ST_SB = 3.U(2.W)

  // ld_type
  val LD_XXX = 0.U(3.W)
  val LD_LW = 1.U(3.W)
  val LD_LH = 2.U(3.W)
  val LD_LB = 3.U(3.W)
  val LD_LHU = 4.U(3.W)
  val LD_LBU = 5.U(3.W)

  import Alu._
  import Instructions._

  // B_sel  imm_sel  alu_op  st_type ld_type wb_sel wb_en

  val default = List(B_XXX, IMM_X, ALU_XXX, ST_XXX, LD_XXX, WB_ALU, N)

  val map = Array(
    // Loads
    LB -> List(B_IMM, IMM_I, ALU_ADD, ST_XXX, LD_LB, WB_MEM, Y)
  )
}

class Control() extends Module {
  val io = IO(new Bundle {
    val inst = Input(UInt(32.W))
    val B_sel = Output(UInt(1.W))
    val imm_sel = Output(UInt(3.W))
    val alu_op = Output(UInt(4.W))
    val wb_sel = Output(UInt(1.W))
    val st_type = Output(UInt(2.W))
    val ld_type = Output(UInt(3.W))
    val wb_en = Output(UInt(1.W))
  })

  val sig = ListLookup(io.inst, Control.default, Control.map)

  io.B_sel := sig(0)
  io.imm_sel := sig(1)
  io.alu_op := sig(2)
  io.st_type := sig(3)
  io.ld_type := sig(4)
  io.wb_sel := sig(5)
  io.wb_en := sig(6)
}

class ImmGen(val xlen: Int) extends Module {
  val io = IO(new Bundle {
    val inst = Input(UInt(xlen.W))
    val sel = Input(UInt(3.W))
    val out = Output(UInt(xlen.W))
  })

  val Iimm = io.inst(31, 20).asSInt
  val Simm = Cat(io.inst(31, 25), io.inst(11, 7)).asSInt
  val Bimm = Cat(io.inst(31), io.inst(7), io.inst(30, 25), io.inst(11, 8), 0.U(1.W)).asSInt
  val Uimm = Cat(io.inst(31, 12), 0.U(12.W)).asSInt
  val Jimm = Cat(io.inst(31), io.inst(19, 12), io.inst(20), io.inst(30, 25), io.inst(24, 21), 0.U(1.W)).asSInt
  val Zimm = io.inst(19, 15).zext

  import Control._

  io.out := MuxLookup(io.sel, 0.S)(
    Seq(IMM_I -> Iimm, IMM_S -> Simm, IMM_B -> Bimm, IMM_U -> Uimm, IMM_J -> Jimm, IMM_Z -> Zimm)
  ).asUInt
}

class Core(xlen: Int, hexPath: String, dataHexPath: Option[String]) extends Module {
  val io = IO(new Bundle {
    // Make logic observable to prevent DCE
    val pc_out     = Output(UInt(xlen.W))
    val inst_out   = Output(UInt(32.W))

    // Decoder/execute signals (handy in waves)
    val B_sel      = Output(UInt(1.W))
    val imm_sel    = Output(UInt(3.W))
    val alu_op     = Output(UInt(4.W))
    val wb_sel     = Output(UInt(1.W))
    val st_type    = Output(UInt(2.W))
    val ld_type    = Output(UInt(3.W))
    val wb_en      = Output(UInt(1.W))

    // Writeback bus
    val wb_wen     = Output(Bool())
    val wb_waddr   = Output(UInt(5.W))
    val wb_wdata   = Output(UInt(xlen.W))

    // ALU / imm
    val alu_A      = Output(UInt(xlen.W))
    val alu_B      = Output(UInt(xlen.W))
    val alu_out    = Output(UInt(xlen.W))
    val imm_out    = Output(UInt(xlen.W))

    // Memory interfaces for visibility
    val imem_addr  = Output(UInt(xlen.W))
    val imem_data  = Output(UInt(32.W))
    val dmem_raddr = Output(UInt(xlen.W))
    val dmem_rdata = Output(UInt(32.W))
  })

  val pc      = RegInit(0.U(xlen.W))
  val imem    = Module(new IMem(xlen, 65536, Some(hexPath)))
  val dmem    = Module(new DMem(xlen, 65536, dataHexPath))
  val control = Module(new Control())
  val alu     = Module(new Alu(xlen))
  val regs    = Module(new RegFile(xlen))
  val immgen  = Module(new ImmGen(xlen))

  // Simple fetch: advance PC each cycle
  pc := pc + 4.U
  imem.io.addr := pc

  val inst  = imem.io.data
  val rd    = inst(11, 7)
  val rs1   = inst(19, 15)
  val rs2   = inst(24, 20)

  // Drive decoder
  control.io.inst := inst

  // Immediate gen
  immgen.io.inst := inst
  immgen.io.sel  := control.io.imm_sel

  // Register file
  regs.io.raddr1 := rs1
  regs.io.raddr2 := rs2
  regs.io.wen    := control.io.wb_en
  regs.io.waddr  := rd
  regs.io.wdata  := Mux(control.io.wb_sel === Control.WB_ALU, alu.io.out, dmem.io.rdata)

  // ALU
  alu.io.A      := regs.io.rdata1
  alu.io.B      := Mux(control.io.B_sel === Control.B_IMM, immgen.io.out, regs.io.rdata2)
  alu.io.alu_op := control.io.alu_op

  // Data memory
  dmem.io.raddr   := alu.io.out
  dmem.io.ld_type := control.io.ld_type

  // ---- Expose signals to anchor the design and for wave debug ----
  io.pc_out     := pc
  io.inst_out   := inst

  io.B_sel      := control.io.B_sel
  io.imm_sel    := control.io.imm_sel
  io.alu_op     := control.io.alu_op
  io.wb_sel     := control.io.wb_sel
  io.st_type    := control.io.st_type
  io.ld_type    := control.io.ld_type
  io.wb_en      := control.io.wb_en

  io.wb_wen     := regs.io.wen
  io.wb_waddr   := regs.io.waddr
  io.wb_wdata   := regs.io.wdata

  io.alu_A      := alu.io.A
  io.alu_B      := alu.io.B
  io.alu_out    := alu.io.out
  io.imm_out    := immgen.io.out

  io.imem_addr  := imem.io.addr
  io.imem_data  := imem.io.data
  io.dmem_raddr := dmem.io.raddr
  io.dmem_rdata := dmem.io.rdata

  // Keep a few intermediates from being swept even if unused later
  val opcode = inst(6, 0)
  val func3  = inst(14, 12)
  dontTouch(opcode); dontTouch(func3)
  dontTouch(rd); dontTouch(rs1); dontTouch(rs2)
}