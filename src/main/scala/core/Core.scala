package core

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import chisel3.util.experimental.loadMemoryFromFileInline
import chisel3.experimental.hierarchy.{instantiable, public}
import core.Control.{BR_BEQ => BR_BEQ}
import core.Control.{BR_BNE => BR_BNE}
import core.Control.{BR_BLT => BR_BLT}

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
    val waddr = Input(UInt(xlen.W))
    val st_type = Input(UInt(2.W))
    val wdata = Input(UInt(32.W))
  })

  val mem = Mem(depthWords, Vec(4, UInt(8.W)))
  hexPath.foreach(p => loadMemoryFromFileInline(mem, p))

  import Control._

  val line  = mem(io.raddr >> 2)
  val rByte = Mux1H(UIntToOH(io.raddr(1,0), 4), line)
  val rHalf = Mux(io.raddr(1), Cat(line(3), line(2)), Cat(line(1), line(0)))
  val rWord = Cat(line.reverse)

  io.rdata := MuxLookup(io.ld_type, 0.U)(Seq(
    LD_LB  -> rByte.asSInt.pad(32).asUInt,
    LD_LBU -> rByte.pad(32),
    LD_LH  -> rHalf.asSInt.pad(32).asUInt,
    LD_LHU -> rHalf.pad(32),
    LD_LW  -> rWord
  ))

  val mask = MuxLookup(io.st_type, 0.U(4.W))(Seq(
    ST_SB -> UIntToOH(io.waddr(1, 0), 4),
    ST_SH -> Mux(io.waddr(1), "b1100".U, "b0011".U),
    ST_SW -> "b1111".U
  ))

  val wOff = io.waddr(1, 0)

  val baseLane = MuxLookup(io.st_type, 0.U(2.W))(Seq(
    ST_SB -> wOff,
    ST_SH -> Cat(wOff(1), 0.U(1.W)),
    ST_SW -> 0.U
  ))

  val shamt = (baseLane << 3)
  val aligned = (io.wdata << shamt)(31,0)
  val wVec = VecInit.tabulate(4)(i => aligned(8*i+7, 8*i))

  mem.write(io.waddr >> 2, wVec, mask.asBools)
}

class RegFile(xlen: Int) extends Module {
  val io = IO(new Bundle {
    val raddr1 = Input(UInt(5.W))
    val raddr2 = Input(UInt(5.W))
    val rdata1 = Output(UInt(xlen.W))
    val rdata2 = Output(UInt(xlen.W))
    val wen    = Input(Bool())
    val waddr  = Input(UInt(5.W))
    val wdata  = Input(UInt(xlen.W))
  })

  val regs = RegInit(VecInit(Seq.fill(32)(0.U(xlen.W))))

  io.rdata1 := Mux(io.raddr1.orR, regs(io.raddr1), 0.U)
  io.rdata2 := Mux(io.raddr2.orR, regs(io.raddr2), 0.U)

  when (io.wen && io.waddr.orR) {
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

  // A_sel
  val A_XXX = 0.U(1.W)
  val A_PC = 0.U(1.W)
  val A_RS1 = 1.U(1.W)

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

  // br_cond
  val BR_XXX = 0.U(3.W)
  val BR_BEQ = 1.U(3.W)
  val BR_BNE = 2.U(3.W)
  val BR_BLT = 3.U(3.W)
  val BR_BGE = 4.U(3.W)
  val BR_BLTU = 5.U(3.W)
  val BR_BGEU = 6.U(3.W)
  val BR_JUMP = 7.U(3.W)

  // wb_sel
  val WB_XXX = 0.U(2.W)
  val WB_ALU = 0.U(2.W)
  val WB_MEM = 1.U(2.W)
  val WB_PC4 = 2.U(2.W)

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

  // B_sel  imm_sel  alu_op  st_type ld_type wb_sel wb_en br_cond

  val default = List(A_XXX, B_XXX, IMM_X, ALU_XXX, ST_XXX, LD_XXX, WB_ALU, N, BR_XXX) 

  val map = Array(
    // Loads
    LB -> List(A_RS1, B_IMM, IMM_I, ALU_ADD, ST_XXX, LD_LB, WB_MEM, Y, BR_XXX),
    LH -> List(A_RS1, B_IMM, IMM_I, ALU_ADD, ST_XXX, LD_LH, WB_MEM, Y, BR_XXX),
    LW -> List(A_RS1, B_IMM, IMM_I, ALU_ADD, ST_XXX, LD_LW, WB_MEM, Y, BR_XXX),
    LBU -> List(A_RS1, B_IMM, IMM_I, ALU_ADD, ST_XXX, LD_LBU, WB_MEM, Y, BR_XXX),
    LHU -> List(A_RS1, B_IMM, IMM_I, ALU_ADD, ST_XXX, LD_LHU, WB_MEM, Y, BR_XXX),
    // Stores
    SB -> List(A_RS1, B_IMM, IMM_S, ALU_ADD, ST_SB, LD_XXX, WB_XXX, N, BR_XXX),
    SH -> List(A_RS1, B_IMM, IMM_S, ALU_ADD, ST_SH, LD_XXX, WB_XXX, N, BR_XXX),
    SW -> List(A_RS1, B_IMM, IMM_S, ALU_ADD, ST_SW, LD_XXX, WB_XXX, N, BR_XXX),
    // Shifts
    SLL -> List(A_RS1, B_RS2, IMM_X, ALU_SLL, ST_XXX, LD_XXX, WB_ALU, Y, BR_XXX),
    SLLI -> List(A_RS1, B_IMM, IMM_I, ALU_SLL, ST_XXX, LD_XXX, WB_ALU, Y, BR_XXX),
    SRL -> List(A_RS1, B_RS2, IMM_X, ALU_SRL, ST_XXX, LD_XXX, WB_ALU, Y, BR_XXX),
    SRLI -> List(A_RS1, B_IMM, IMM_I, ALU_SRL, ST_XXX, LD_XXX, WB_ALU, Y, BR_XXX),
    SRA -> List(A_RS1, B_RS2, IMM_X, ALU_SRA, ST_XXX, LD_XXX, WB_ALU, Y, BR_XXX),
    SRAI -> List(A_RS1, B_IMM, IMM_I, ALU_SRA, ST_XXX, LD_XXX, WB_ALU, Y, BR_XXX),
    // Arithmetic
    ADD -> List(A_RS1, B_RS2, IMM_X, ALU_ADD, ST_XXX, LD_XXX, WB_ALU, Y, BR_XXX),
    ADDI -> List(A_RS1, B_IMM, IMM_I, ALU_ADD, ST_XXX, LD_XXX, WB_ALU, Y, BR_XXX),
    SUB -> List(A_RS1, B_RS2, IMM_X, ALU_SUB, ST_XXX, LD_XXX, WB_ALU, Y, BR_XXX),
    LUI -> List(A_RS1, B_IMM, IMM_U, ALU_COPY_B, ST_XXX, LD_XXX, WB_ALU, Y, BR_XXX),
    AUIPC -> List(A_PC, B_IMM, IMM_U, ALU_ADD, ST_XXX, LD_XXX, WB_ALU, Y, BR_XXX),
    // Logical
    XOR -> List(A_RS1, B_RS2, IMM_X, ALU_XOR, ST_XXX, LD_XXX, WB_ALU, Y, BR_XXX),
    XORI -> List(A_RS1, B_IMM, IMM_I, ALU_XOR, ST_XXX, LD_XXX, WB_ALU, Y, BR_XXX),
    OR -> List(A_RS1, B_RS2, IMM_X, ALU_OR, ST_XXX, LD_XXX, WB_ALU, Y, BR_XXX),
    ORI -> List(A_RS1, B_IMM, IMM_I, ALU_OR, ST_XXX, LD_XXX, WB_ALU, Y, BR_XXX),
    AND -> List(A_RS1, B_RS2, IMM_X, ALU_AND, ST_XXX, LD_XXX, WB_ALU, Y, BR_XXX),
    ANDI -> List(A_RS1, B_IMM, IMM_I, ALU_AND, ST_XXX, LD_XXX, WB_ALU, Y, BR_XXX),
    // Branches
    BEQ -> List(A_PC, B_IMM, IMM_B, ALU_ADD, ST_XXX, LD_XXX, WB_XXX, N, BR_BEQ),
    BNE -> List(A_PC, B_IMM, IMM_B, ALU_ADD, ST_XXX, LD_XXX, WB_XXX, N, BR_BNE),
    BLT -> List(A_PC, B_IMM, IMM_B, ALU_ADD, ST_XXX, LD_XXX, WB_XXX, N, BR_BLT),
    BGE -> List(A_PC, B_IMM, IMM_B, ALU_ADD, ST_XXX, LD_XXX, WB_XXX, N, BR_BGE),
    BLTU -> List(A_PC, B_IMM, IMM_B, ALU_ADD, ST_XXX, LD_XXX, WB_XXX, N, BR_BLTU),
    BGEU -> List(A_PC, B_IMM, IMM_B, ALU_ADD, ST_XXX, LD_XXX, WB_XXX, N, BR_BGEU),
    // Jump & Link
    JAL -> List(A_PC, B_IMM, IMM_J, ALU_ADD, ST_XXX, LD_XXX, WB_PC4, Y, BR_JUMP),
    JALR -> List(A_RS1, B_IMM, IMM_I, ALU_ADD, ST_XXX, LD_XXX, WB_PC4, Y, BR_JUMP)
  )
}

class Control() extends Module {
  val io = IO(new Bundle {
    val inst = Input(UInt(32.W))
    val A_sel = Output(UInt(1.W))
    val B_sel = Output(UInt(1.W))
    val imm_sel = Output(UInt(3.W))
    val alu_op = Output(UInt(4.W))
    val wb_sel = Output(UInt(2.W))
    val st_type = Output(UInt(2.W))
    val ld_type = Output(UInt(3.W))
    val wb_en = Output(UInt(1.W))
    val br_cond = Output(UInt(3.W))
  })

  val sig = ListLookup(io.inst, Control.default, Control.map)

  io.A_sel := sig(0)
  io.B_sel := sig(1)
  io.imm_sel := sig(2)
  io.alu_op := sig(3)
  io.st_type := sig(4)
  io.ld_type := sig(5)
  io.wb_sel := sig(6)
  io.wb_en := sig(7)
  io.br_cond := sig(8)
}

class BrCond(val xlen: Int) extends Module {
  val io = IO(new Bundle {
    val br_cond = Input(UInt(3.W))
    val A = Input(UInt(xlen.W))
    val B = Input(UInt(xlen.W))
    val enbl = Output(Bool())
  })

  import Control._

  io.enbl := MuxLookup(io.br_cond, false.B)(Seq(
    BR_BEQ -> (io.A === io.B),
    BR_BNE -> (io.A =/= io.B),
    BR_BLT -> (io.A.asSInt < io.B.asSInt),
    BR_BGE -> (io.A.asSInt >= io.B.asSInt),
    BR_BLTU -> (io.A < io.B),
    BR_BGEU -> (io.A >= io.B),
    BR_JUMP -> true.B,
  ))
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
  val io = IO(new Bundle {})

  val pc      = RegInit(0.U(xlen.W))
  val imem    = Module(new IMem(xlen, 65536, Some(hexPath)))
  val dmem    = Module(new DMem(xlen, 65536, dataHexPath))
  val control = Module(new Control())
  val alu     = Module(new Alu(xlen))
  val regs    = Module(new RegFile(xlen))
  val immgen  = Module(new ImmGen(xlen))
  val brcond  = Module(new BrCond(xlen))

  pc := Mux(brcond.io.enbl, Cat(alu.io.out(alu.io.out.getWidth-1, 1), 0.U(1.W)), pc + 4.U)
  imem.io.addr := pc

  val inst  = imem.io.data
  val rd    = inst(11, 7)
  val rs1   = inst(19, 15)
  val rs2   = inst(24, 20)

  brcond.io.br_cond := control.io.br_cond
  brcond.io.A := regs.io.rdata1
  brcond.io.B := regs.io.rdata2

  control.io.inst := inst

  immgen.io.inst := inst
  immgen.io.sel  := control.io.imm_sel

  regs.io.raddr1 := rs1
  regs.io.raddr2 := rs2
  regs.io.wen    := control.io.wb_en
  regs.io.waddr  := rd

  regs.io.wdata := MuxLookup(control.io.wb_sel, 0.U)(Seq(
    Control.WB_ALU -> alu.io.out,
    Control.WB_MEM -> dmem.io.rdata,
    Control.WB_PC4 -> (pc + 4.U),
  ))

  alu.io.A := Mux(control.io.A_sel === Control.A_RS1, regs.io.rdata1, pc)
  alu.io.B := Mux(control.io.B_sel === Control.B_RS2, regs.io.rdata2, immgen.io.out)
  alu.io.alu_op := control.io.alu_op

  dmem.io.raddr   := alu.io.out
  dmem.io.ld_type := control.io.ld_type
  dmem.io.st_type := control.io.st_type
  dmem.io.waddr := alu.io.out
  dmem.io.wdata := regs.io.rdata2

  val opcode = inst(6, 0)
  val func3  = inst(14, 12)
  dontTouch(opcode); dontTouch(func3)
  dontTouch(rd); dontTouch(rs1); dontTouch(rs2)
}

class CoreWithTaps(xlen: Int, hexPath: String, dataHexPath: Option[String])
    extends Core(xlen, hexPath, dataHexPath) {

  // Make it a real port so the simulator maps it
  val regsTap = IO(Output(Vec(32, UInt(xlen.W))))

  // If regs.regs is inside a submodule, keep using BoringUtils to pull it out:
  regsTap := BoringUtils.tapAndRead[Vec[UInt]](regs.regs)
}