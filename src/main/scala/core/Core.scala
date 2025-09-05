package core

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import chisel3.util.experimental.loadMemoryFromFileInline
import chisel3.experimental.hierarchy.{instantiable, public}
import core.Control.{BR_BEQ => BR_BEQ}
import core.Control.{BR_BNE => BR_BNE}
import core.Control.{BR_BLT => BR_BLT}
import core.Control.ldSize
import core.Control.loadExtend
import core.Control.stMask

object Instructions {
  // Loads
  def LB = BitPat("b?????????????????000?????0000011")
  def LH = BitPat("b?????????????????001?????0000011")
  def LW = BitPat("b?????????????????010?????0000011")
  def LBU = BitPat("b?????????????????100?????0000011")
  def LHU = BitPat("b?????????????????101?????0000011")
  // Stores
  def SB = BitPat("b?????????????????000?????0100011")
  def SH = BitPat("b?????????????????001?????0100011")
  def SW = BitPat("b?????????????????010?????0100011")
  // Shifts
  def SLL = BitPat("b0000000??????????001?????0110011")
  def SLLI = BitPat("b0000000??????????001?????0010011")
  def SRL = BitPat("b0000000??????????101?????0110011")
  def SRLI = BitPat("b0000000??????????101?????0010011")
  def SRA = BitPat("b0100000??????????101?????0110011")
  def SRAI = BitPat("b0100000??????????101?????0010011")
  // Arithmetic
  def ADD = BitPat("b0000000??????????000?????0110011")
  def ADDI = BitPat("b?????????????????000?????0010011")
  def SUB = BitPat("b0100000??????????000?????0110011")
  def LUI = BitPat("b?????????????????????????0110111")
  def AUIPC = BitPat("b?????????????????????????0010111")
  // Logical
  def XOR = BitPat("b0000000??????????100?????0110011")
  def XORI = BitPat("b?????????????????100?????0010011")
  def OR = BitPat("b0000000??????????110?????0110011")
  def ORI = BitPat("b?????????????????110?????0010011")
  def AND = BitPat("b0000000??????????111?????0110011")
  def ANDI = BitPat("b?????????????????111?????0010011")
  // Compare
  def SLT = BitPat("b0000000??????????010?????0110011")
  def SLTI = BitPat("b?????????????????010?????0010011")
  def SLTU = BitPat("b0000000??????????011?????0110011")
  def SLTIU = BitPat("b?????????????????011?????0010011")
  // Branches
  def BEQ = BitPat("b?????????????????000?????1100011")
  def BNE = BitPat("b?????????????????001?????1100011")
  def BLT = BitPat("b?????????????????100?????1100011")
  def BGE = BitPat("b?????????????????101?????1100011")
  def BLTU = BitPat("b?????????????????110?????1100011")
  def BGEU = BitPat("b?????????????????111?????1100011")
  // Jump & Link
  def JAL = BitPat("b?????????????????????????1101111")
  def JALR = BitPat("b?????????????????000?????1100111")
  // Other
  def WFI = BitPat("b00010000001000000000000001110011")
  def NOP = BitPat.bitPatToUInt(BitPat("b00000000000000000000000000010011"))
}

// RegFile: 32x32 register file
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

// Alu: integer ALU
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

  def stMask(st: UInt, addr: UInt): UInt = MuxLookup(st, 0.U(4.W))(Seq(
    Control.ST_SB -> UIntToOH(addr(1,0), 4),
    Control.ST_SH -> Mux(addr(1), "b1100".U, "b0011".U),
    Control.ST_SW -> "b1111".U
  ))

  // ld_type
  val LD_XXX = 0.U(3.W)
  val LD_LW = 1.U(3.W)
  val LD_LH = 2.U(3.W)
  val LD_LB = 3.U(3.W)
  val LD_LHU = 4.U(3.W)
  val LD_LBU = 5.U(3.W)

  def ldSize(ld: UInt): UInt = MuxLookup(ld, 2.U)(Seq(
    Control.LD_LB  -> 0.U, Control.LD_LBU -> 0.U,
    Control.LD_LH  -> 1.U, Control.LD_LHU -> 1.U,
    Control.LD_LW  -> 2.U
  ))

  def loadExtend(ld: UInt, addr: UInt, rdata: UInt): UInt = {
    val byte = ((rdata >> (addr(1,0) << 3)).asUInt)(7,0)
    val half = Mux(addr(1), rdata(31,16), rdata(15,0))
    MuxLookup(ld, rdata)(Seq(
      Control.LD_LB  -> byte.asSInt.pad(32).asUInt,
      Control.LD_LBU -> byte.zext.asUInt,
      Control.LD_LH  -> half.asSInt.pad(32).asUInt,
      Control.LD_LHU -> half.zext.asUInt,
      Control.LD_LW  -> rdata
    ))
  }

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

// Control: instruction decode
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

// BrCond: branch comparator
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

// ImmGen: immediate generator
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

// Core: RV32I core
class Core(xlen: Int, hexPath: String, dataHexPath: Option[String]) extends Module {
  val io = IO(new Bundle {
    val imem = new MemPort(xlen)
    val dmem = new MemPort(xlen)
  })

  val pc = RegInit(0.U(xlen.W))
  val inst = RegInit(0.U(32.W))

  val control = Module(new Control())
  val alu = Module(new Alu(xlen))
  val regs = Module(new RegFile(xlen))
  val immgen = Module(new ImmGen(xlen))
  val brcond = Module(new BrCond(xlen))

  val rd  = inst(11, 7)
  val rs1 = inst(19, 15)
  val rs2 = inst(24, 20)

  immgen.io.inst := inst
  control.io.inst := inst

  immgen.io.sel := control.io.imm_sel

  regs.io.raddr1 := rs1
  regs.io.raddr2 := rs2

  alu.io.A := Mux(control.io.A_sel === Control.A_RS1, regs.io.rdata1, pc)
  alu.io.B := Mux(control.io.B_sel === Control.B_RS2, regs.io.rdata2, immgen.io.out)
  alu.io.alu_op := control.io.alu_op

  brcond.io.br_cond := control.io.br_cond
  brcond.io.A := regs.io.rdata1
  brcond.io.B := regs.io.rdata2

  regs.io.wen := false.B
  regs.io.waddr := rd
  regs.io.wdata := 0.U

  io.imem.req.valid := false.B
  io.imem.req.bits.addr  := pc
  io.imem.req.bits.wdata := 0.U
  io.imem.req.bits.wstrb := 0.U
  io.imem.req.bits.size  := 2.U
  io.imem.resp.ready := false.B

  io.dmem.req.valid := false.B
  io.dmem.req.bits.addr  := alu.io.out
  io.dmem.req.bits.wdata := regs.io.rdata2
  io.dmem.req.bits.wstrb := 0.U
  io.dmem.req.bits.size  := 2.U
  io.dmem.resp.ready := false.B

  val sFetch :: sWaitI :: sExec :: sIssueLoad :: sWaitLoad :: sIssueStore :: Nil = Enum(6)
  val state = RegInit(sFetch)

  val nextPc = Mux(brcond.io.enbl, Cat(alu.io.out(alu.io.out.getWidth-1, 1), 0.U(1.W)), pc + 4.U)

  switch (state) {
    is(sFetch) {
      io.imem.req.valid := true.B
      when(io.imem.req.fire) { state := sWaitI }
    }
    is (sWaitI) {
      io.imem.resp.ready := true.B
      when (io.imem.resp.fire) {
        inst := io.imem.resp.bits
        state := sExec
      }
    }
    is (sExec) {
      when(control.io.ld_type =/= Control.LD_XXX) {
        state := sIssueLoad
      }.elsewhen(control.io.st_type =/= Control.ST_XXX) {
        state := sIssueStore
      }.otherwise {
        when (control.io.wb_en.asBool) {
          regs.io.wen := true.B
          regs.io.wdata := MuxLookup(control.io.wb_sel, 0.U)(Seq(
            Control.WB_ALU -> alu.io.out,
            Control.WB_PC4 -> (pc + 4.U)
          ))
        }
        state := sFetch
        pc := nextPc
      }
    }
    is (sIssueLoad) {
      io.dmem.req.valid := true.B
      io.dmem.req.bits.size := ldSize(control.io.ld_type)
      when (io.dmem.req.fire) { state := sWaitLoad }
    }
    is (sWaitLoad) {
      io.dmem.resp.ready := true.B
      when (io.dmem.resp.fire) {
        when(control.io.wb_en.asBool) {
          regs.io.wen := true.B
          regs.io.wdata := loadExtend(control.io.ld_type, alu.io.out, io.dmem.resp.bits)
        }
        pc := nextPc
        state := sFetch
      }
    }
    is(sIssueStore) {
      io.dmem.req.valid := true.B
      io.dmem.req.bits.wstrb := stMask(control.io.st_type, alu.io.out)
      io.dmem.req.bits.size := MuxLookup(control.io.st_type, 2.U)(Seq(
        Control.ST_SB -> 0.U, Control.ST_SH -> 1.U, Control.ST_SW -> 2.U
      ))
      when (io.dmem.req.fire) {
        pc := nextPc
        state := sFetch
      }
    }
  }
}

// CoreWithTaps: core with simple instruction/data memories
class CoreWithTaps(xlen: Int, hexPath: String, dataHexPath: Option[String]) extends Module {
  val core = Module(new Core(xlen, hexPath, dataHexPath))

  val regsTap = IO(Output(Vec(32, UInt(xlen.W))))
  regsTap := BoringUtils.tapAndRead[Vec[UInt]](core.regs.regs)

  val iMemDepth = 65536
  val iMem = Mem(iMemDepth, UInt(32.W))
  loadMemoryFromFileInline(iMem, hexPath)

  val iBusy = RegInit(false.B)
  val iAddrReg = RegInit(0.U(xlen.W))
  core.io.imem.req.ready := !iBusy
  when (core.io.imem.req.fire) {
    iAddrReg := core.io.imem.req.bits.addr
    iBusy := true.B
  }
  val iData = iMem(iAddrReg >> 2)
  core.io.imem.resp.valid := iBusy
  core.io.imem.resp.bits  := iData
  when (core.io.imem.resp.fire) { iBusy := false.B }

  val dMemDepth = 65536
  val dMem = Mem(dMemDepth, Vec(4, UInt(8.W)))
  dataHexPath.foreach(p => loadMemoryFromFileInline(dMem, p))

  val dBusy     = RegInit(false.B)
  val dAddrReg  = RegInit(0.U(xlen.W))
  val dWdataReg = RegInit(0.U(xlen.W))
  val dWstrbReg = RegInit(0.U((xlen/8).W))
  val dSizeReg  = RegInit(0.U(2.W))

  core.io.dmem.req.ready := !dBusy
  when (core.io.dmem.req.fire) {
    dAddrReg  := core.io.dmem.req.bits.addr
    dWdataReg := core.io.dmem.req.bits.wdata
    dWstrbReg := core.io.dmem.req.bits.wstrb
    dSizeReg  := core.io.dmem.req.bits.size
    dBusy     := core.io.dmem.req.bits.wstrb === 0.U

    when (core.io.dmem.req.bits.wstrb.orR) {
      val addr = core.io.dmem.req.bits.addr
      val wdata = core.io.dmem.req.bits.wdata
      val size  = core.io.dmem.req.bits.size
      val baseLane = MuxLookup(size, 0.U(2.W))(Seq(
        0.U -> addr(1,0),
        1.U -> Cat(addr(1), 0.U(1.W)),
        2.U -> 0.U
      ))
      val shamt   = (baseLane << 3)
      val aligned = (wdata << shamt)(31,0)
      val wVec    = VecInit.tabulate(4)(i => aligned(8*i+7, 8*i))
      dMem.write(addr >> 2, wVec, core.io.dmem.req.bits.wstrb.asBools)
    }
  }

  val dLine = dMem(dAddrReg >> 2)
  val dWord = Cat(dLine.reverse)
  core.io.dmem.resp.valid := dBusy
  core.io.dmem.resp.bits  := dWord
  when (core.io.dmem.resp.fire) { dBusy := false.B }
}
