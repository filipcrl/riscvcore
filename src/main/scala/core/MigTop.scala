package core

import chisel3._
import chisel3.util._
import chisel3.experimental._

// MIG 7-series memory controller black box
class Mig7Series extends BlackBox {
  override def desiredName: String = "mig_7series_0"
  val io = IO(new Bundle {
    // DDR3 SDRAM interface
    val ddr3_dq     = Analog(64.W)
    val ddr3_dqs_n  = Analog(8.W)
    val ddr3_dqs_p  = Analog(8.W)
    val ddr3_addr   = Output(UInt(14.W))
    val ddr3_ba     = Output(UInt(3.W))
    val ddr3_ras_n  = Output(Bool())
    val ddr3_cas_n  = Output(Bool())
    val ddr3_we_n   = Output(Bool())
    val ddr3_reset_n= Output(Bool())
    val ddr3_ck_p   = Output(UInt(1.W))
    val ddr3_ck_n   = Output(UInt(1.W))
    val ddr3_cke    = Output(UInt(1.W))
    val ddr3_cs_n   = Output(UInt(1.W))
    val ddr3_dm     = Output(UInt(8.W))
    val ddr3_odt    = Output(UInt(1.W))

    // Clocks
    val sys_clk_p   = Input(Clock())
    val sys_clk_n   = Input(Clock())
    val clk_ref_p   = Input(Clock())
    val clk_ref_n   = Input(Clock())

    // User interface clocks
    val ui_clk           = Output(Clock())
    val ui_clk_sync_rst  = Output(Bool())
    val mmcm_locked      = Output(Bool())

    // Control
    val aresetn      = Input(Bool())
    val app_sr_req   = Input(Bool())
    val app_ref_req  = Input(Bool())
    val app_zq_req   = Input(Bool())
    val app_sr_active= Output(Bool())
    val app_ref_ack  = Output(Bool())
    val app_zq_ack   = Output(Bool())

    // AXI slave interface (to MIG)
    // Write Address
    val s_axi_awid    = Input(UInt(4.W))
    val s_axi_awaddr  = Input(UInt(30.W))
    val s_axi_awlen   = Input(UInt(8.W))
    val s_axi_awsize  = Input(UInt(3.W))
    val s_axi_awburst = Input(UInt(2.W))
    val s_axi_awlock  = Input(UInt(1.W))
    val s_axi_awcache = Input(UInt(4.W))
    val s_axi_awprot  = Input(UInt(3.W))
    val s_axi_awqos   = Input(UInt(4.W))
    val s_axi_awvalid = Input(Bool())
    val s_axi_awready = Output(Bool())
    // Write Data
    val s_axi_wdata   = Input(UInt(512.W))
    val s_axi_wstrb   = Input(UInt(64.W))
    val s_axi_wlast   = Input(Bool())
    val s_axi_wvalid  = Input(Bool())
    val s_axi_wready  = Output(Bool())
    // Write Response
    val s_axi_bready  = Input(Bool())
    val s_axi_bid     = Output(UInt(4.W))
    val s_axi_bresp   = Output(UInt(2.W))
    val s_axi_bvalid  = Output(Bool())
    // Read Address
    val s_axi_arid    = Input(UInt(4.W))
    val s_axi_araddr  = Input(UInt(30.W))
    val s_axi_arlen   = Input(UInt(8.W))
    val s_axi_arsize  = Input(UInt(3.W))
    val s_axi_arburst = Input(UInt(2.W))
    val s_axi_arlock  = Input(UInt(1.W))
    val s_axi_arcache = Input(UInt(4.W))
    val s_axi_arprot  = Input(UInt(3.W))
    val s_axi_arqos   = Input(UInt(4.W))
    val s_axi_arvalid = Input(Bool())
    val s_axi_arready = Output(Bool())
    // Read Data
    val s_axi_rready  = Input(Bool())
    val s_axi_rid     = Output(UInt(4.W))
    val s_axi_rdata   = Output(UInt(512.W))
    val s_axi_rresp   = Output(UInt(2.W))
    val s_axi_rlast   = Output(Bool())
    val s_axi_rvalid  = Output(Bool())

    val init_calib_complete = Output(Bool())
    val device_temp_i       = Input(UInt(12.W))
    val device_temp         = Output(UInt(12.W))
    val sys_rst             = Input(Bool())
  })
}

class MigTop(initFile: Option[String] = None) extends RawModule {
  // MIG ports
  val ddr3_dq     = IO(Analog(64.W))
  val ddr3_dqs_n  = IO(Analog(8.W))
  val ddr3_dqs_p  = IO(Analog(8.W))
  val ddr3_addr   = IO(Output(UInt(14.W)))
  val ddr3_ba     = IO(Output(UInt(3.W)))
  val ddr3_ras_n  = IO(Output(Bool()))
  val ddr3_cas_n  = IO(Output(Bool()))
  val ddr3_we_n   = IO(Output(Bool()))
  val ddr3_reset_n= IO(Output(Bool()))
  val ddr3_ck_p   = IO(Output(UInt(1.W)))
  val ddr3_ck_n   = IO(Output(UInt(1.W)))
  val ddr3_cke    = IO(Output(UInt(1.W)))
  val ddr3_cs_n   = IO(Output(UInt(1.W)))
  val ddr3_dm     = IO(Output(UInt(8.W)))
  val ddr3_odt    = IO(Output(UInt(1.W)))

  val sys_clk_p   = IO(Input(Clock()))
  val sys_clk_n   = IO(Input(Clock()))
  val clk_ref_p   = IO(Input(Clock()))
  val clk_ref_n   = IO(Input(Clock()))

  val sys_rst     = IO(Input(Bool()))

  // MIG instance
  val mig = Module(new Mig7Series)

  ddr3_dq     <> mig.io.ddr3_dq
  ddr3_dqs_n  <> mig.io.ddr3_dqs_n
  ddr3_dqs_p  <> mig.io.ddr3_dqs_p
  ddr3_addr   := mig.io.ddr3_addr
  ddr3_ba     := mig.io.ddr3_ba
  ddr3_ras_n  := mig.io.ddr3_ras_n
  ddr3_cas_n  := mig.io.ddr3_cas_n
  ddr3_we_n   := mig.io.ddr3_we_n
  ddr3_reset_n:= mig.io.ddr3_reset_n
  ddr3_ck_p   := mig.io.ddr3_ck_p
  ddr3_ck_n   := mig.io.ddr3_ck_n
  ddr3_cke    := mig.io.ddr3_cke
  ddr3_cs_n   := mig.io.ddr3_cs_n
  ddr3_dm     := mig.io.ddr3_dm
  ddr3_odt    := mig.io.ddr3_odt

  mig.io.sys_clk_p := sys_clk_p
  mig.io.sys_clk_n := sys_clk_n
  mig.io.clk_ref_p := clk_ref_p
  mig.io.clk_ref_n := clk_ref_n

  mig.io.aresetn := sys_rst
  mig.io.app_sr_req := false.B
  mig.io.app_ref_req:= false.B
  mig.io.app_zq_req := false.B

  mig.io.device_temp_i := 0.U
  mig.io.sys_rst := sys_rst

  val top = withClockAndReset(mig.io.ui_clk, mig.io.ui_clk_sync_rst) { Module(new Top(32, sets=64, initFile = initFile)) }

  mig.io.s_axi_awid    := top.io.axi.writeAddr.bits.id
  mig.io.s_axi_awaddr  := top.io.axi.writeAddr.bits.addr(29,0)
  mig.io.s_axi_awlen   := top.io.axi.writeAddr.bits.len
  mig.io.s_axi_awsize  := top.io.axi.writeAddr.bits.size
  mig.io.s_axi_awburst := top.io.axi.writeAddr.bits.burst
  mig.io.s_axi_awlock  := top.io.axi.writeAddr.bits.lock
  mig.io.s_axi_awcache := top.io.axi.writeAddr.bits.cache
  mig.io.s_axi_awprot  := top.io.axi.writeAddr.bits.prot
  mig.io.s_axi_awqos   := top.io.axi.writeAddr.bits.qos
  mig.io.s_axi_awvalid := top.io.axi.writeAddr.valid
  top.io.axi.writeAddr.ready := mig.io.s_axi_awready

  mig.io.s_axi_wdata   := top.io.axi.writeData.bits.data
  mig.io.s_axi_wstrb   := top.io.axi.writeData.bits.strb
  mig.io.s_axi_wlast   := top.io.axi.writeData.bits.last
  mig.io.s_axi_wvalid  := top.io.axi.writeData.valid
  top.io.axi.writeData.ready := mig.io.s_axi_wready

  mig.io.s_axi_bready  := top.io.axi.writeResp.ready
  top.io.axi.writeResp.bits.id   := mig.io.s_axi_bid
  top.io.axi.writeResp.bits.resp := mig.io.s_axi_bresp
  top.io.axi.writeResp.valid     := mig.io.s_axi_bvalid

  mig.io.s_axi_arid    := top.io.axi.readAddr.bits.id
  mig.io.s_axi_araddr  := top.io.axi.readAddr.bits.addr(29,0)
  mig.io.s_axi_arlen   := top.io.axi.readAddr.bits.len
  mig.io.s_axi_arsize  := top.io.axi.readAddr.bits.size
  mig.io.s_axi_arburst := top.io.axi.readAddr.bits.burst
  mig.io.s_axi_arlock  := top.io.axi.readAddr.bits.lock
  mig.io.s_axi_arcache := top.io.axi.readAddr.bits.cache
  mig.io.s_axi_arprot  := top.io.axi.readAddr.bits.prot
  mig.io.s_axi_arqos   := top.io.axi.readAddr.bits.qos
  mig.io.s_axi_arvalid := top.io.axi.readAddr.valid
  top.io.axi.readAddr.ready := mig.io.s_axi_arready

  mig.io.s_axi_rready := top.io.axi.readData.ready
  top.io.axi.readData.bits.id   := mig.io.s_axi_rid
  top.io.axi.readData.bits.data := mig.io.s_axi_rdata
  top.io.axi.readData.bits.resp := mig.io.s_axi_rresp
  top.io.axi.readData.bits.last := mig.io.s_axi_rlast
  top.io.axi.readData.valid     := mig.io.s_axi_rvalid
}
