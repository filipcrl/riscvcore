package core

import chisel3._
import chisel3.stage.ChiselStage

object EmitMigTop extends App {
  (new ChiselStage).emitVerilog(new MigTop, args)
}

object EmitTopWithMem extends App {
  (new ChiselStage).emitVerilog(new Top(32), args)
}

