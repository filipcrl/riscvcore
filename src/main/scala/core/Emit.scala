package core

import chisel3._
import circt.stage.ChiselStage

object EmitMigTop extends App {
  ChiselStage.emitSystemVerilogFile(
    new MigTop,
    args = Array("--target-dir", "synthesized")
  )
}

object EmitTopWithMem extends App {
  ChiselStage.emitSystemVerilogFile(
    new Top(32),
    args = Array("--target-dir", "synthesized")
  )
}
