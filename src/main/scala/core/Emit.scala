package core

import chisel3._
import circt.stage.ChiselStage

object EmitMigTop extends App {
  ChiselStage.emitSystemVerilogFile(
    new MigTop(initFile=Some("prog.hex")),
    args = Array("--target-dir", "synthesized")
  )
}
